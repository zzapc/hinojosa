FUNCTION z_lanzar_job_ap_new_task.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     VALUE(JOBNAME) TYPE  TBTCJOB-JOBNAME
*"     VALUE(REPORT) TYPE  SY-CPROG
*"     VALUE(VARIANT) TYPE  SY-SLSET OPTIONAL
*"     VALUE(RETRASO) TYPE  INT4 DEFAULT 10
*"     VALUE(USER) TYPE  SY-UNAME DEFAULT SY-UNAME
*"     VALUE(GRABAR_LOG) TYPE  ZPROCESO DEFAULT ''
*"     VALUE(ORIGEN) TYPE  KEDRIDENT DEFAULT ''
*"     VALUE(MAIL_SI_ERROR) TYPE  TEXT255 DEFAULT ''
*"     VALUE(PARAMETROS) TYPE  RSPARAMS_TT OPTIONAL
*"     VALUE(ESPERA_SI_JOB_ACTIVO) TYPE  INT4 DEFAULT 10
*"     VALUE(CLAVE) TYPE  OBJKEY DEFAULT ''
*"     VALUE(EXIT) TYPE  EXIT_BIBL DEFAULT ''
*"     VALUE(NT) TYPE  XFELD DEFAULT 'X'
*"     VALUE(TEST) TYPE  XFELD DEFAULT ''
*"  EXPORTING
*"     VALUE(MESSAGE) TYPE  BAPI_MSG
*"----------------------------------------------------------------------

  SELECT SINGLE obj_name FROM tadir
    INTO @DATA(l_obj_name)
   WHERE pgmid = 'R3TR'
     AND object = 'PROG'
     AND obj_name = @report.
  IF sy-subrc NE 0.
    message = |No existe el report { report }|.
  ELSE.
    IF NOT variant IS INITIAL.
      IF zcl_ap_variante=>existe( report = report
                                  variant = variant ) = ''.
        message = |No existe la variante { variant } del report { report }|.
      ENDIF.
    ENDIF.

    IF message IS INITIAL.

      IF espera_si_job_activo > 0.
        DO espera_si_job_activo TIMES.
          IF zcl_ap_jobs=>esta_job_pendiente( jobname = jobname ) = 'X'.
            EXIT.
          ENDIF.
        ENDDO.
      ENDIF.

      IF test = 'X'.
        SUBMIT (report)
         USING SELECTION-SET variant
          WITH SELECTION-TABLE parametros
           AND RETURN
           VIA SELECTION-SCREEN.
        RETURN.
      ENDIF.
      DATA(o_job) = NEW zcl_ap_jobs( jobname = jobname ).

      o_job->abrir( ).

      IF NOT o_job->jobcount IS INITIAL.
        TRY.
            CLEAR o_job->print_parameters-primm.

            SUBMIT (report)
            USING SELECTION-SET variant
                  WITH SELECTION-TABLE parametros
               AND RETURN
               VIA JOB o_job->jobname NUMBER o_job->jobcount
               TO SAP-SPOOL SPOOL PARAMETERS o_job->print_parameters WITHOUT SPOOL DYNPRO
                USER user.

            IF retraso = 0.
              DATA(l_inmediato) = 'X'.
            ENDIF.

            o_job->cerrar( inmediato = l_inmediato retraso = retraso ).
            IF NOT o_job->message IS INITIAL.
              message = o_job->message.
            ENDIF.
          CATCH cx_root INTO DATA(o_root).
            message = o_root->get_text( ).
        ENDTRY.
      ELSE.
        message = o_job->message.
      ENDIF.
    ENDIF.
  ENDIF.

  DATA: l_error  TYPE string,
        l_msg_ok TYPE string.
  IF NOT clave IS INITIAL.
    DATA(l_msg_c) = |clave = { clave }|.
  ENDIF.
  IF NOT origen IS INITIAL.
    DATA(l_msg_o) = |origen = { origen }|.
  ENDIF.

  l_msg_ok = |Se ha lanzado el job { jobname } report { report } { l_msg_c } { l_msg_o }|.

  IF NOT message IS INITIAL.
    l_error = |Error { message } lanzando job { jobname } report { report } { l_msg_c } { l_msg_o }|.
  ENDIF.

  IF NOT exit IS INITIAL.
    IF message IS INITIAL.
      zcl_ap_exits=>log_st( exit = exit clave = clave p1 = l_msg_ok msgty = 'E' ).
    ELSE.
      zcl_ap_exits=>log_st( exit = exit clave = clave p1 = l_error msgty = 'E' ).
    ENDIF.
  ENDIF.

  IF NOT grabar_log IS INITIAL.
    IF message IS INITIAL.
      zcl_ap_log=>set_log( proceso = grabar_log clave = clave p1 = |Se ha lanzado el job { jobname } report { report } origen { origen }| msgty = 'S' ).
    ELSE.
      zcl_ap_log=>set_log( proceso = grabar_log clave = clave p1 = l_error msgty = 'E' ).
    ENDIF.
  ENDIF.

  IF NOT mail_si_error IS INITIAL AND NOT message IS INITIAL.
    zcl_ap_envio_mail=>mail( direccion = mail_si_error
                             subject = l_error
                             clave = clave  ).
  ENDIF.



ENDFUNCTION.


**EJEMPLO DE LLAMADA
*data PARAMETROS TYPE  RSPARAMS_TT.
*parametros = value #( ( SELNAME = 'S_MATNR'
*                        KIND    = 'S'
*                        SIGN    = 'I'
*                        OPTION  = 'EQ'
*                        LOW     = 'COTPNI00000054' ) ).
*CALL FUNCTION 'Z_LANZAR_JOB_AP'
*  STARTING NEW TASK 'UPDATE'
*  DESTINATION 'NONE'
*  EXPORTING
*    JOBNAME    = 'JOB_PRUEBA'
*    report     = 'ZPRUEBA'
*    retraso    = 10
*    parametros = parametros.
