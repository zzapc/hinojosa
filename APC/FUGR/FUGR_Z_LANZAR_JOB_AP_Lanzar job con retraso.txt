FUNCTION z_lanzar_job_ap.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     VALUE(JOBNAME) TYPE  TBTCJOB-JOBNAME
*"     VALUE(REPORT) TYPE  SY-CPROG
*"     VALUE(VARIANT) TYPE  SY-SLSET OPTIONAL
*"     VALUE(RETRASO) TYPE  INT4 DEFAULT 10
*"     VALUE(USER) TYPE  SY-UNAME DEFAULT SY-UNAME
*"     VALUE(GRABAR_LOG) TYPE  ZPROCESO DEFAULT ''
*"     VALUE(ORIGEN) TYPE  ANY DEFAULT ''
*"     VALUE(MAIL_SI_ERROR) TYPE  ANY DEFAULT ''
*"     VALUE(PARAMETROS) TYPE  RSPARAMS_TT OPTIONAL
*"     VALUE(ESPERA_SI_JOB_ACTIVO) TYPE  INT4 DEFAULT 10
*"     VALUE(CLAVE) TYPE  ANY DEFAULT ''
*"     VALUE(EXIT) TYPE  ANY DEFAULT ''
*"     VALUE(NEW_TASK) TYPE  XFELD DEFAULT ''
*"     VALUE(TEST) TYPE  XFELD DEFAULT ''
*"  EXPORTING
*"     VALUE(MESSAGE) TYPE  BAPI_MSG
*"----------------------------------------------------------------------
  DATA: l_clave         TYPE objkey,
        l_exit          TYPE exit_bibl,
        l_mail_si_error TYPE text255,
        l_origen        TYPE kedrident.

  l_clave = clave.
  l_exit  = exit.
  l_origen = origen.
  l_mail_si_error = mail_si_error.

  DATA(l_new_task) = new_task.
  IF l_new_task = 'X'.
    IF zcl_ap_usuario=>get_total_modos_activos( ) > 5.
      CLEAR l_new_task.
    ENDIF.
  ENDIF.

  IF l_new_task IS INITIAL.
    CALL FUNCTION 'Z_LANZAR_JOB_AP_NEW_TASK'
      EXPORTING
        jobname              = jobname
        report               = report
        variant              = variant
        retraso              = retraso
        user                 = user
        grabar_log           = grabar_log
        origen               = l_origen
        mail_si_error        = l_mail_si_error
        parametros           = parametros
        espera_si_job_activo = espera_si_job_activo
        clave                = l_clave
        exit                 = l_exit
        test                 = test
      IMPORTING
        message              = message.
  ELSE.
    CALL FUNCTION 'Z_LANZAR_JOB_AP_NEW_TASK'
      STARTING NEW TASK 'UPDATE'
      DESTINATION 'NONE'
      EXPORTING
        jobname              = jobname
        report               = report
        variant              = variant
        retraso              = retraso
        user                 = user
        grabar_log           = grabar_log
        origen               = l_origen
        mail_si_error        = l_mail_si_error
        parametros           = parametros
        espera_si_job_activo = espera_si_job_activo
        clave                = l_clave
        exit                 = l_exit
        test                 = test.
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
