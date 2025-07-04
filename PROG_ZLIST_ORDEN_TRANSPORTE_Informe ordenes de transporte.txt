***********************************************************************
* APLICACION : BC
* TIPO : LISTADO
* TITULO : Informe ordenes de transporte
* *
* DESCRIPCION : Informe ordenes de transporte
*
* AUTOR: Andrés Picazo                                FECHA: 12/01/2012
* ANALISTA: ??<<<<<<<<
*
* MODIFICACIONES
*
***********************************************************************
REPORT zlist_orden_transporte.

*------INCLUDES--------------------------------------------------------*

*------TABLAS/ESTRUCTURAS----------------------------------------------*
TABLES: tmstpalog, e070, e071, e071k, bkpf, ztemp, *ztemp, vbak, sscrfields.

TYPES: BEGIN OF t_ztasks_ot,
         ntask    TYPE vbeln_va,
         strkorr  TYPE e070-strkorr,
         as4text  TYPE tmstpalog-as4text,
         fecha    TYPE sy-datum,
         hora     TYPE sy-uzeit,
         pgmid    TYPE e071-pgmid,
         object   TYPE e071-object,
         obj_name TYPE e071-obj_name,
         tabkey   TYPE e071k-tabkey,
       END OF t_ztasks_ot.
DATA: i_ztasks_ot TYPE TABLE OF t_ztasks_ot,
      l_ztasks_ot TYPE t_ztasks_ot.

*------TABLAS INTERNAS-------------------------------------------------*
TYPES: BEGIN OF t_listado,
         check,
         lights,
         fecha      TYPE sy-datum,
         hora       TYPE sy-uzeit,
         ntask      TYPE vbeln_va,
         strkorr    TYPE e070-strkorr,
         as4text    TYPE tmstpalog-as4text,
         trkorr     TYPE tmstpalog-trkorr,
         trfunction TYPE tmstpalog-trfunction,
         truser     TYPE tmstpalog-truser,
         pgmid      TYPE e071-pgmid,
         object     TYPE e071-object,
         obj_name   TYPE e071-obj_name,
         tabkey     TYPE e071k-tabkey,
         cont       TYPE i,
       END OF t_listado.
DATA: i_listado TYPE TABLE OF t_listado,
      l_listado TYPE t_listado.

FIELD-SYMBOLS <listado> TYPE t_listado.

*------VARIABLES-------------------------------------------------------*
DATA: o_sgpi TYPE REF TO zcl_ap_sgpi.
DATA: i_log TYPE tmstpalogs.

*----------------------------------------------------------------------*
* CLASS lcl_alv DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_alv DEFINITION INHERITING FROM zcl_ap_alv_check.
  PUBLIC SECTION.
    METHODS: handle_double_click REDEFINITION.
    METHODS: handle_user_command REDEFINITION.
    METHODS: top_of_page REDEFINITION.
ENDCLASS. "lcl_alv DEFINITION

DATA: o_alv TYPE REF TO lcl_alv.
DATA: o_prog TYPE REF TO  zcl_ap_dev.

*------PARAMETER/SELECT-OPTIONS EN PANTALLA----------------------------*
SELECTION-SCREEN BEGIN OF BLOCK 001 WITH FRAME.
PARAMETERS: p_system TYPE tmssysnam DEFAULT 'PRO',
            p_domain TYPE tmsdomnam DEFAULT 'DOMAIN_DES'.
SELECT-OPTIONS: s_user FOR tmstpalog-truser,
                s_func FOR tmstpalog-trfunction,
                s_korr FOR tmstpalog-trkorr,
                s_text  FOR tmstpalog-as4text,
                s_ntask FOR bkpf-belnr.
SELECTION-SCREEN: SKIP 1.
SELECT-OPTIONS: s_pgmid  FOR e071-pgmid,
                s_object FOR e071-object,
                s_name   FOR e071-obj_name.

PARAMETERS: p_transp RADIOBUTTON GROUP g USER-COMMAND g.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(31) text-f01.
PARAMETERS: p_fini LIKE bkpf-cpudt,
            p_hini LIKE bkpf-cputm.
SELECTION-SCREEN COMMENT 54(3) text-f02.
PARAMETERS: p_ffin LIKE bkpf-cpudt,
            p_hfin LIKE bkpf-cputm DEFAULT '240000'.
SELECTION-SCREEN END OF LINE.
PARAMETERS: p_actual RADIOBUTTON GROUP g,
            p_pend   RADIOBUTTON GROUP g.
SELECTION-SCREEN: SKIP 1.
PARAMETERS: p_detal AS CHECKBOX USER-COMMAND det,
            p_dup   AS CHECKBOX MODIF ID det.
PARAMETERS p_export NO-DISPLAY.
PARAMETERS: p_vari LIKE disvariant-variant.
SELECTION-SCREEN END OF BLOCK 001.
SELECTION-SCREEN: FUNCTION KEY 1.
SELECTION-SCREEN: FUNCTION KEY 2.
SELECTION-SCREEN: FUNCTION KEY 3.




************************************************************************
*
* LOGICA DEL PROGRAMA
*
************************************************************************

*----------------------------------------------------------------------*
* CLASS lcl_alv IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_alv IMPLEMENTATION.
  METHOD handle_double_click.
    DATA l_return.
    READ TABLE i_listado INDEX row INTO l_listado.
    IF sy-subrc = 0.
      CASE column.
        WHEN 'NTASK'.
          *ztemp-valor1 = l_listado-ntask.
          zcl_ap_popup=>popup_usuario( EXPORTING campo1 = 'ZTEMP-VALOR1'
                                                 titulo = 'Informe nº tarea'
                                       IMPORTING return = l_return
                                       CHANGING  valor1 = *ztemp-valor1 ).
          vbak-vbeln = *ztemp-valor1.
          __poner_ceros vbak-vbeln.
          IF vbak-vbeln NE l_listado-ntask.
            CLEAR ztemp.
            ztemp-clave = 'TASKS_OT'.
            ztemp-subclave = l_listado-strkorr.
            l_listado-ntask = ztemp-valor1 = vbak-vbeln.
            MODIFY ztemp FROM ztemp.
            MODIFY i_listado FROM l_listado INDEX row.
            o_alv->refresh( ).
          ENDIF.

        WHEN OTHERS.
          CALL FUNCTION 'TMS_UI_SHOW_TRANSPORT_REQUEST'
            EXPORTING
              iv_request = l_listado-trkorr
*             iv_target_system = p_system
*             iv_verbose = ''
*             is_queue   = gs_disque-s_sys
*             it_requests      = lt_tr_requests
            EXCEPTIONS
              OTHERS     = 0.
      ENDCASE.
    ENDIF.
  ENDMETHOD. "handle_double_click
  METHOD handle_user_command.
    DATA: l_row       TYPE i,
          l_string    TYPE string,
          r_obj       TYPE RANGE OF syst-cprog,
          lr_obj      LIKE LINE OF r_obj,
          l_report,
          l_clases,
          l_tablas,
          l_funciones,
          l_hikey     TYPE string.

    CASE e_salv_function.
      WHEN 'EXCEL'.
        exportar_excel( ).
      WHEN 'F01'.
        get_seleccion( ).
        CLEAR i_ztasks_ot.
        LOOP AT i_listado ASSIGNING <listado> WHERE check = 'X'
                                                AND ntask NE ''.
          MOVE-CORRESPONDING <listado> TO l_ztasks_ot.
          APPEND l_ztasks_ot TO i_ztasks_ot.
        ENDLOOP.

        IF NOT i_ztasks_ot IS INITIAL.
          CALL TRANSFORMATION id_indent
               SOURCE obj   = i_ztasks_ot[]
               RESULT XML l_string.

          zcl_ap_ws=>grabar_xml( EXPORTING string = l_string dialogo = 'X' fichero = 'ZTASKS_OT.XML' codepage = zcl_c=>codepage  ).
        ENDIF.

      WHEN 'F02'.

        get_seleccion( ).
        CLEAR lr_obj.
        lr_obj-option = 'EQ'.
        lr_obj-sign = 'I'.
        LOOP AT i_listado ASSIGNING <listado> WHERE check = 'X'.
          lr_obj-low = <listado>-obj_name.
          COLLECT lr_obj INTO r_obj.

          CASE <listado>-object.
            WHEN 'CLAS'. l_clases = 'X'.
            WHEN 'PROG' OR 'REPS' OR 'DOMA' OR 'DTEL'. l_report = 'X'.
            WHEN 'TABL'. l_tablas = 'X'.
            WHEN 'FUGR'. l_funciones = 'X'.
          ENDCASE.
        ENDLOOP.

        SUBMIT zbcut011
          AND RETURN
          VIA SELECTION-SCREEN
         WITH s_report IN r_obj
         WITH p_report = l_report
         WITH p_clases = l_clases
         WITH p_tablas = l_tablas
         WITH p_funcio = l_funciones.


      WHEN 'F03'.
        get_seleccion( CHANGING t_tabla = i_listado ).
        LOOP AT i_listado ASSIGNING <listado> WHERE check = 'X'. "#EC NEEDED
          DELETE FROM  e071
                 WHERE  trkorr   = <listado>-trkorr
                 AND    pgmid    = <listado>-pgmid
                 AND    object   = <listado>-object
                 AND    obj_name  = <listado>-obj_name.

          DELETE FROM  e071k
                 WHERE  trkorr   = <listado>-trkorr
                 AND    pgmid    = <listado>-pgmid
                 AND    object   = <listado>-object
                 AND    objname  = <listado>-obj_name
                 AND    tabkey   = <listado>-tabkey.

          CONCATENATE <listado>-obj_name '%' INTO l_hikey.
          DELETE FROM  tlock
                 WHERE  trkorr   = <listado>-trkorr
                 AND    object   = <listado>-object
                 AND    hikey    LIKE l_hikey.

          DELETE i_listado.
        ENDLOOP.
        IF sy-subrc <> 0.
          MESSAGE i104(dlcn). "Seleccione por lo menos un registro
        ELSE.
          refresh( ).
        ENDIF.

      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD. "handle_USER_COMMAND

  METHOD top_of_page.
  ENDMETHOD.                    "handle_double_click

ENDCLASS. "lcl_alv IMPLEMENTATION

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.

  DATA l_week TYPE scal-week.

  l_week = zcl_ap_fechas=>get_semana( sy-datum ).
  p_fini = zcl_ap_fechas=>get_primer_dia_semana( l_week ).
  p_ffin = p_fini + 6.

  CREATE OBJECT o_prog
    EXPORTING
      status        = 'INICIO_DYN'
      get_nombre_pc = 'X'
      no_param      = 'X'
      status_prog   = 'ZAP_STATUS'.
*PERFORM add_button IN PROGRAM zap_status USING 'M01' 'Log' '' ''.

  CREATE OBJECT o_alv
    EXPORTING
      lights             = 'LIGHTS'
      status             = 'STANDARD_ALV_DYN'
      top_of_page_auto   = 'X'
      top_of_page_titulo = 'X'
      status_prog        = 'ZAP_STATUS'.
  p_vari = o_alv->get_default_layout( ).



  IF zcl_ap_documentos=>existe_doc( 'MANUAL' ) = 'X'.
    sscrfields-functxt_01 = '@J7@ Manual usuario'.
  ENDIF.
  IF zcl_ap_documentos=>existe_doc( 'PLANTILLA' ) = 'X'.
    sscrfields-functxt_02 = '@J2@ Ejemplo fichero carga'.
  ENDIF.

  p_system = zcl_c=>get_constante( 'SISTEMA_TRANSPORTE' ).
  p_domain = zcl_c=>get_constante( 'DOMINIO_TRANSPORTE' ).
  o_prog->initialization_i( CHANGING sscrfields = sscrfields ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.

  p_vari = o_alv->get_f4_layout( ).

************************************************************************
* AT SELECTION-SCREEN.
************************************************************************
AT SELECTION-SCREEN.


  CASE sy-ucomm.
    WHEN 'G'.
      IF p_pend = 'X'.
        CLEAR: p_fini.
        p_ffin = '99991231'.
      ENDIF.
    WHEN 'FC01'.
      zcl_ap_documentos=>visualizar_documento( nombre = 'MANUAL' ).
    WHEN 'FC02'.
      zcl_ap_documentos=>visualizar_documento( nombre = 'PLANTILLA' ).
    WHEN 'FC03'.
      zcl_ap_documentos=>popup_list( ).
    WHEN 'CDOCU'.
      SUBMIT zdocumentos
        AND RETURN
       WITH s_tcode = sy-cprog.
    WHEN 'TABLA'.
      zcl_ap_utils=>mantener_tabla( 'TABLA' ).
  ENDCASE.

AT SELECTION-SCREEN OUTPUT.
  zcl_ap_dynpro=>screen_visible( group1 = 'DET' variable = p_detal ).

*----------------------------------------------------------------------
* START-OF-SELECTION.
*----------------------------------------------------------------------*
START-OF-SELECTION.

  CREATE OBJECT o_sgpi
    EXPORTING
      commit_work = 'X'.


  IF p_transp = 'X'.
    PERFORM leer_datos.
  ELSEIF p_actual = 'X'.
    PERFORM leer_cola.
  ELSEIF p_pend = 'X'.
    PERFORM leer_ot.
  ENDIF.

  DATA: l_aux1(100), l_aux2(100).
  LOOP AT i_listado ASSIGNING <listado>.
    SELECT SINGLE as4text FROM  e07t
      INTO <listado>-as4text
    WHERE  trkorr  = <listado>-strkorr.

    IF <listado>-as4text CS '#T'.
      SPLIT <listado>-as4text AT '#T' INTO l_aux1 l_aux2.
      <listado>-ntask = l_aux2.
      zcl_ap_string=>poner_ceros_c( CHANGING cadena = <listado>-ntask ).
    ENDIF.

    IF <listado>-ntask IS INITIAL.
      IF <listado>-strkorr IS INITIAL.
        <listado>-strkorr = <listado>-trkorr.
      ENDIF.
      SELECT SINGLE valor1 FROM ztemp
        INTO ztemp-valor1
       WHERE clave = 'TASKS_OT'
         AND subclave = <listado>-strkorr.
      IF sy-subrc = 0.
        <listado>-ntask = ztemp-valor1.
        zcl_ap_string=>poner_ceros_c( CHANGING cadena = <listado>-ntask ).
      ENDIF.
    ENDIF.

    IF NOT <listado>-ntask IN s_ntask.
      DELETE i_listado.
    ENDIF.
  ENDLOOP.

  IF p_detal = 'X'.
    PERFORM detalle_objetos.
  ENDIF.

  IF p_export = 'X'.
    EXPORT i_list_ot FROM i_listado TO MEMORY ID 'ZLIST_ORDEN_TRANSPORTE'.
  ELSE.
    PERFORM listado.
  ENDIF.

************************************************************************
*
* FORMS ADICIONALES
*
************************************************************************
*&---------------------------------------------------------------------*
*& Form listado
*&---------------------------------------------------------------------*
FORM listado .

  o_sgpi->texto( 'Generando informe' ).
  o_alv->set_layout( p_vari ).
*  o_alv->set_top_of_page( ).

  o_alv->set_field_text( campo = 'NTASK' valor = 'Nº Tarea' ).
  o_alv->set_orden( 'FECHA,HORA,NTASK,STRKORR,TRKORR,TRFUNCTION,TRUSER,AS4TEXT' ).
  o_alv->set_field_noout( 'CONT' ).

  o_alv->add_button( button = 'F01' text = 'Exportar a ZTASK' icon = icon_export ).
  o_alv->add_button( button = 'F02' text = 'Detalle desarrollos' icon = icon_list ).
  IF p_detal IS INITIAL.
    o_alv->set_field_noout( 'PGMID,OBJECT,OBJ_NAME' ).
  ELSE.
    o_alv->add_button( button = 'F03' text = 'Borrar objetos de orden'  icon = icon_delete ).
  ENDIF.

  o_alv->show( ).

ENDFORM. " listado
*&---------------------------------------------------------------------*
*&      Form  LEER_DATOS
*&---------------------------------------------------------------------*
FORM leer_datos .

  PERFORM leer_historial_transporte USING p_fini p_hini p_ffin p_hfin.

  o_sgpi->texto( 'Preparando información' ).

  DELETE i_log WHERE as4text = 'ORDEN'
                  OR as4text = 'orden'.

  LOOP AT i_log INTO tmstpalog WHERE truser IN s_user
                                 AND trfunction IN s_func
                                 AND trkorr IN s_korr
                                 AND as4text IN s_text.
    CLEAR l_listado.
    MOVE-CORRESPONDING tmstpalog TO l_listado.
    l_listado-fecha = tmstpalog-trtime(8).
    l_listado-hora = tmstpalog-trtime+8.

    IF tmstpalog-retcode = '0000'.
      l_listado-lights = zcl_ap_alv=>c_sem_verde.
    ELSEIF tmstpalog-retcode <= '0004'.
      l_listado-lights = zcl_ap_alv=>c_sem_ambar.
    ELSE.
      l_listado-lights = zcl_ap_alv=>c_sem_rojo.
    ENDIF.

    APPEND l_listado TO i_listado.
  ENDLOOP.
  SORT i_listado.

ENDFORM.                    " LEER_DATOS
*&---------------------------------------------------------------------*
*&      Form  LEER_COLA
*&---------------------------------------------------------------------*
FORM leer_cola .
  DATA: i_request TYPE tmsiqreqs,
        l_request TYPE stmsiqreq.

  o_sgpi->texto( 'Leyendo cola' ).
  CALL FUNCTION 'TMS_UIQ_IQD_READ_QUEUE'
    EXPORTING
      iv_system         = p_system
      iv_domain         = p_domain
*     IV_COLLECT        =
      iv_read_shadow    = 'X'
*     IV_USE_DATA       =
      iv_max_exp        = '010'
      iv_monitor        = 'X'
*     IV_VERBOSE        =
    IMPORTING
*     EV_COLLECT_DATE   =
*     EV_COLLECT_TIME   =
*     EV_NR_OF_GRPS     =
*     EV_IS_A_QA_SYS    =
*     EV_NO_IMP_ALL     =
*     EV_IMP_SINGLE     =
*     EV_FTP_INCOMPLETE =
*     ES_SYSTEM         =
*     ES_BUFCNT         =
      et_requests       = i_request
*     ET_CLIENTS        =
*     ET_PROJECTS       =
* TABLES
*     TT_SYSTEM         =
    EXCEPTIONS
      read_queue_failed = 1
      OTHERS            = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  PERFORM leer_historial_transporte USING p_fini p_hini p_ffin p_hfin.

  o_sgpi->texto( 'Preparando información' ).
  LOOP AT i_request INTO l_request WHERE owner  IN s_user
                                     AND trfunc IN s_func
                                     AND trkorr IN s_korr
                                     AND text   IN s_text.

    CLEAR l_listado.
    MOVE-CORRESPONDING l_request TO l_listado.
    l_listado-truser = l_request-owner.
    l_listado-as4text = l_request-text.
    l_listado-trfunction = l_request-trfunc.

    IF l_request-maxrc = ''.
    ELSE.
      READ TABLE i_log INTO tmstpalog WITH KEY trkorr = l_listado-trkorr.
      IF sy-subrc = 0.
        l_listado-fecha = tmstpalog-trtime(8).
        l_listado-hora = tmstpalog-trtime+8.
      ENDIF.
      IF l_request-maxrc = '0000'.
        l_listado-lights = zcl_ap_alv=>c_sem_verde.
      ELSEIF l_request-maxrc <= '0004'.
        l_listado-lights = zcl_ap_alv=>c_sem_ambar.
      ELSE.
        l_listado-lights = zcl_ap_alv=>c_sem_rojo.
      ENDIF.
    ENDIF.

    APPEND l_listado TO i_listado.
  ENDLOOP.
  SORT i_listado.

ENDFORM.                    " LEER_COLA

*&---------------------------------------------------------------------*
*&      Form  leer_historial_transporte
*&---------------------------------------------------------------------*
FORM leer_historial_transporte USING p_fini p_hini p_ffin p_hfin.

  o_sgpi->texto( 'Leyendo historial transporte' ).
  CLEAR i_log.
  CALL FUNCTION 'TMS_TM_GET_HISTORY'
    EXPORTING
      iv_system     = p_system
      iv_domain     = p_domain
      iv_allcli     = 'X'
*     IV_TRCLI      =
*     IV_TRFUNCTION =
*     IV_PROJECT    =
      iv_imports    = 'X'
*     IV_EXPORTS    =
*     IV_ALL_STEPS  =
*     IV_ALL_ALOG_STEPS       =
*     IV_TPSTAT_KEY =
      iv_monitor    = 'X'
    IMPORTING
*     EV_ALOG_LINENR          =
      et_tmstpalog  = i_log
*     ES_EXCEPTION  =
    CHANGING
      cv_start_date = p_fini
      cv_start_time = p_hini
      cv_end_date   = p_ffin
      cv_end_time   = p_hfin
    EXCEPTIONS
      alert         = 1
      OTHERS        = 2.
ENDFORM.                    "leer_historial_transporte
*&---------------------------------------------------------------------*
*&      Form  DETALLE_OBJETOS
*&---------------------------------------------------------------------*
FORM detalle_objetos .
  DATA i_list TYPE TABLE OF t_listado.

  i_list = i_listado.
  CLEAR i_listado.

  LOOP AT i_list ASSIGNING <listado>.
    SELECT * FROM e071
     WHERE trkorr = <listado>-trkorr
       AND pgmid  IN s_pgmid
       AND object IN s_object
       AND obj_name IN s_name.
      MOVE-CORRESPONDING e071 TO <listado>.

      SELECT tabkey FROM  e071k
        INTO e071k-tabkey
             WHERE  trkorr   = e071-trkorr
             AND    pgmid    = e071-pgmid
             AND    object   = e071-object
             AND    objname  = e071-obj_name.
        <listado>-tabkey = e071k-tabkey.
        APPEND <listado> TO i_listado.
        CLEAR <listado>-tabkey.
      ENDSELECT.
      IF sy-subrc NE 0.
        APPEND <listado> TO i_listado.
      ENDIF.
    ENDSELECT.
  ENDLOOP.

  IF p_dup = 'X' AND p_detal = 'X'.
    DELETE i_listado WHERE object = 'VDAT' OR object = 'TABU' OR object = 'CDAT' OR object = 'TDAT'.
    DATA i_dup TYPE t_listado OCCURS 0 WITH HEADER LINE.
    LOOP AT i_listado ASSIGNING <listado>.
      CLEAR i_dup.
      i_dup-pgmid     = <listado>-pgmid.
      i_dup-object    = <listado>-object.
      i_dup-obj_name  = <listado>-obj_name.
      i_dup-tabkey    = <listado>-tabkey.
      i_dup-cont = 1.
      COLLECT i_dup.
    ENDLOOP.
    LOOP AT i_dup WHERE cont < 2.
      DELETE i_listado WHERE pgmid     = i_dup-pgmid
      AND object    = i_dup-object
      AND obj_name  = i_dup-obj_name.
    ENDLOOP.
  ENDIF.

ENDFORM.                    " DETALLE_OBJETOS
*&---------------------------------------------------------------------*
*&      Form  LEER_OT
*&---------------------------------------------------------------------*
FORM leer_ot .

  SELECT DISTINCT trkorr strkorr FROM e070
    INTO CORRESPONDING FIELDS OF TABLE i_listado
   WHERE ( trkorr IN  s_korr
        OR strkorr IN s_korr )
     AND trstatus IN ('D', 'L')
     AND as4date >= p_fini
     AND as4date <= p_ffin.


  LOOP AT i_listado ASSIGNING FIELD-SYMBOL(<listado>) WHERE strkorr IS INITIAL.
    SELECT SINGLE * FROM e071
   WHERE trkorr = <listado>-trkorr
     AND pgmid  IN s_pgmid
     AND object IN s_object
     AND obj_name IN s_name.
    IF sy-subrc NE 0.
      DELETE i_listado.
    ENDIF.
  ENDLOOP.


ENDFORM.                    " LEER_OT
