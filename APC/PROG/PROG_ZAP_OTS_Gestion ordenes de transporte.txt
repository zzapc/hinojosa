***********************************************************************
* TIPO : LISTADO
* TITULO : Gestion ordenes de transporte
* DESCRIPCION : Gestion ordenes de transporte
*
* AUTOR: Andres Picazo                                FECHA: 18/04/2017
*
***********************************************************************
REPORT zap_ots.
TYPE-POOLS: trwbo, sctsc.

*------TABLAS/ESTRUCTURAS----------------------------------------------*
TABLES: tmsbuffer, tmscdom.

*------TABLAS INTERNAS-------------------------------------------------*
*----------------------------------------------------------------------*
* CLASS lcl_alv DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_alv DEFINITION INHERITING FROM zcl_ap_alv_check.
  PUBLIC SECTION.
    METHODS handle_user_command REDEFINITION.
    METHODS visualizar_objeto   REDEFINITION.
ENDCLASS.


*----------------------------------------------------------------------*
*       CLASS zcl_report DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS zcl_report DEFINITION INHERITING FROM zcl_ap_dev.
  PUBLIC SECTION.
    TYPES: BEGIN OF t_listado,
             check          TYPE xfeld,
             lights         TYPE zico_estado_mensaje,
             request_type   TYPE snodetext-name,
             system         TYPE snodetext-text,
             modifiable     TYPE snodetext-name,
             request_number TYPE tmsbuffer-trkorr,
             as4date        TYPE as4date,
             as4time        TYPE as4time,
             fecha          TYPE dats,
             hora           TYPE uzeit,
             copy           TYPE icon_d,
             request_owner  TYPE snodetext-name,
             description    TYPE trwbo_request_header-as4text,
             retcode        TYPE trretcode,
             actflg         TYPE actflg,
             proyecto       TYPE ztemps-valor,
             notas          TYPE ztemps-string,
             message        TYPE bapi_msg,
             trstep         TYPE trtpstep,
             paso           TYPE string,
             color          TYPE lvc_t_scol,
           END OF t_listado,
           tt_listado TYPE TABLE OF t_listado.

    DATA: i_listado   TYPE tt_listado,
          l_listado   TYPE t_listado,
          i_liberadas TYPE tt_listado.

    METHODS  main.

    METHODS: listado,
             seleccionar_datos,
             liberar CHANGING !list TYPE t_listado,

      activar IMPORTING sistema TYPE any
              CHANGING  !list   TYPE t_listado,

      importar IMPORTING sistema   TYPE any
               EXPORTING cancelado TYPE abap_bool
               CHANGING  !list     TYPE t_listado.

    METHODS fill_selections
      RETURNING VALUE(rt_selections) TYPE trwbo_selections.

ENDCLASS.

*------VARIABLES-------------------------------------------------------*
DATA: o_prog TYPE REF TO zcl_report,
      o_alv  TYPE REF TO lcl_alv,
      ztemps TYPE ztemps.

*------PARAMETER/SELECT-OPTIONS EN PANTALLA----------------------------*
SELECTION-SCREEN BEGIN OF BLOCK 001 WITH FRAME TITLE TEXT-sel.
  PARAMETERS p_uname TYPE sy-uname DEFAULT sy-uname OBLIGATORY.
  SELECT-OPTIONS: s_trkorr FOR tmsbuffer-trkorr,
                  s_proyec FOR ztemps-valor.

  SELECTION-SCREEN SKIP 1.
  PARAMETERS: p_fin  AS CHECKBOX,
              p_excl AS CHECKBOX,
              p_dias TYPE i DEFAULT 30.
  SELECTION-SCREEN SKIP 1.
  PARAMETERS p_vari LIKE disvariant-variant.
SELECTION-SCREEN END OF BLOCK 001.
__botones_plantilla.


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
  METHOD visualizar_objeto.
    DATA mod TYPE abap_bool.

    FIELD-SYMBOLS <listado> TYPE o_prog->t_listado.

    ASSIGN o_prog->i_listado[ row ] TO <listado>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CASE column.
      WHEN 'COPY'.
        zcl_ap_string=>to_clipboard( string = CONV #( <listado>-request_number ) ).
      WHEN 'REQUEST_NUMBER'.
        CALL FUNCTION 'TR_DISPLAY_REQUEST'
          EXPORTING
            i_trkorr    = <listado>-request_number
            i_operation = 'EDIT'.

        <listado>-proyecto = o_prog->string.
        IF <listado>-proyecto IS INITIAL AND <listado>-notas IS INITIAL.
          zcl_ap_temp=>delete_string_st( clave = 'ZAP_OTS' subclave = <listado>-request_number ).
        ELSE.
          zcl_ap_temp=>set_string_st( clave = 'ZAP_OTS' subclave = <listado>-request_number valor = <listado>-proyecto string = <listado>-notas permanente = 'X' ).
        ENDIF.
        o_alv->refresh( ).

      WHEN 'NOTAS'.
        o_prog->string = <listado>-notas.
        zcl_ap_string=>popup_texto( EXPORTING titulo     = 'Notas'
                                              editar     = 'X'
                                    IMPORTING modificado = mod CHANGING
                                              texto      = o_prog->string ).
        IF mod = 'X'.
          <listado>-notas = o_prog->string.
          IF <listado>-proyecto IS INITIAL AND <listado>-notas IS INITIAL.
            zcl_ap_temp=>delete_string_st( clave = 'ZAP_OTS' subclave = <listado>-request_number ).
          ELSE.
            zcl_ap_temp=>set_string_st( clave = 'ZAP_OTS' subclave = <listado>-request_number valor = <listado>-proyecto string = <listado>-notas permanente = 'X' ).
          ENDIF.
          o_alv->refresh( ).
        ENDIF.

      WHEN OTHERS.
        SUBMIT zlist_orden_transporte
          AND RETURN
               WITH s_korr = <listado>-request_number
               WITH p_fini = '00000000'
               WITH p_ffin = '99991231'
               WITH p_transp = ''
               WITH p_pend = 'X'
               WITH p_detal = 'X'.
    ENDCASE.
  ENDMETHOD.

  METHOD handle_user_command.
    TYPES: BEGIN OF t_clp,
             request_number TYPE tmsbuffer-trkorr,
             description    TYPE trwbo_request_header-as4text,
           END OF t_clp.

    DATA i_clp TYPE TABLE OF t_clp.

    check_ucomm_sel = 'LIBERAR,LIBERAR_SV,IMP_CAL,IMP_PROD'.

    super->handle_user_command( e_salv_function ).

    CASE ucomm.
      WHEN 'RESEL'.
        LEAVE TO SCREEN 0.
      WHEN 'EXCEL'.
        exportar_excel( ).
      WHEN 'REFRESCAR'.
        o_prog->seleccionar_datos( ).
        refresh( ).
      WHEN 'CLP'.
        get_seleccion( CHANGING t_tabla = o_prog->i_listado ).
        LOOP AT o_prog->i_listado ASSIGNING FIELD-SYMBOL(<listado>) WHERE check = 'X'.
          APPEND VALUE #( request_number = <listado>-request_number description = <listado>-description ) TO i_clp.
        ENDLOOP.
        IF sy-subrc <> 0.
          MESSAGE 'Seleccione algun registro' TYPE 'I'.
        ELSE.
          zcl_ap_string=>to_clipboard( tabla = i_clp ).
          MESSAGE 'Se han dejado datos en el portapapeles' TYPE 'S'.
        ENDIF.
      WHEN 'LIBERAR' OR 'LIBERAR_SV' OR 'IMP_CAL' OR 'IMP_PRD'.
        get_seleccion( CHANGING t_tabla = o_prog->i_listado ).
        LOOP AT o_prog->i_listado ASSIGNING <listado> WHERE check = 'X'.
          CASE ucomm.
            WHEN 'LIBERAR' OR 'LIBERAR_SV'.
              IF ucomm = 'LIBERAR'.
                SET PARAMETER ID 'ZNO_VERIF' FIELD '' ##EXISTS.
              ELSE.
                SET PARAMETER ID 'ZNO_VERIF' FIELD 'X' ##EXISTS.
              ENDIF.
              o_prog->liberar( CHANGING list = <listado> ).
            WHEN 'IMP_CAL' OR 'IMP_PRD'.
              IF ucomm = 'IMP_PRD'.
                IF <listado>-retcode = '0008'.
                  IF zcl_ap_popup=>confirmar( texto = 'Se ha producido errores en calidad'
                                              texto2 = '¿Está seguro de querer subirla a producción' opcion = 'N' ) = ''.
                    RETURN.
                  ENDIF.
                ELSEIF <listado>-message <> 'En calidad'.
                  IF NOT zcl_c=>entorno_calidad IS INITIAL ##BOOL_OK.
                    IF zcl_ap_popup=>confirmar( texto = 'No ha verificado que la OT este en calidad sin errores'
                                                texto2 = '¿Esta seguro de querer subirla a producción' opcion = 'N' ) = ''.
                      RETURN.
                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDIF.
              o_prog->importar( EXPORTING sistema = ucomm+4
                                IMPORTING cancelado = DATA(l_cancelado)
                                CHANGING list = <listado> ).
          ENDCASE.
        ENDLOOP.
        IF sy-subrc <> 0.
          MESSAGE 'Seleccione algún registro' TYPE 'I'.
        ELSE.
          IF l_cancelado IS INITIAL.
            DATA(i_list) = o_prog->i_listado.
            IF ucomm(7) <> 'LIBERAR'.
              WAIT UP TO 5 SECONDS.
              cl_gui_cfw=>flush( ).
            ENDIF.
            DO 20 TIMES.
              CLEAR o_prog->i_listado.
              COMMIT WORK AND WAIT.
              WAIT UP TO 1 SECONDS.
              COMMIT WORK AND WAIT.
              o_prog->seleccionar_datos( ).
              DATA(l_salir) = ''.
              IF ucomm(7) = 'LIBERAR'.
                EXIT.
              ENDIF.
              LOOP AT i_list ASSIGNING FIELD-SYMBOL(<list>) WHERE check = 'X'.
                ASSIGN o_prog->i_listado[ request_number = <list>-request_number ] TO <listado>.
                IF sy-subrc <> 0.
                  l_salir = 'X'.
                ELSEIF <listado>-message <> <list>-message.
                  l_salir = 'X'.
                  IF <listado>-retcode = '0008'.
                    MESSAGE |Se han producido errores en OT { <listado>-request_number } { <listado>-description }| TYPE 'I'.
                  ELSE.
                    zcl_ap_string=>to_clipboard( string = CONV #( <list>-request_number ) ).
                  ENDIF.
                ENDIF.
              ENDLOOP.
              IF sy-subrc = 0 AND l_salir IS INITIAL.
                MESSAGE 'Esperando refresco de la cola' TYPE 'S'.
                COMMIT WORK AND WAIT.
                WAIT UP TO 1 SECONDS.
              ELSE.
                EXIT.
              ENDIF.
            ENDDO.
          ENDIF.
          IF ucomm = 'IMP_PRD'.
            set_seleccion( CHANGING t_tabla = o_prog->i_listado ).
          ENDIF.
          refresh( refresh_mode = if_salv_c_refresh=>full ).
        ENDIF.

      WHEN 'ACTIVAR'.
        get_seleccion( CHANGING t_tabla = o_prog->i_listado ).
        LOOP AT o_prog->i_listado ASSIGNING <listado> WHERE check = 'X'.
          o_prog->activar( EXPORTING sistema = 'CAL' CHANGING list = <listado> ).
        ENDLOOP.
        IF sy-subrc <> 0.
          MESSAGE 'Seleccione algún registro' TYPE 'I'.
        ELSE.
          o_prog->seleccionar_datos( ).
          refresh( ).
        ENDIF.

      WHEN 'CLAVED' OR 'CLAVEQ' OR 'CLAVEP'.
        CLEAR o_prog->string.
        zcl_ap_string=>from_clipboard( IMPORTING string = o_prog->string ).
        IF NOT o_prog->string IS INITIAL.
          zcl_ap_temp=>set_st( clave = 'ZAP_OTS' subclave = sy-uname && ucomm valor1 = o_prog->string ).
          MESSAGE |Nuevo { ucomm }  { o_prog->string }| TYPE 'S'.
        ENDIF.
      WHEN 'FIN'.
        p_fin = 'X'.
        o_prog->seleccionar_datos( ).
        refresh( ).

      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.


*----------------------------------------------------------------------*
*       CLASS zcl_report IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS zcl_report IMPLEMENTATION.
  METHOD main.
    seleccionar_datos( ).
    listado( ).
  ENDMETHOD.                    " REPORT

  METHOD seleccionar_datos.
    DATA lt_selections          TYPE trwbo_selections.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA ls_settings            TYPE ctsusrcust.
    DATA lv_username            LIKE sy-uname.
    DATA ls_selection           LIKE LINE OF lt_selections.
    DATA lt_request_headers_tmp TYPE trwbo_request_headers.
    DATA lt_request_headers     TYPE trwbo_request_headers.
    DATA: ayer   TYPE dats,
          system TYPE tmssysnam.
    DATA: i_log_calidad    TYPE tmstpalogs,
          i_log_produccion TYPE tmstpalogs.
    DATA: i_buffer_cal TYPE TABLE OF tmsbuffer,
          i_buffer_pro TYPE TABLE OF tmsbuffer.
    DATA ls_request_header TYPE trwbo_request_header.
    DATA: l_color TYPE string,
          l_ndias TYPE int4,
          l_int   TYPE int1.

    FIELD-SYMBOLS <log> TYPE tmstpalog.

    CLEAR i_listado.

*     Read user settings
    CALL FUNCTION 'TRINT_READ_USER_CUSTOMIZING'
      IMPORTING
        es_settings = ls_settings.

    sgpi_texto( 'Seleccionando datos' ).

    lt_selections = fill_selections( ).

    lv_username = p_uname. " trdyse01cm-username.

    LOOP AT lt_selections INTO ls_selection.

      CALL FUNCTION 'TRINT_SELECT_REQUESTS'
        EXPORTING
          iv_username_pattern  = lv_username
          is_selection         = ls_selection
          iv_complete_projects = 'X'
        IMPORTING
          et_requests          = lt_request_headers_tmp.

      APPEND LINES OF lt_request_headers_tmp TO lt_request_headers.

    ENDLOOP.

    ayer = sy-datum - p_dias.
    IF zcl_c=>entorno_calidad <> ''.
      SELECT domnam FROM tmscdom                    "#EC "#EC CI_BYPASS
        INTO CORRESPONDING FIELDS OF tmscdom
      UP TO 1 ROWS
      ORDER BY moddat DESCENDING.
      ENDSELECT.
      system = zcl_c=>entorno_calidad.
      CALL FUNCTION 'TMS_TM_GET_HISTORY'
        EXPORTING
          iv_system     = system
          iv_domain     = tmscdom-domnam
          iv_allcli     = 'X'
*         IV_TRCLI      =
*         IV_TRFUNCTION =
*         IV_PROJECT    =
          iv_imports    = 'X'
*         IV_EXPORTS    =
          iv_all_steps  = 'X'
*         IV_ALL_ALOG_STEPS       =
*         IV_TPSTAT_KEY =
          iv_monitor    = 'X'
        IMPORTING
*         EV_ALOG_LINENR          =
          et_tmstpalog  = i_log_calidad
*         ES_EXCEPTION  =
        CHANGING
          cv_start_date = ayer
*         cv_end_date   = sy-datum
*         cv_end_time   = sy-uzeit
        EXCEPTIONS
          alert         = 1
          OTHERS        = 2.
      IF sy-subrc <> 0.
        MESSAGE 'Error recuperando historia' TYPE 'S'.
      ENDIF.

      CALL FUNCTION 'TMS_MGR_READ_TRANSPORT_QUEUE'
        EXPORTING
          iv_system          = system
          iv_domain          = tmscdom-domnam
          iv_collect_data    = 'X'
          iv_count_only      = ''
          iv_read_shadow     = 'X'
          iv_use_data_files  = ''
          iv_verbose         = ''
          iv_expiration_date = sy-datum
          iv_allow_expired   = ''
        TABLES
          tt_buffer          = i_buffer_cal
        EXCEPTIONS
          read_config_failed = 1.
      IF sy-subrc <> 0.
        MESSAGE 'Error leyendo cola' TYPE 'I'.
      ENDIF.
    ENDIF.

    system = zcl_c=>entorno_produccion.
    ayer = sy-datum - p_dias.
    CALL FUNCTION 'TMS_TM_GET_HISTORY'
      EXPORTING
        iv_system         = system
        iv_domain         = tmscdom-domnam
        iv_allcli         = 'X'
*       IV_TRCLI          =
*       IV_TRFUNCTION     =
*       IV_PROJECT        =
        iv_imports        = 'X'
*       IV_EXPORTS        =
        iv_all_steps      = 'X'
        iv_all_alog_steps = ''
*       IV_TPSTAT_KEY     =
        iv_monitor        = 'X'
      IMPORTING
*       EV_ALOG_LINENR    =
        et_tmstpalog      = i_log_produccion
*       ES_EXCEPTION      =
      CHANGING
        cv_start_date     = ayer
*       cv_end_date       = sy-datum
*       cv_end_time       = sy-uzeit
      EXCEPTIONS
        alert             = 1
        OTHERS            = 2.
    IF sy-subrc <> 0.
      MESSAGE 'Error recuparando historia' TYPE 'S'.
    ENDIF.

    CALL FUNCTION 'TMS_MGR_READ_TRANSPORT_QUEUE'
      EXPORTING
        iv_system          = system
        iv_domain          = tmscdom-domnam
        iv_collect_data    = 'X'
        iv_count_only      = ''
        iv_read_shadow     = 'X'
        iv_use_data_files  = ''
        iv_verbose         = ''
        iv_expiration_date = sy-datum
        iv_allow_expired   = ''
      TABLES
        tt_buffer          = i_buffer_pro
      EXCEPTIONS
        read_config_failed = 1.
    IF sy-subrc <> 0.
      MESSAGE 'Error leyendo cola' TYPE 'I'.
    ENDIF.
*    DELETE lt_request_headers WHERE trkorr < 'D40K908525'. "PRUEBA!

    o_prog->o_sgpi->get_filas_tabla( lt_request_headers[] ).
    LOOP AT lt_request_headers INTO ls_request_header WHERE trkorr IN s_trkorr.
      sgpi_texto( texto1 = 'Procesando datos' cant_porc = 100 ).
      CLEAR: l_listado,
             l_color.

      l_listado-as4date        = ls_request_header-as4date.
      l_listado-as4time        = ls_request_header-as4time.
      l_listado-request_number = ls_request_header-trkorr.
      IF ls_request_header-strkorr IS NOT INITIAL.
        l_listado-request_number = ls_request_header-strkorr.
        CONTINUE.
      ENDIF.
      SELECT valor, string
        FROM ztemps
        INTO (@l_listado-proyecto, @l_listado-notas)
        UP TO 1 ROWS
        WHERE clave    = 'ZAP_OTS'
          AND subclave = @l_listado-request_number
        ORDER BY PRIMARY KEY.
      ENDSELECT.

      IF ls_request_header-trfunction = 'K'.
        l_listado-request_type = 'Workbench'.
      ELSEIF ls_request_header-trfunction = 'W'.
        l_listado-request_type = 'Customizing'.
      ENDIF.

      IF ls_request_header-tarsystem IS INITIAL.
        l_listado-system = `Local Change Requests`.
        IF p_excl IS INITIAL.
          CONTINUE.
        ENDIF.
      ELSE.
        l_listado-system = ls_request_header-tarsystem.
      ENDIF.

      l_listado-request_owner = ls_request_header-as4user.
      l_listado-description   = ls_request_header-as4text.

      IF ls_request_header-trstatus = 'D'.
        l_listado-modifiable = `Modificable`.
        string = l_listado-description.
        string = to_upper( string ).
        IF string CS '!NO!' OR string = 'BASURA' OR string = 'BORRAR' OR string CS 'NO TRANSPORTAR' OR l_listado-notas = 'Excluir'.
          l_color = 'R'.
          l_listado-message = 'Prohibido liberar de momento'.
          IF p_excl IS INITIAL.
            CONTINUE.
          ENDIF.
        ELSE.
          l_color = 'A'.
        ENDIF.
      ELSEIF ls_request_header-trstatus = 'R'.
        l_listado-modifiable = `Liberada`.
        DATA(l_ok) = ''.
        LOOP AT i_log_produccion ASSIGNING <log> WHERE trkorr = ls_request_header-trkorr.
        ENDLOOP.
        IF sy-subrc = 0.
          l_listado-fecha   = <log>-trtime(8).
          l_listado-hora    = <log>-trtime+8(6).
          l_listado-retcode = <log>-retcode.
          l_listado-trstep  = <log>-trstep.

          IF line_exists( i_buffer_pro[ trkorr = ls_request_header-trkorr ] ).
            IF <log>-trstep <> 'U'.
              l_color = 'V'.
              l_listado-message = 'En producción'.
              l_ok = 'X'.
            ENDIF.
          ELSEIF <log>-trstep = 'U'.
            l_color = 'R'.
            l_listado-message = 'Borrada de la cola de producción'.
            l_ndias = sy-datum - l_listado-fecha.
            IF l_ndias > 7.
              CONTINUE.
            ENDIF.
            l_ok = 'X'.
          ENDIF.
          IF l_ok IS INITIAL AND zcl_c=>entorno_calidad IS INITIAL ##BOOL_OK.
            l_color = 'V'.
            l_listado-message = 'En producción'.
            l_ok = 'X'.
          ENDIF.
        ENDIF.
        IF l_ok IS INITIAL.
          LOOP AT i_log_calidad ASSIGNING <log> WHERE     trkorr   = ls_request_header-trkorr
                                                      AND as4text <> ''. "?
          ENDLOOP.
          IF sy-subrc = 0.
            l_listado-fecha   = <log>-trtime(8).
            l_listado-hora    = <log>-trtime+8(6).
            l_listado-retcode = <log>-retcode.
            l_listado-trstep  = <log>-trstep.
            IF line_exists( i_buffer_cal[ trkorr = ls_request_header-trkorr ] ).
              l_listado-message = 'En calidad'.
              l_color = 'AZUL'.
            ELSE.
              l_color = 'R'.
              l_listado-message = 'Borrada de la cola de calidad'.
              l_ndias = sy-datum - l_listado-fecha.
              IF l_ndias > 7.
                CONTINUE.
              ENDIF.
            ENDIF.

            ASSIGN i_buffer_pro[ trkorr = ls_request_header-trkorr ] TO FIELD-SYMBOL(<buf_pro>).
            IF sy-subrc = 0.
              IF <buf_pro>-actflg = 'I'.
                l_listado-actflg = <buf_pro>-actflg.
                l_int = 1.
              ENDIF.
            ENDIF.
          ELSE.
            IF line_exists( i_liberadas[ request_number = ls_request_header-trkorr ] ).
              l_color = 'A'.
            ELSE.
              l_color = 'N'.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      IF p_excl IS INITIAL AND to_upper( l_listado-notas ) = '!NO!'.
        CONTINUE.
      ENDIF.

      set_status_list( EXPORTING color = l_color int = l_int CHANGING list = l_listado ).

      IF l_listado-retcode = '0008'.
        set_status_list( EXPORTING color = 'R' campos_color = 'RETCODE,MESSAGE' CHANGING list = l_listado ).
      ELSEIF l_listado-retcode = '0004'.
        set_status_list( EXPORTING color = 'W' campos_color = 'RETCODE,MESSAGE' CHANGING list = l_listado ).
      ENDIF.
      IF l_listado-actflg = 'I'.
        set_status_list( EXPORTING color = 'R' int = 1 campos_color = 'ACTFLG' CHANGING list = l_listado ).
      ELSEIF l_listado-actflg = 'X'.
        set_status_list( EXPORTING color = 'V' campos_color = 'ACTFLG' CHANGING list = l_listado ).
      ENDIF.

      l_listado-copy = icon_system_copy.
      l_listado-paso = get( tabla = 'D TRTPSTEP' clave = l_listado-trstep ).

      APPEND l_listado TO i_listado.
    ENDLOOP.

    SORT i_listado BY request_number.
    DELETE ADJACENT DUPLICATES FROM i_listado COMPARING request_number.

    IF p_fin IS INITIAL.
      DELETE i_listado WHERE ( message = 'En producción' OR message = 'Borrada de la cola de producción' ) AND retcode <= '0004'.
    ENDIF.
    DELETE i_listado WHERE NOT proyecto IN s_proyec.
  ENDMETHOD.

  METHOD listado.
    sgpi_texto( 'Generando informe' ).

    o_alv->add_button( button = 'F01' text = 'Refrescar' icon = icon_refresh ucomm = 'REFRESCAR' ).
    o_alv->add_button( button = 'F02' text = 'Liberar' icon = icon_release ucomm = 'LIBERAR' ).
    IF zcl_c=>entorno_calidad <> ''.
      o_alv->add_button( button = 'F03' text = 'Importar en calidad' icon = icon_import ucomm = 'IMP_CAL' ).
    ENDIF.
    o_alv->add_button( button = 'F04' text = 'Importar en producción' icon = icon_product_group ucomm = 'IMP_PRD' ).
*    o_alv->add_button( button = 'F05' qinfo = 'Activar'  icon = icon_activate ucomm = 'ACTIVAR' ).
    o_alv->add_button( button = 'F06' qinfo = 'Finalizadas' icon = icon_display_more ucomm = 'FIN' ).
    o_alv->add_button( button = 'F07' qinfo = 'Exportar a portapapeles' icon = icon_export ucomm = 'CLP' ).
    o_alv->add_button( button = 'F08' qinfo = 'Reseleccionar' icon = icon_interface ucomm = 'RESEL' ).
    o_alv->add_button( button = 'F09' qinfo = 'Liberar sin verificaciones' icon = icon_release ucomm = 'LIBERAR_SV' ).

    o_alv->set_field_text( 'PROYECTO,NOTAS' ).
    o_alv->set_field_text( campo = 'FECHA' valor = 'F.Import' ).
    o_alv->set_field_text( campo = 'HORA' valor = 'H.Import' ).
    o_alv->set_field_hotspot( campo = 'REQUEST_NUMBER,COPY' auto = 'X' ).
    o_alv->set_field_hotspot( campo = 'PROYECTO,NOTAS' valor = 'TEXT_EDT' ).
    o_alv->set_layout( p_vari ).

    o_alv->set_top_of_page( ).

    o_alv->set_field( campo = 'LIGHTS' op = 'KEY' ).
    o_alv->set_field_noout( 'CHECK' ).
    o_alv->set_seleccion( CHANGING t_tabla = i_listado ).

    o_alv->show( ).
  ENDMETHOD.

  METHOD fill_selections.
    DATA ls_selection   TYPE trwbo_selection.

    DATA lv_system_name TYPE sy-sysid.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA lv_system_type TYPE sy-sysid.

    DATA lv_sys_pattern TYPE e070-trkorr.

    CLEAR rt_selections.

    ls_selection-trkorrpattern               = space.
    ls_selection-client                      = sy-mandt.

    ls_selection-stdrequest                  = space.
    ls_selection-connect_req_task_conditions = 'X'.

* Types of requests
    ls_selection-reqfunctions(1)             = sctsc_type_workbench.
    ls_selection-reqfunctions+1(1)           = sctsc_type_customizing.
    ls_selection-reqfunctions+2(1)           = sctsc_type_transport.
    ls_selection-reqfunctions+3(3)           = sctsc_types_relocations.
    CONDENSE ls_selection-reqfunctions NO-GAPS.

* Types of assigned tasks
    ls_selection-taskfunctions   = sctsc_types_tasks.

* Status of tasks
*    IF trdyse01cm-sel_rt_icr = ' '.
*   Don't select released tasks in unreleased requests
*      IF trdyse01cm-sel_chan = 'X'.
    ls_selection-taskstatus(1)   = sctsc_state_protected.
    ls_selection-taskstatus+1(1) = sctsc_state_changeable.
*      ENDIF.

*      IF trdyse01cm-sel_rele = 'X'.
    ls_selection-taskstatus+2(1) = sctsc_state_released.
    ls_selection-taskstatus+3(1) = sctsc_state_notconfirmed.
*      ENDIF.
    ls_selection-taskstatus+4(1) = sctsc_state_export_started.

*   Free tasks are handled like tasks in requests
    ls_selection-singletasks     = 'X'.
    ls_selection-freetasks_f     = ls_selection-taskfunctions.
    ls_selection-freetasks_s     = ls_selection-taskstatus.

*    ELSE.
**   Select also released tasks in unreleased requests
*      ls_selection-taskstatus      = sctsc_states_all.
*
**   Free tasks are handled like requests
*      ls_selection-singletasks       = 'X'.
*      ls_selection-freetasks_f       = ls_selection-taskfunctions.
*
**   Status of free tasks ('OPEN' always)
*      IF trdyse01cm-sel_chan = 'X'.
*        ls_selection-freetasks_s(1)   = sctsc_state_protected.
*        ls_selection-freetasks_s+1(1) = sctsc_state_changeable.
*      ENDIF.
*      IF trdyse01cm-sel_rele = 'X'.
*        ls_selection-freetasks_s+2(1) = sctsc_state_released.
*        ls_selection-freetasks_s+3(1) = sctsc_state_notconfirmed.
*      ENDIF.
*      ls_selection-freetasks_s+4(1)    = sctsc_state_export_started.
*    ENDIF.

* Status of requests
*    IF  trdyse01cm-sel_chan = 'X'
*    AND trdyse01cm-sel_rele = ' '.
    ls_selection-reqstatus(1)    = sctsc_state_protected.
    ls_selection-reqstatus+1(1)  = sctsc_state_changeable.
    ls_selection-reqstatus+2(1)  = sctsc_state_export_started.
    APPEND ls_selection TO rt_selections.
*
*    ELSEIF trdyse01cm-sel_chan = ' '
*    AND    trdyse01cm-sel_rele = 'X'.
    ls_selection-reqstatus(1)   = sctsc_state_released.
    ls_selection-reqstatus+1(1) = sctsc_state_export_started.
*
*      IF NOT trdyse01cm-since_date IS INITIAL.
*        ls_selection-fromdate = trdyse01cm-since_date.
    ls_selection-fromdate       = sy-datum - p_dias.
    ls_selection-todate         = sy-datum.
*      ENDIF.
    APPEND ls_selection TO rt_selections.
*
*    ELSEIF trdyse01cm-sel_chan = 'X'
*    AND    trdyse01cm-sel_rele = 'X'.
*      IF trdyse01cm-since_date IS INITIAL.
*        ls_selection-reqstatus      = sctsc_states_all.
*        APPEND ls_selection TO rt_selections.
*      ELSE.
*        ls_selection-reqstatus(1)   = sctsc_state_protected.
*        ls_selection-reqstatus+1(1) = sctsc_state_changeable.
*        ls_selection-reqstatus+2(1) = sctsc_state_export_started.
*        ls_selection-freetasks_s    = ls_selection-reqstatus.
*        APPEND ls_selection TO rt_selections.
*
*        ls_selection-taskstatus     = sctsc_state_released.
*        ls_selection-reqstatus      = sctsc_state_released.
*        ls_selection-freetasks_s    = ls_selection-reqstatus.
*        ls_selection-fromdate       = trdyse01cm-since_date.
*        ls_selection-todate         = sy-datum.
*        APPEND ls_selection TO rt_selections.
*      ENDIF.
*    ENDIF.

    CALL FUNCTION 'TR_SYS_PARAMS'
      IMPORTING
        systemname = lv_system_name
        systemtype = lv_system_type.

    lv_sys_pattern(3)   = lv_system_name(3).
    lv_sys_pattern+3(1) = '%'.

    LOOP AT rt_selections INTO ls_selection.
      IF     ls_selection-trkorrpattern  = space
         AND ls_selection-reqfunctions  NA 'FDP'.
        ls_selection-trkorrpattern = lv_sys_pattern.
        MODIFY rt_selections FROM ls_selection.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD liberar.
    DATA ls_return TYPE bapiret2.

    IF list-message = 'Prohibido liberar de momento'.
      MESSAGE list-message TYPE 'I'.
    ELSE.
      CALL FUNCTION 'BAPI_CTREQUEST_RELEASE'
        EXPORTING
          requestid = list-request_number
          complete  = 'X' " Release request including tasks
        IMPORTING
          return    = ls_return.

      IF ls_return-type IS INITIAL.
        list-modifiable = `Liberada`.
        APPEND list TO i_liberadas.
        MESSAGE 'Se ha liberado la orden' TYPE 'S'.
      ELSE.
        MESSAGE ID ls_return-id TYPE ls_return-type NUMBER ls_return-number WITH ls_return-message_v1 ls_return-message_v2 ls_return-message_v3 ls_return-message_v4.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD activar.
    DATA l_system       TYPE tmscsys-sysnam.
    DATA lt_tr_requests TYPE TABLE OF tmsbuffer.

* No tenemos implementado esta opción actualmente
    RETURN.

    IF sistema = 'CAL'.
      l_system = zcl_c=>entorno_calidad.
      " TODO: variable is assigned but never used (ABAP cleaner)
      DATA(l_clave) = |{ sy-uname }CLAVEQ|.
    ELSEIF sistema = 'PRD'.
      l_system = zcl_c=>entorno_produccion.
      l_clave = |{ sy-uname }CLAVEP|.
    ENDIF.

    APPEND VALUE #( domnam = 'DOMAIN_D40' sysnam = l_system trkorr = list-request_number trfunc = 'X' actflg = 'X' preflg = 'N' comsys = sy-sysid srccli = sy-mandt ) TO lt_tr_requests.
    CALL FUNCTION 'TMS_UI_MAINTAIN_TR_QUEUE'
      EXPORTING
        iv_system      = l_system
        iv_request     = list-request_number
        iv_tarcli      = sy-mandt
        iv_act_request = 'X'
        iv_verbose     = ''
        iv_expert_mode = ''
        it_requests    = lt_tr_requests
      EXCEPTIONS
        OTHERS         = 99.

    IF sy-subrc = 0.
      MESSAGE 'Se ha activado la orden' TYPE 'S'.
    ELSE.
      MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.

  METHOD importar.
    DATA l_system TYPE tmscsys-sysnam.

    CLEAR cancelado.
    IF sistema = 'CAL'.
      l_system = zcl_c=>entorno_calidad.
      DATA(l_clave) = |{ sy-uname }CLAVEQ|.
    ELSEIF sistema = 'PRD'.
      l_system = zcl_c=>entorno_produccion.
      l_clave = |{ sy-uname }CLAVEP|.
    ENDIF.

    IF NOT l_clave IS INITIAL.

      o_prog->string = zcl_ap_temp=>get_st_valor1( clave = 'ZAP_OTS' subclave = l_clave ).
      IF NOT o_prog->string IS INITIAL.
        zcl_ap_string=>to_clipboard( string = o_prog->string ).
        MESSAGE |Se copia en portapapeles clave { o_prog->string }| TYPE 'S'.
      ENDIF.
    ENDIF.

*    IF l_system = zcl_c=>entorno_produccion OR l_system = zcl_c=>entorno_calidad.
*      activar( EXPORTING sistema = sistema CHANGING list = list ).
*    ENDIF.

*  if sy-cprog = 'ZAP_OTS'.
*    gs_dyn220-online = 'X'.
*    gs_dyn220-offline = ''.
*    GS_DYN220-OVERTAKE = ''. "Dejar_orden_transporte_en_la_cola_para_import_siguiente____
*    GS_DYN220-IMP_AGAIN = ''. "Importar_orden_de_transporte_otra_vez
*    GS_DYN220-IGN_ORIG = ''. "Sobrescribir_originales
*    GS_DYN220-IGN_REP = ''. "Sobrescribir_objetos_en_reparaciones_sin_confirmar
*    GS_DYN220-IGN_TYP = ''. "Ignorar_cl.transporte_no_permitida_
*    GS_DYN220-IGN_TAB = ''. "Ignorar_clase_de_tabla_no_permitida__
*    GS_DYN220-IGN_PRE = ''. "Ignorar_relaciones_predecesor__
*    GS_DYN220-IGN_CVERS = 'X'. "Ignorar_versión_de_componente_no_adecuada___
*    WTMSU-CLIENT = sy-mandt.
*  endif.
*
    IF tmscdom-domnam IS INITIAL.
      SELECT domnam FROM tmscdom                    "#EC "#EC CI_BYPASS
        INTO CORRESPONDING FIELDS OF tmscdom
      UP TO 1 ROWS
      ORDER BY moddat DESCENDING.
      ENDSELECT.
    ENDIF.

    CALL FUNCTION 'TMS_UI_IMPORT_TR_REQUEST'
      EXPORTING
        iv_system             = l_system
        iv_domain             = tmscdom-domnam
        iv_request            = list-request_number
        iv_tarcli             = sy-mandt
        iv_some_active        = 'X'
        iv_verbose            = ''
        iv_expert_mode        = ''
        iv_check_strategy     = 'X'
      EXCEPTIONS
        cancelled_by_user     = 1
        import_request_denied = 2
        import_request_failed = 3
        OTHERS                = 4.

    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.
      MESSAGE 'Se ha importado la orden' TYPE 'S'.
    ELSE.
      cancelado = 'X'.
      IF sy-subrc > 1.
        IF sy-msgty IS INITIAL.
          MESSAGE |Error { sy-subrc } importando orden| TYPE 'I'.
        ELSE.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  o_prog = NEW #(
    status        = 'INICIO_DYN'
    status_prog   = 'ZAP_STATUS'
    get_nombre_pc = 'X'
    no_param      = 'X' ).

  IF sy-uname = zcl_c=>usuario_ap.
    PERFORM add_button IN PROGRAM zap_status
            USING 'M01'
                  'Claves'
                  ''
                  ''.
  ENDIF.

  o_alv = NEW #(
    status             = 'STANDARD_ALV_DYN'
    status_prog        = 'ZAP_STATUS'
    top_of_page_auto   = 'X'
    top_of_page_titulo = 'X'
    color              = 'COLOR' ).

  p_vari = o_alv->get_default_layout( ).

  o_prog->initialization_i( CHANGING sscrfields = sscrfields ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  p_vari = o_alv->get_f4_layout( ).

************************************************************************
* AT SELECTION-SCREEN.
************************************************************************
AT SELECTION-SCREEN.
  TYPES: BEGIN OF t_cl,
           subclave TYPE ztemp-subclave,
           valor1   TYPE ztemp-valor1,
         END OF t_cl.

  DATA i_cl    TYPE TABLE OF t_cl.
  DATA l_ucomm TYPE sy-ucomm.

  CASE sy-ucomm.
    WHEN 'M01'.
      i_cl = VALUE #( ( subclave = |{ sy-uname }CLAVED| ) ( subclave = |{ sy-uname }CLAVEQ| ) ( subclave = |{ sy-uname }CLAVEP| ) ).
      LOOP AT i_cl ASSIGNING FIELD-SYMBOL(<cl>).
        <cl>-valor1 = zcl_ap_temp=>get_st_valor1( clave = 'ZAP_OTS' subclave = <cl>-subclave ).
      ENDLOOP.
      CALL FUNCTION 'Z_POPUP_ALV_AP'
        EXPORTING
          botones      = 'OK_CANCEL'
          campos_input = 'VALOR1'
        IMPORTING
          ucomm        = l_ucomm
        TABLES
          t_datos      = i_cl.
      IF l_ucomm = 'F01'.
        LOOP AT i_cl ASSIGNING <cl>.
          zcl_ap_temp=>set_st( clave = 'ZAP_OTS' subclave = <cl>-subclave valor1 = <cl>-valor1 ).
        ENDLOOP.
      ENDIF.
    WHEN OTHERS.
      o_prog->at_selection( ).
  ENDCASE.

AT SELECTION-SCREEN ON EXIT-COMMAND.
  o_prog->at_selection( ).

*----------------------------------------------------------------------
* START-OF-SELECTION.
*----------------------------------------------------------------------*
START-OF-SELECTION.
  o_prog->main( ).

  IF 0 = 1. " Sólo para que no salga mensaje en ATC
    DATA header TYPE trheader.
    PERFORM f4_se09 CHANGING header.
  ENDIF.

* INCLUDE LSCTSREQFH1
*FORM user_command_0110.
*
*  CASE sy-ucomm.
*    WHEN 'AT_LINE_DELETE'.
*      PERFORM delete_attribute.
*  ENDCASE.
*
*  if sy-ucomm = 'F4' AND SY-UNAME = ZCL_C=>USUARIO_aP.
*    PERFORM f4_se09 IN PROGRAM ZAP_OTS CHANGING TRheader.
*  ENDIF.
*
FORM f4_se09 CHANGING header TYPE trheader.
*  DATA: o_popup TYPE REF TO zcl_ap_matchcode_z.
*
*  DATA: BEGIN OF i_textos OCCURS 0,
*          as4text TYPE e07t-as4text,
*        END OF i_textos.
*
*  SELECT as4text FROM e070 JOIN e07t ON e070~trkorr = e07t~trkorr
*    UP TO 50 ROWS
*    INTO TABLE @DATA(i_ots)
*   WHERE as4user = @sy-uname
*    ORDER BY as4date DESCENDING.
*
*  LOOP AT i_ots ASSIGNING FIELD-SYMBOL(<ots>).
*    IF NOT line_exists( i_textos[ as4text = <ots>-as4text ] ).
*      i_textos-as4text = <ots>-as4text.
*      APPEND i_textos.
*    ENDIF.
*  ENDLOOP.
*
*
*  CREATE OBJECT o_popup
*    EXPORTING
*      tabname = 'E07T'.
*
*  o_popup->add_field( field = 'AS4TEXT' selectflag = 'X' ).
*
*  LOOP AT i_textos.
*    o_popup->add_valor( i_textos-as4text ).
*  ENDLOOP.
*
*  CLEAR i_textos.
*  o_popup->matchcode( EXPORTING field   = 'AS4TEXT'
*                      CHANGING  valor   = i_textos ).
*
*  IF NOT i_textos IS INITIAL.
*    header-as4text = i_textos.
*  ENDIF.

  PERFORM f4_se09_obj USING ''
          CHANGING header-as4text.
ENDFORM.


*INCLUDE LSTR8F01
*FORM user_command.
*  DATA: lv_ok              TYPE c,
*        lv_answer          TYPE c,
*        lv_del_idx         TYPE i.
*
*  IF SY-UNAME = ZCL_C=>USUARIO_AP AND KO013-AS4TEXT IS INITIAL.
*    FIELD-SYMBOLS <FS>.
*    ASSIGN ('(SAPLSEDTATTR)TRDIR-NAME') TO <FS>.
*    PERFORM f4_se09_OBJ IN PROGRAM ZAP_OTS USING <FS> CHANGING KO013-AS4TEXT.
*  ENDIF.

FORM f4_se09_obj USING    objeto      TYPE any
                 CHANGING descripcion TYPE any.

  DATA l_hoy   TYPE char10.
  DATA o_popup TYPE REF TO zcl_ap_matchcode_z.

  FIELD-SYMBOLS <fs>.

  DATA: BEGIN OF i_ots OCCURS 0,
          as4text  TYPE e07t-as4text,
          as4date  TYPE e070-as4date,
          obj_name TYPE e071-obj_name,
        END OF i_ots.

  DATA: l_objeto  TYPE string,
        l_sistema TYPE string,
        r_trkorr  TYPE RANGE OF e071-trkorr,
        i_textos  LIKE i_ots OCCURS 0 WITH HEADER LINE.

  IF NOT objeto IS INITIAL.
    l_objeto = objeto.
  ELSE.
    ASSIGN ('(SAPLSTRD)KO008-OBJ_NAME') TO <fs>.
    IF sy-subrc = 0.
      l_objeto = <fs>.
    ENDIF.
  ENDIF.

  CONCATENATE sy-sysid '%' INTO l_sistema.

  IF NOT l_objeto IS INITIAL.
    SELECT trkorr FROM e071
      INTO TABLE @DATA(i_ots_obj)
      WHERE obj_name    = @l_objeto
        AND trkorr   LIKE @l_sistema.
    IF sy-subrc = 0.
      LOOP AT i_ots_obj ASSIGNING FIELD-SYMBOL(<obj>).
        APPEND VALUE #( option = 'EQ' sign = 'I' low = <obj>-trkorr ) TO r_trkorr.
      ENDLOOP.

      SELECT as4text as4date
        FROM                              e070 JOIN e07t
             ON e070~trkorr = e07t~trkorr
        INTO CORRESPONDING FIELDS OF TABLE i_ots
        WHERE as4user = sy-uname
          AND (    e070~trkorr IN r_trkorr
                OR strkorr     IN r_trkorr )
        ORDER BY as4date DESCENDING.
      LOOP AT i_ots ASSIGNING FIELD-SYMBOL(<ots>).
        <ots>-obj_name = l_objeto.
      ENDLOOP.
    ENDIF.
  ENDIF.

  SELECT as4text as4date
    FROM                              e070 JOIN e07t
         ON e070~trkorr = e07t~trkorr
    UP TO 50 ROWS
    APPENDING CORRESPONDING FIELDS OF TABLE i_ots
    WHERE as4user        = sy-uname
      AND e070~trkorr LIKE l_sistema
    ORDER BY as4date DESCENDING.

  LOOP AT i_ots ASSIGNING <ots>.
    DATA(i_fechas) = zcl_ap_regexp=>buscar_fechas( string = CONV #( <ots>-as4text ) ).
    IF sy-subrc = 0.
      WRITE sy-datum TO l_hoy.
      LOOP AT i_fechas ASSIGNING FIELD-SYMBOL(<fecha>).
        REPLACE ALL OCCURRENCES OF <fecha> IN <ots>-as4text WITH l_hoy.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

  LOOP AT i_ots ASSIGNING <ots>.
    IF NOT line_exists( i_textos[ as4text = <ots>-as4text ] ).
      APPEND <ots> TO i_textos.
    ENDIF.
  ENDLOOP.

  SORT i_textos BY obj_name DESCENDING
                   as4date DESCENDING.

  o_popup = NEW #(
    tabname = 'E07T' ).

  o_popup->add_field( field = 'AS4TEXT' selectflag = 'X' ).
  o_popup->add_field( field = 'AS4DATE' tabname = 'E070' ).
  o_popup->add_field( field = 'OBJ_NAME' tabname = 'E071' ).

  LOOP AT i_textos.
    o_popup->add_valor( i_textos-as4text ).
    o_popup->add_valor( i_textos-as4date ).
    o_popup->add_valor( i_textos-obj_name ).
  ENDLOOP.

  CLEAR i_textos.
  o_popup->matchcode( EXPORTING field = 'AS4TEXT'
                      CHANGING  valor = i_textos ).

  IF NOT i_textos IS INITIAL.
    descripcion = i_textos.
  ENDIF.
ENDFORM.
