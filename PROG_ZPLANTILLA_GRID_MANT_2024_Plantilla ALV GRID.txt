***********************************************************************
* TIPO : MANTENIMIENTO
* TITULO : Mantenimiento
* DESCRIPCION : Mantenimiento
*
* AUTOR: Andres Picazo                                FECHA: 23/10/2024
* ANALISTA: Andres Picazo
*
***********************************************************************
REPORT  zplantilla_grid_mant_20024.

TABLES ztemp.

CONSTANTS tabla TYPE tabname VALUE 'ZTEMP'.

CLASS lcl_event_grid DEFINITION INHERITING FROM zcl_ap_alv_grid_eventos FINAL.
  PUBLIC SECTION.
    METHODS: data_changed REDEFINITION,
      data_changed_finished REDEFINITION,
      user_command REDEFINITION,
      toolbar      REDEFINITION.
ENDCLASS.


*----------------------------------------------------------------------*
*       CLASS zcl_report DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS zcl_report DEFINITION INHERITING FROM zcl_ap_dev FINAL.
  PUBLIC SECTION.
    TYPES: BEGIN OF  t_listado,
             check         TYPE xfeld,
             lights        TYPE zico_estado_mensaje.
             INCLUDE TYPE ztemp.
             TYPES:   message       TYPE bapi_msg,
             clave_interna TYPE string,
             style         TYPE lvc_t_styl,
             color         TYPE lvc_t_scol,
             updkz         TYPE char1,
             tabix         TYPE int4,
           END OF t_listado,
           tt_listado TYPE STANDARD TABLE OF t_listado.

    DATA: i_listado           TYPE tt_listado,
          i_listado_ini       TYPE tt_listado,
          i_listado_mod       TYPE tt_listado,
          tabix               TYPE sy-tabix,
          campos_clave        TYPE string,
          campos_input        TYPE string,
          campos_obligatorios TYPE string,
          longitud_clave      TYPE int4,
          i_campos_tabla      TYPE TABLE OF dd03l,
          i_campos_alv        TYPE lvc_t_fcat,
          where               TYPE string.

    DATA: o_alv   TYPE REF TO zcl_ap_alv_grid,
          o_event TYPE REF TO lcl_event_grid.

    METHODS: buscar_datos REDEFINITION,

      validaciones IMPORTING !mod    TYPE abap_bool DEFAULT ''
                   CHANGING  listado TYPE t_listado ##NEEDED,

      status_dynpro_0100,
      command_dynpro_0100,
      configuracion,

      where_clave IMPORTING listado      TYPE t_listado
                  RETURNING VALUE(where) TYPE string.

ENDCLASS.

DATA o_prog TYPE REF TO zcl_report ##NEEDED.

SELECTION-SCREEN BEGIN OF BLOCK 001 WITH FRAME.
SELECT-OPTIONS: s_clave FOR ztemp-clave,
                s_subcl FOR ztemp-subclave.
SELECTION-SCREEN SKIP.
PARAMETERS p_vari LIKE disvariant-variant.
SELECTION-SCREEN END OF BLOCK 001.
PARAMETERS p_vis NO-DISPLAY. " Si queremos opción de no editar.
__botones_plantilla.


************************************************************************
*
* LOGICA DEL PROGRAMA
*
************************************************************************
CLASS lcl_event_grid IMPLEMENTATION.
  METHOD toolbar.
    super->toolbar( e_object = e_object e_interactive = e_interactive ).

    add_boton( function = 'COPIAR' icon = icon_system_copy text = 'Copiar' quickinfo = 'Copiar' e_object = e_object ).
  ENDMETHOD.

  METHOD user_command.
    DATA: l_listado TYPE o_prog->t_listado,
          l_hay_sel TYPE c LENGTH 1.

    CASE e_ucomm.
      WHEN 'NUEVO'.
        o_prog->tabix = o_prog->tabix + 1.
        l_listado-tabix = o_prog->tabix.
        o_prog->validaciones( EXPORTING mod = 'I' CHANGING listado = l_listado ).
        APPEND l_listado TO o_prog->i_listado.

        o_alv->refrescar_grid( ).
      WHEN 'BORRAR'.
        o_alv->comprobar_cambios( ).
        o_alv->set_marca_filas_sel( EXPORTING validar_seleccion = 'X' CHANGING t_tabla = o_prog->i_listado hay_sel = l_hay_sel ).
        IF l_hay_sel = 'X'.
          LOOP AT o_prog->i_listado ASSIGNING FIELD-SYMBOL(<listado>) WHERE check = 'X'.
            IF <listado>-updkz = 'I'.
              DELETE o_prog->i_listado.
            ELSEIF <listado>-updkz = 'D'.
              CLEAR: <listado>-updkz, <listado>-lights, <listado>-check.
              o_prog->validaciones( EXPORTING mod = 'U' CHANGING listado = <listado> ).
            ELSE.
              o_prog->validaciones( EXPORTING mod = 'D' CHANGING listado = <listado> ).
            ENDIF.
          ENDLOOP.
          o_alv->refrescar_grid( ).
        ENDIF.

      WHEN 'COPIAR'.
        o_alv->comprobar_cambios( ).
        o_alv->set_marca_filas_sel( EXPORTING validar_seleccion = 'X' CHANGING t_tabla = o_prog->i_listado hay_sel = l_hay_sel ).
        IF l_hay_sel = 'X'.
          LOOP AT o_prog->i_listado ASSIGNING <listado> WHERE check = 'X'.
            CLEAR <listado>-check.
            l_listado = <listado>.
            o_prog->validaciones( EXPORTING mod = 'I' CHANGING listado = l_listado ).
            APPEND l_listado TO o_prog->i_listado.
          ENDLOOP.
          SORT o_prog->i_listado BY tabix.
          LOOP AT o_prog->i_listado ASSIGNING <listado>.
            <listado>-tabix = sy-tabix.
          ENDLOOP.
          o_alv->refrescar_grid( soft_refresh = '' ).
        ENDIF.
      WHEN OTHERS.
        super->user_command( e_ucomm = e_ucomm ).
    ENDCASE.
  ENDMETHOD.

  METHOD data_changed.
    ini_data_changed( cambios = er_data_changed->mt_good_cells  ).

    LOOP AT i_cambios_celda INTO cambio_celda.
      AT NEW row_id.
        READ TABLE o_prog->i_listado INTO DATA(l_listado_ini) INDEX cambio_celda-row_id. "#EC CI_SUBRC
        DATA(l_listado) = l_listado_ini.
      ENDAT.

      set_valor_mod( CHANGING datos = l_listado ).

      AT END OF row_id.
        o_prog->validaciones( EXPORTING mod = 'U' CHANGING listado = l_listado ).
        MODIFY o_prog->i_listado FROM l_listado INDEX cambio_celda-row_id.
        actualizar_fila( fila_ini = l_listado_ini fila_fin = l_listado er_data_changed = er_data_changed fila = cambio_celda-row_id ).
        IF l_listado-style NE l_listado_ini-style OR l_listado-color NE l_listado_ini-color.
          APPEND l_listado TO o_prog->i_listado_mod. "No me guarda los cambios en los estilos
        ENDIF.
      ENDAT.
    ENDLOOP.
  ENDMETHOD.

  METHOD data_changed_finished.

    IF NOT tabla_data_changed IS INITIAL.
      LOOP AT o_prog->i_listado_mod ASSIGNING FIELD-SYMBOL(<list_mod>).
        READ TABLE o_prog->i_listado ASSIGNING FIELD-SYMBOL(<list>) WITH KEY tabix = <list_mod>-tabix.
        IF sy-subrc = 0.
          <list> = <list_mod>.
        ENDIF.
        DELETE o_prog->i_listado_mod.
      ENDLOOP.
      IF sy-subrc = 0.
        o_alv->refrescar_grid( soft_refresh = 'X' ).
      ENDIF.
      CLEAR tabla_data_changed.
    ENDIF.

  ENDMETHOD.
ENDCLASS.


*----------------------------------------------------------------------*
*       CLASS zcl_report IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS zcl_report IMPLEMENTATION.
  METHOD buscar_datos.
    sgpi_texto( 'Seleccionando datos'(sda) ).

    CLEAR i_listado.
    SELECT * FROM (tabla)
      INTO CORRESPONDING FIELDS OF TABLE i_listado
     WHERE clave IN s_clave
       AND subclave IN s_subcl
      ORDER BY PRIMARY KEY.

    o_prog->o_sgpi->get_filas_tabla( i_listado[] ).
    LOOP AT i_listado ASSIGNING FIELD-SYMBOL(<listado>).
      tabix = sy-tabix.
      <listado>-tabix = tabix.
      sgpi_texto( texto1 = 'Procesando datos'(pda) cant_porc = 100 ).

      validaciones( CHANGING listado = <listado> ).
    ENDLOOP.
    SORT i_listado BY clave_interna.
    i_listado_ini = i_listado.
  ENDMETHOD.                                               " seleccionar_datos

  METHOD status_dynpro_0100.
    IF inicio IS NOT INITIAL.
      RETURN.
    ENDIF.

    status_dynpro( EXPORTING cprog = 'ZAP_STATUS' status = 'ST_DYN' titulo = string CHANGING i_listado = i_listado ).

    inicio = 'X'.
    o_alv->variant-variant = p_vari.
    o_alv->registrar_mod( ).
    o_alv->set_layout( ancho_optimizado = 'X' no_rowmove = 'X' no_rowins = 'X' style = 'STYLE' colort = 'COLOR' ).
    o_alv->quitar_opciones( cl_gui_alv_grid=>mc_fc_refresh ).
    o_alv->set_campos_tabint( i_listado[] ).
    o_alv->set_field_quitar( 'CHECK,MANDT,MESSAGE,TABIX,UPDKZ,CLAVE_INTERNA' ).

    IF p_vis IS INITIAL.
      o_alv->add_button( button = 'F01' text = 'Grabar' icon = icon_system_save ucomm = 'GRABAR' ).

      IF sy-sysid = zcl_c=>entorno_desarrollo.
        o_alv->add_button( button = 'F02' text = 'Crear OT'  icon =  icon_import_transport_request ucomm = 'OT' ).
        o_alv->add_button( button = 'F03' text = 'Comparar con producción'  icon =  icon_compare ucomm = 'COMP' ).
      ELSE.
        o_alv->add_button( button = 'F03' text = 'Comparar con desarrollo'  icon =  icon_compare ucomm = 'COMP' ).
      ENDIF.
      o_alv->set_field_input( campos_input ).
    ENDIF.

    o_alv->set_orden( campos_clave ).

    sgpi_texto( 'Generando informe' ).
    o_alv->show( CHANGING tabla = i_listado ).

    i_campos_alv = o_alv->get_fcat_inicial( ).
  ENDMETHOD.

  METHOD command_dynpro_0100.
    DATA: l_comm_sel TYPE string VALUE 'OT,LIST_EMP,SIMULAR',
          l_hay_sel  TYPE c LENGTH 1,
          l_cont     TYPE i.
    DATA: i_list_dup TYPE TABLE OF t_listado,
          l_list_dup TYPE t_listado.
    DATA: l_key  TYPE c LENGTH 120,
          i_keys TYPE TABLE OF string.

    command_dynpro( EXPORTING o_alv = o_alv seleccion = l_comm_sel
                            CHANGING i_listado = i_listado i_listado_ini = i_listado_ini hay_sel = l_hay_sel ).

    CASE ucomm.
      WHEN 'GRABAR'.
        LOOP AT i_listado ASSIGNING FIELD-SYMBOL(<listado>).
          IF line_exists( i_list_dup[ clave_interna = <listado>-clave_interna  ] ).
            i_list_dup[ clave_interna = <listado>-clave_interna  ]-tabix = 2.
          ELSE.
            l_list_dup-clave_interna = <listado>-clave_interna.
            l_list_dup-tabix = 1.
            APPEND l_list_dup TO i_list_dup.
          ENDIF.
          CLEAR <listado>-color.
        ENDLOOP.
        LOOP AT i_list_dup ASSIGNING FIELD-SYMBOL(<list>) WHERE tabix > 1.
          LOOP AT i_listado ASSIGNING <listado> WHERE clave_interna = <list>-clave_interna.
            set_status_list( EXPORTING color = 'R' CHANGING list = <listado> ).
          ENDLOOP.
        ENDLOOP.
        IF sy-subrc = 0.
          MESSAGE 'No puede grabar si hay duplicados' TYPE 'I'.
          o_alv->refrescar_grid( ).
          RETURN.
        ENDIF.

        LOOP AT i_listado ASSIGNING <listado> WHERE updkz <> '' AND NOT message IS INITIAL.
          MESSAGE 'No puede grabar si hay errores' TYPE 'I'.
          RETURN.
        ENDLOOP.

        LOOP AT i_listado ASSIGNING <listado> WHERE updkz <> ''.

          MOVE-CORRESPONDING <listado> TO ztemp.
          l_cont = l_cont + 1.
          IF <listado>-updkz = 'D'.
            DATA(l_where_delete) = where_clave( <listado> ).
            DELETE FROM (tabla)
             WHERE (l_where_delete).
            IF sy-subrc = 0.
              DELETE i_listado.
            ENDIF.
          ELSE.
            CLEAR: <listado>-updkz, <listado>-lights, <listado>-message, <listado>-style.
            CLEAR ztemp.
            MOVE-CORRESPONDING <listado> TO ztemp.
            MODIFY ztemp FROM ztemp.
          ENDIF.
        ENDLOOP.

        IF l_cont = 0.
          MESSAGE 'No había cambios' TYPE 'S'.
        ELSE.
          MESSAGE |Se han grabado { l_cont } registros| TYPE 'S'.
          i_listado_ini = i_listado.
        ENDIF.
        o_alv->refrescar_grid( ).

      WHEN 'OT'.
        LOOP AT i_listado ASSIGNING <listado> WHERE check = 'X'.
          MOVE-CORRESPONDING <listado> TO ztemp.
          ztemp-mandt = sy-mandt.
          l_key = ztemp(longitud_clave).
          APPEND l_key TO i_keys.
        ENDLOOP.

        zcl_ap_utils=>grabar_tabla_en_ot( tabla = tabla i_keys = i_keys ).

      WHEN 'COMP'.
        SUBMIT zap_comparar_tablas
          AND RETURN
               WITH s_table = tabla.
*             WITH p_clave = p_cprog. "CAMBIAR

    ENDCASE.
  ENDMETHOD.

  METHOD validaciones.
    DATA: campos_edit  TYPE string,
          campos_error TYPE string.

    CLEAR: listado-message, listado-style, listado-color, listado-lights.

    CASE mod.
      WHEN 'I'.
        listado-updkz = 'I'.
      WHEN 'D'.
        listado-updkz = 'D'.
      WHEN 'U'.
        IF listado-updkz IS INITIAL.
          ASSIGN i_listado_ini[ tabix = listado-tabix ] TO FIELD-SYMBOL(<ini>).
          IF sy-subrc <> 0.
            listado-updkz = 'U'.
          ELSEIF <ini> <> listado.
            listado-updkz = 'U'.
          ENDIF.
        ENDIF.
    ENDCASE.

    CASE listado-updkz.
      WHEN 'I'.
        DATA(l_icono) = icon_create.
        campos_edit = campos_clave.
      WHEN 'D'.
        l_icono = icon_delete.
      WHEN 'U'.
        l_icono = icon_change.
    ENDCASE.

    IF NOT listado-updkz IS INITIAL.
      SPLIT campos_obligatorios AT ',' INTO TABLE DATA(i_campos_obl).
      LOOP AT i_campos_obl ASSIGNING FIELD-SYMBOL(<obl>).
        ASSIGN COMPONENT <obl> OF STRUCTURE listado TO FIELD-SYMBOL(<fs>).
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.

        IF <fs> IS NOT INITIAL.
          CONTINUE.
        ENDIF.

        __add_lista campos_error <obl>.
        IF listado-message IS NOT INITIAL.
          CONTINUE.
        ENDIF.

        ASSIGN i_campos_alv[ fieldname = <obl> ] TO FIELD-SYMBOL(<alv>).
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.

        DATA(l_desc_campo) = <alv>-scrtext_m.
        IF l_desc_campo IS INITIAL.
          SELECT SINGLE ddtext FROM dd03t
           INTO l_desc_campo
          WHERE tabname    = tabla
            AND ddlanguage = sy-langu
            AND fieldname  = <obl>.
          IF sy-subrc <> 0.
            ASSIGN i_campos_tabla[ fieldname = <obl> ] TO FIELD-SYMBOL(<campo>).
            SELECT ddtext FROM dd04t
              INTO l_desc_campo
              UP TO 1 ROWS
             WHERE ddlanguage = sy-langu
               AND rollname   = <campo>-rollname
              ORDER BY PRIMARY KEY.
            ENDSELECT.
          ENDIF.
          IF l_desc_campo IS INITIAL.
            l_desc_campo = <obl>.
          ENDIF.
        ENDIF.

        listado-message = |Informe campo { l_desc_campo }|.
        l_icono = icon_red_light.
      ENDLOOP.
    ENDIF.

    CLEAR: listado-clave_interna.
    LOOP AT i_campos_tabla ASSIGNING <campo> WHERE keyflag = 'X'.
      ASSIGN COMPONENT <campo>-fieldname OF STRUCTURE listado TO <fs>.
      IF sy-subrc = 0.
        CONCATENATE listado-clave_interna <fs> INTO listado-clave_interna SEPARATED BY '-'.
      ENDIF.
    ENDLOOP.

    set_status_list( EXPORTING message = listado-message
                               icono = l_icono
                               campos_editables = campos_edit
                               resaltar_campos = campos_error
                               color_resalte = 'R'
                     CHANGING list = listado ).
  ENDMETHOD.

  METHOD configuracion.
    CLEAR: campos_clave, i_campos_tabla, i_campos_alv.

    i_campos_alv = get_fieldcatalog_tabla_alv( tabla = i_listado ).

    SELECT fieldname keyflag leng position FROM dd03l
      INTO CORRESPONDING FIELDS OF TABLE i_campos_tabla
     WHERE tabname = tabla
       AND fieldname NE 'MANDT'
     ORDER BY position.
    IF sy-subrc <> 0.
      MESSAGE |No existe tabla { tabla }| TYPE 'E'.
    ENDIF.

    LOOP AT i_campos_tabla ASSIGNING FIELD-SYMBOL(<campo>).
      IF line_exists(  i_campos_alv[ fieldname = <campo>-fieldname ] ).
        IF <campo>-keyflag = 'X'.
          IF campos_clave IS INITIAL.
            campos_clave = <campo>-fieldname.
          ELSE.
            CONCATENATE campos_clave <campo>-fieldname INTO campos_clave SEPARATED BY ','.
          ENDIF.
          longitud_clave = longitud_clave + <campo>-leng.
        ELSE.
          __add_lista campos_input <campo>-fieldname.
        ENDIF.
      ENDIF.
    ENDLOOP.

    campos_obligatorios = campos_clave.
  ENDMETHOD.

  METHOD where_clave.
    CLEAR where.
    LOOP AT i_campos_tabla ASSIGNING FIELD-SYMBOL(<campo>) WHERE keyflag = 'X'.
      ASSIGN COMPONENT <campo>-fieldname OF STRUCTURE listado TO FIELD-SYMBOL(<fs>).
      DATA(l_w) = |{ <campo>-fieldname } = '{ <fs> }'|.
      IF where IS INITIAL.
        where = l_w.
      ELSE.
        where = |{ where } AND { l_w }|.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

INITIALIZATION.
  o_prog = NEW #( status       = 'INICIO_DYN'
                  guardar_logz = 'X'
                  status_prog  = 'ZAP_STATUS' ).

  IF sy-batch IS INITIAL.
    o_prog->o_event = NEW #( boton_refrescar = 'X'
                             boton_excel     = 'Y'
                             boton_nuevo     = 'X'
                             boton_borrar    = 'X'
                             o_prog          = o_prog ).

    o_prog->o_alv   = NEW #( estructura = ''
                             o_event    = o_prog->o_event ).

    p_vari = o_prog->o_alv->get_default_layout( ).
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.

  IF NOT o_prog->o_alv IS INITIAL.
    p_vari = o_prog->o_alv->get_f4_layout( ).
  ENDIF.



START-OF-SELECTION.
  o_prog->configuracion( ).
  o_prog->buscar_datos( ).

  IF sy-batch IS INITIAL.
    CALL SCREEN 0100.
  ELSE.
    MESSAGE 'Este programa no se puede ejecutar en fondo'(pnf) TYPE 'E'.
  ENDIF.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*

MODULE status_0100 OUTPUT.

  o_prog->status_dynpro_0100( ).

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  o_prog->command_dynpro_0100( ).


ENDMODULE.
