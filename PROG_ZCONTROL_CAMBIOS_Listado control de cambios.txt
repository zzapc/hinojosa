***********************************************************************
* TIPO : LISTADO
* TITULO : Listado Control de cambios
* DESCRIPCION : Listado Control de cambios
*
* AUTOR: Andrés Picazo                                FECHA: 04/06/2014
*
***********************************************************************
REPORT zcontrol_cambios.

*------INCLUDES--------------------------------------------------------*

*------TABLAS/ESTRUCTURAS----------------------------------------------*
TABLES: cdhdr, cdpos, sscrfields, zcdpos_coment.

*------TABLAS INTERNAS-------------------------------------------------*
TYPES: BEGIN OF t_listado,
         check,
         objectid    LIKE cdhdr-objectid,
         udate       LIKE cdhdr-udate,
         utime       LIKE cdhdr-utime,
         username    LIKE cdhdr-username,
         tcode       LIKE cdhdr-tcode,
         changenr    TYPE cdpos-changenr,
         tabname     LIKE cdpos-tabname,
         tabkey      LIKE cdpos-tabkey,
         fname       LIKE cdpos-fname,
         chngind     TYPE cdpos-chngind,
         ddtext      TYPE dd04t-ddtext,
         value_old   LIKE cdpos-value_old,
         value_new   LIKE cdpos-value_new,
         comentarios TYPE sstn_comments,
         tabix       TYPE sy-tabix,
       END OF t_listado.
DATA: i_listado TYPE TABLE OF t_listado,
      l_listado TYPE t_listado.

FIELD-SYMBOLS <listado> TYPE t_listado.

*----------------------------------------------------------------------*
* CLASS lcl_alv DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_alv DEFINITION INHERITING FROM zcl_ap_alv_check.
  PUBLIC SECTION.
    METHODS: visualizar_objeto REDEFINITION.
    METHODS: handle_user_command REDEFINITION.
    METHODS: top_of_page REDEFINITION.
ENDCLASS. "lcl_alv DEFINITION

*----------------------------------------------------------------------*
*       CLASS zcl_report DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS zcl_report DEFINITION INHERITING FROM zcl_ap_dev.
  PUBLIC SECTION.
    METHODS: main.

    METHODS:  listado,
      seleccionar_datos.

ENDCLASS.                    "REPORT DEFINITION


*------VARIABLES-------------------------------------------------------*
DATA: o_prog TYPE REF TO zcl_report,
      o_alv  TYPE REF TO lcl_alv.

*------PARAMETER/SELECT-OPTIONS EN PANTALLA----------------------------*
SELECTION-SCREEN BEGIN OF BLOCK 001 WITH FRAME.
  PARAMETERS: p_object LIKE cdhdr-objectclas OBLIGATORY.
  SELECT-OPTIONS: s_id FOR cdhdr-objectid,
                  s_user FOR cdhdr-username,
                  s_udate FOR cdhdr-udate,
                  s_utime FOR cdhdr-utime,
                  s_table FOR cdpos-tabname,
                  s_fname FOR cdpos-fname,
                  s_tcode FOR cdhdr-tcode.

  PARAMETERS: p_nocamp TYPE text255 NO-DISPLAY.
  SELECTION-SCREEN: SKIP 1.
  PARAMETERS: p_vari LIKE disvariant-variant.
SELECTION-SCREEN END OF BLOCK 001.
SELECTION-SCREEN: FUNCTION KEY 2,
FUNCTION KEY 3,
FUNCTION KEY 4.


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
    DATA: l_list        TYPE t_listado,
          zcdpos_coment TYPE zcdpos_coment.

    l_list = list.
    CASE column.
      WHEN 'COMENTARIOS'.
        o_prog->string = l_list-comentarios.
        DATA mod TYPE xfeld.
        zcl_ap_string=>popup_texto( EXPORTING editar = 'X' IMPORTING modificado = mod CHANGING texto = o_prog->string ).
        IF mod = 'X'.
          i_listado[ tabix = l_list-tabix ]-comentarios = o_prog->string.
          l_list-comentarios = o_prog->string.

          MOVE-CORRESPONDING l_list TO zcdpos_coment.
          zcdpos_coment-objectclas = p_object.
          MODIFY zcdpos_coment FROM zcdpos_coment.
          o_alv->refresh( ).
        ENDIF.
      WHEN OTHERS. message = 'No implementado'.
    ENDCASE.
  ENDMETHOD. "handle_double_click

  METHOD handle_user_command.
    DATA: l_row TYPE i.


    super->handle_user_command( e_salv_function ).


    CASE ucomm.
      WHEN 'EXCEL'.
        exportar_excel( ).

      WHEN 'COMENT'.
        DATA l_return TYPE xfeld.
        CLEAR zcdpos_coment.
        zcl_ap_popup=>popup_usuario( EXPORTING campo1 = 'ZCDPOS_COMENT-COMENTARIOS'
                                       titulo = 'Introduzca justificación'
                                     IMPORTING  return = l_return
                                      CHANGING  valor1 = zcdpos_coment-comentarios ).
        IF l_return NE 'A' AND zcdpos_coment-comentarios NE ''.
          LOOP AT i_listado ASSIGNING FIELD-SYMBOL(<listado>).
            <listado>-comentarios = zcdpos_coment-comentarios.
            MOVE-CORRESPONDING <listado> TO zcdpos_coment.
            zcdpos_coment-objectclas = p_object.
            MODIFY zcdpos_coment FROM zcdpos_coment.
          ENDLOOP.
          o_alv->refresh( ).
        ENDIF.

      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD. "handle_USER_COMMAND
  METHOD top_of_page.
    CLEAR o_content.

    o_top_page->set_titulo( sy-title ).

    o_top_page->add_rango_auto( ).
*    o_top_page->add_rango( texto = 'Centro' rango = s_vbeln[] ).
    o_top_page->crea_info_seleccion( ).

    o_content = o_top_page->get_grid( ).

  ENDMETHOD.                    "top_of_page
ENDCLASS. "lcl_alv IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS zcl_report IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS zcl_report IMPLEMENTATION.
  METHOD main.
    seleccionar_datos( ).
    listado( ).
  ENDMETHOD.                    "REPORT

  METHOD seleccionar_datos.
    DATA: i_cdhdr             TYPE TABLE OF cdhdr,
          i_cdpos             TYPE TABLE OF cdpos,
          l_tabla_comentarios TYPE tabname.

    FIELD-SYMBOLS: <cdpos> TYPE cdpos,
                   <cdhdr> TYPE cdhdr.

    SELECT SINGLE tabname FROM dd02l
      INTO l_tabla_comentarios
     WHERE tabname = 'ZCDPOS_COMENT'.

    sgpi_texto( 'Seleccionando datos' ).

    SELECT * FROM cdhdr
      INTO TABLE i_cdhdr
     WHERE objectclas = p_object
       AND objectid   IN s_id
       AND username   IN s_user
       AND udate      IN s_udate
       AND utime      IN s_utime
       AND tcode      IN s_tcode.

    IF sy-subrc = 0.
      SELECT * FROM  cdpos                         "#EC CI_NO_TRANSFORM
        INTO TABLE i_cdpos
        FOR ALL ENTRIES IN i_cdhdr
             WHERE  objectclas  = i_cdhdr-objectclas
             AND    objectid    = i_cdhdr-objectid
             AND    changenr    = i_cdhdr-changenr
             AND    tabname     IN s_table
             AND    fname       IN s_fname.

      LOOP AT i_cdpos ASSIGNING <cdpos>.
        READ TABLE i_cdhdr ASSIGNING <cdhdr> WITH KEY changenr = <cdpos>-changenr.
        IF sy-subrc = 0.
          CLEAR l_listado.
          MOVE-CORRESPONDING <cdhdr> TO l_listado.
          MOVE-CORRESPONDING <cdpos> TO l_listado.
          IF NOT <cdpos>-fname IS INITIAL.
            l_listado-ddtext = zcl_ap_dev=>get_descripcion_campo( campo = <cdpos>-fname tabla = <cdpos>-tabname ).
          ENDIF.

          IF NOT l_tabla_comentarios IS INITIAL.
            SELECT SINGLE comentarios FROM (l_tabla_comentarios)
              INTO l_listado-comentarios
             WHERE objectclas = <cdpos>-objectclas
               AND objectid   = <cdpos>-objectid
               AND changenr   = <cdpos>-changenr
               AND tabname    = <cdpos>-tabname
               AND tabkey     = <cdpos>-tabkey
               AND fname      = <cdpos>-fname
               AND chngind    = <cdpos>-chngind.
          ENDIF.

          APPEND l_listado TO i_listado.
        ENDIF.
      ENDLOOP.

      LOOP AT i_listado ASSIGNING FIELD-SYMBOL(<listado>).
        <listado>-tabix = sy-tabix.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.                    "seleccionar_datos


  METHOD listado.

    sgpi_texto( 'Generando informe' ).

    o_alv->set_layout( p_vari ).

*    o_alv->set_top_of_page( ).


    o_alv->set_seleccion( CHANGING t_tabla = i_listado ).

    o_alv->set_field_quitar( 'TABIX' ).
    o_alv->set_field_noout( 'CHANGENR' ).
    SELECT SINGLE tabname FROM dd02l
      INTO @DATA(l_tabla_comentarios)
     WHERE tabname = 'ZCDPOS_COMENT'.
    IF sy-subrc NE 0.
      o_alv->set_field_quitar( 'COMENTARIOS' ).
    ELSE.
      o_alv->add_button( button = 'F01' text = 'Comentario para todo'  icon = icon_change ucomm = 'COMENT' ).
      o_alv->set_field_hotspot( campo = 'COMENTARIOS' valor = 'TEXT_EDT' ).
    ENDIF.

    IF NOT p_nocamp IS INITIAL.
      o_alv->set_field_noout( p_nocamp ).
    ENDIF.

    o_alv->set_orden( 'OBJECTID,UDATE,UTIME' ).
    o_alv->show( ).


  ENDMETHOD.                    "

ENDCLASS.                    "REPORT IMPLEMENTATION

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.

  CREATE OBJECT o_prog
    EXPORTING
      status = ''.

  CREATE OBJECT o_alv
    EXPORTING
      status      = 'STANDARD_ALV_DYN'
      status_prog = 'ZAP_STATUS'.
  p_vari = o_alv->get_default_layout( ).


  o_prog->initialization( CHANGING sscrfields = sscrfields ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.

  p_vari = o_alv->get_f4_layout( ).

************************************************************************
* AT SELECTION-SCREEN.
************************************************************************
AT SELECTION-SCREEN.

  o_prog->at_selection(  ).

*----------------------------------------------------------------------
* START-OF-SELECTION.
*----------------------------------------------------------------------*
START-OF-SELECTION.

  o_prog->main( ).
