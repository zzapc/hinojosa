***********************************************************************
* APLICACION : BC
* TIPO : LISTADO
* TITULO : Parametrización tablas Z
* *
* DESCRIPCION : Parametrización tablas Z
*
*
* AUTOR: Andrés Picazo                                FECHA: 19/05/2011
* ANALISTA: Andrés Picazo
*
* MODIFICACIONES
*
***********************************************************************
REPORT  zspro.

TABLES: zspro, *zspro.
DATA: i_listado TYPE TABLE OF zspro,
      i_spro    TYPE TABLE OF zspro.

TYPES: BEGIN OF t_tablas,
         key         TYPE i,
         tabla       TYPE zspro-tabla,
         clave_param TYPE zspro-clave_param,
         programa    TYPE zspro-programa,
         tcode       TYPE zspro-tcode,
         parid       TYPE zspro-parid,
         parva       TYPE zspro-parva,
         inmed       TYPE zspro-inmed,
         url         TYPE zspro-url,
         comentario  TYPE zspro-comentario,
       END OF t_tablas.
DATA: i_tablas TYPE TABLE OF t_tablas,
      l_tabla  TYPE t_tablas.
FIELD-SYMBOLS <zspro> TYPE zspro.


PARAMETERS: p_clave  LIKE zspro-clave,
            p_filtro TYPE repid NO-DISPLAY.

*----------------------------------------------------------------------*
*       CLASS lcl_alv DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_alv_tree DEFINITION INHERITING FROM zcl_ap_alv_tree.
  PUBLIC SECTION.
    METHODS: handle_double_click REDEFINITION.
    METHODS: handle_user_command REDEFINITION.
ENDCLASS.                    "lcl_alv DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_alv IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_alv_tree IMPLEMENTATION.
  METHOD handle_double_click.

    READ TABLE i_tablas INTO l_tabla WITH KEY key = node_key.
    IF sy-subrc = 0.
      IF NOT l_tabla-parid IS INITIAL.
        SET PARAMETER ID l_tabla-parid FIELD l_tabla-parva.
      ENDIF.
      IF l_tabla-tabla = 'ZPARAMETROS' AND NOT l_tabla-clave_param IS INITIAL.
        zcl_ap_parametros=>mantenimiento_st( l_tabla-clave_param ).
      ELSEIF l_tabla-tabla = 'ZDOCUMENTOS'.
        zcl_ap_documentos=>visualizar_documento( tcode = l_tabla-programa
                                                 nombre = l_tabla-url ).
      ELSEIF NOT l_tabla-tabla IS INITIAL.
        zcl_ap_utils=>mantener_tabla( tabla = l_tabla-tabla se16 = 'X' ).
      ELSEIF NOT l_tabla-tcode IS INITIAL.
        IF l_tabla-inmed IS INITIAL.
          CALL TRANSACTION l_tabla-tcode.
        ELSE.
          CALL TRANSACTION l_tabla-tcode AND SKIP FIRST SCREEN.
        ENDIF.
      ELSEIF NOT l_tabla-programa IS INITIAL.
        IF l_tabla-inmed IS INITIAL.
          SUBMIT (l_tabla-programa)
            VIA SELECTION-SCREEN
            AND RETURN.
        ELSE.
          SUBMIT (l_tabla-programa)
            AND RETURN.
        ENDIF.
      ELSEIF NOT l_tabla-url IS INITIAL.
        zcl_ap_gos=>visualizar_fichero_st( l_tabla-url ).
      ENDIF.
    ENDIF.
  ENDMETHOD.                    "handle_double_click
  METHOD handle_user_command.
    CASE e_salv_function.
      WHEN 'BUSCAR'.
        PERFORM buscar.
      WHEN 'PARAM'.
        zcl_ap_utils=>mantener_tabla( 'ZSPRO' ).
    ENDCASE.
  ENDMETHOD.                    "handle_USER_COMMAND

ENDCLASS.                    "lcl_alv IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS REPORT DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS report DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS main.
  PRIVATE SECTION.
    CLASS-DATA:
      o_alv TYPE REF TO lcl_alv_tree.

    CLASS-METHODS: recuperar_datos,
      listado.
ENDCLASS.                    "REPORT DEFINITION


*----------------------------------------------------------------------*
*       CLASS REPORT IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS report IMPLEMENTATION.
  METHOD main.
    listado( ).
  ENDMETHOD.                    "REPORT

  METHOD recuperar_datos.

    IF p_clave IS INITIAL.
      SELECT * FROM zspro                               "#EC CI_NOWHERE
        INTO TABLE i_spro.
    ELSE.
      SELECT * FROM zspro
        INTO TABLE i_spro
       WHERE clave = p_clave.
    ENDIF.

    IF NOT p_filtro IS INITIAL.
      LOOP AT i_spro ASSIGNING <zspro>.
        TRANSLATE <zspro> TO UPPER CASE.
        IF NOT <zspro> CS p_filtro.
          DELETE i_spro.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.                    "REPORT

  METHOD listado.
    DATA l_new.

    CREATE OBJECT o_alv
      EXPORTING
        tabla = ''.

    o_alv->constructor_tabla( EXPORTING  optimize = 'X'
                                         status   = 'STANDARD'
                              CHANGING t_tabla    = i_listado ).

    recuperar_datos( ).

    DATA: l_salida TYPE lvc_nkey,
          l_modulo TYPE lvc_nkey.

    SORT i_spro.
    LOOP AT i_spro INTO zspro.
      AT NEW modulo.
        CLEAR *zspro.
        *zspro-modulo = zspro-modulo.
        l_modulo = o_alv->add_nodo( registro = *zspro
                                    key      = ''
                                    text = zspro-modulo ).
      ENDAT.

      AT NEW grupo.
        CLEAR *zspro.
        *zspro-grupo = zspro-grupo.
        l_salida = o_alv->add_nodo( registro = *zspro
                                    key      = l_modulo
                                    text = |{ zspro-grupo }| ).
      ENDAT.

      CLEAR: zspro-modulo, zspro-grupo.
      l_tabla-key = o_alv->add_nodo( registro = zspro
                                     key      = l_salida
                                     text = |{ zspro-nombre_tabla }| ).

      l_tabla-tabla = zspro-tabla.
      l_tabla-clave_param = zspro-clave_param.
      l_tabla-programa = zspro-programa.
      l_tabla-url = zspro-url.
      l_tabla-tcode = zspro-tcode.
      l_tabla-parid = zspro-parid.
      l_tabla-parva = zspro-parva.
      l_tabla-inmed = zspro-inmed.
      l_tabla-comentario = zspro-comentario.
      APPEND l_tabla TO i_tablas.
    ENDLOOP.

    o_alv->set_field( campo = 'MANDT,MODULO,GRUPO,NOMBRE_TABLA' op = 'NOOUT' ).

    IF NOT p_clave IS INITIAL.
      o_alv->set_field( campo = 'CLAVE' op = 'NOOUT' ).
    ENDIF.

    DESCRIBE TABLE i_tablas LINES sy-tfill.
    IF NOT p_filtro IS INITIAL OR sy-tfill < 20.
      o_alv->expand_all( ).
    ENDIF.
    o_alv->show(  ).
  ENDMETHOD.                    "

ENDCLASS.                    "REPORT IMPLEMENTATION


START-OF-SELECTION.

  report=>main( ).
*&---------------------------------------------------------------------*
*&      Form  BUSCAR
*&---------------------------------------------------------------------*
FORM buscar .
  DATA: l_return,
        l_buscar TYPE repid.

  zcl_ap_popup=>popup_usuario( EXPORTING campo1 = 'ZSPRO-PROGRAMA'
                                         titulo = 'Concepto de búsqueda'
                               IMPORTING return = l_return
                               CHANGING  valor1 = l_buscar ).

  IF l_return IS INITIAL.
    SUBMIT zspro WITH p_filtro = l_buscar.
  ENDIF.

ENDFORM.                    " BUSCAR
