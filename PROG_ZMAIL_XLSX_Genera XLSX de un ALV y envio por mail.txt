***********************************************************************
* TIPO : LISTADO
* TITULO : Genera XLSX de un ALV y envío por mail
* DESCRIPCION : Genera XLSX de un ALV
*
* AUTOR: Andrés Picazo                                FECHA: 29/10/2024
*
***********************************************************************
REPORT zmail_xlsx.

DATA: v_msg      TYPE bapi_msg,
      is_variant TYPE disvariant,
      layout     TYPE disvariant-variant.

*------PARAMETER/SELECT-OPTIONS EN PANTALLA----------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text-ori.
PARAMETERS: p_progra TYPE programm,
            p_varian TYPE sy-slset,
            p_layout LIKE disvariant-variant.
SELECTION-SCREEN END OF BLOCK b01.
SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE text-des.
PARAMETERS p_visual AS CHECKBOX.
SELECTION-SCREEN SKIP.
PARAMETERS p_fiche AS CHECKBOX USER-COMMAND f.
PARAMETERS p_file TYPE text255 MODIF ID fic.
SELECTION-SCREEN SKIP.
PARAMETERS: p_ftp   AS CHECKBOX USER-COMMAND g,
            p_dftp  TYPE text50 MODIF ID ftp,
            p_user  TYPE text50 MODIF ID ftp,
            p_pass  TYPE text50 MODIF ID ftp,
            p_filef TYPE text255 MODIF ID ftp.
SELECTION-SCREEN SKIP.
PARAMETERS p_mail AS CHECKBOX USER-COMMAND m.
PARAMETERS: p_email  TYPE text255 MODIF ID mai,
            p_asunto TYPE text255 MODIF ID mai,
            p_cuerpo TYPE text255 MODIF ID mai,
            p_filem  TYPE text255 MODIF ID mai.
SELECTION-SCREEN END OF BLOCK b02.
SELECTION-SCREEN BEGIN OF BLOCK opc WITH FRAME TITLE text-opc.
PARAMETERS: p_soloc  TYPE char255 MODIF ID opc,
            p_estru  TYPE tabname MODIF ID opc,
            p_layo_p LIKE disvariant-variant MODIF ID opc.
SELECTION-SCREEN SKIP.
PARAMETERS: p_xlsstd RADIOBUTTON GROUP g DEFAULT 'X' MODIF ID opc,
            p_a2xls  RADIOBUTTON GROUP g MODIF ID opc.

SELECTION-SCREEN END OF BLOCK opc.
PARAMETERS p_noerr TYPE char1 NO-DISPLAY.

INITIALIZATION.
  DATA(o_var) = NEW zcl_ap_variante( ).

AT SELECTION-SCREEN OUTPUT.
  zcl_ap_dynpro=>screen_visible( group1 = 'FIC' variable = p_fiche ).
  zcl_ap_dynpro=>screen_visible( group1 = 'MAI' variable = p_mail ).
  zcl_ap_dynpro=>screen_visible( group1 = 'FTP' variable = p_ftp ).
  zcl_ap_dynpro=>screen_visible( group1 = 'OPC' variable = '' ).

  LOOP AT SCREEN.
    IF screen-name = 'P_PASS'.
      screen-invisible = '1'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

AT SELECTION-SCREEN.
  IF sy-ucomm = 'ONLI'.
    IF p_progra IS INITIAL.
      MESSAGE 'Informe nombre de programa a ajecutar' TYPE 'E'.
    ENDIF.
    IF p_fiche = 'X' AND p_file IS INITIAL.
      MESSAGE 'Informe fichero destino' TYPE 'E'.
    ENDIF.
    IF p_ftp = 'X'.
      IF p_dftp IS INITIAL.
        MESSAGE 'Informe dirección FTP' TYPE 'E'.
      ENDIF.
      IF p_user IS INITIAL.
        MESSAGE 'Informe usuario FTP' TYPE 'E'.
      ENDIF.
    ENDIF.
  ENDIF.

START-OF-SELECTION.
  zcl_ap_utils=>set_runtime_info( ).

  DATA: i_sel           TYPE rsparams_tt,
        l_fichero_final TYPE string.

  IF NOT p_varian IS INITIAL.
    IF o_var->existe( report = p_progra variant = p_varian ).
      o_var->get_contenidos( report = p_progra variant = p_varian ).

      IF NOT p_layout IS INITIAL.
        layout = p_layout.
      ELSE.
        READ TABLE o_var->i_valores ASSIGNING FIELD-SYMBOL(<var>) WITH KEY selname = 'ALV_DEF'.
        IF sy-subrc = 0.
          layout = <var>-low.
        ELSE.
          READ TABLE o_var->i_valores ASSIGNING <var> WITH KEY selname = 'P_VARI'.
          IF sy-subrc = 0.
            layout = <var>-low.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
      MESSAGE |No existe la variante { p_varian }| TYPE 'E'.
    ENDIF.
  ENDIF.

  SUBMIT (p_progra)
    AND RETURN
         WITH SELECTION-TABLE i_sel
         USING SELECTION-SET p_varian.

  IF NOT layout IS INITIAL.
    SELECT SINGLE report, handle, variant
      FROM ltdx
      INTO CORRESPONDING FIELDS OF @is_variant
      WHERE relid = 'LT'
        AND report = @p_progra
        AND variant = @layout.
  ENDIF.

  DATA lt_data_ref TYPE REF TO data.

  FIELD-SYMBOLS <tabla> TYPE STANDARD TABLE.

  TRY.
      cl_salv_bs_runtime_info=>get_data_ref(
        IMPORTING
          r_data            = lt_data_ref ).
    CATCH cx_salv_bs_sc_runtime_info.
      IF p_noerr IS INITIAL.
        v_msg = |Error recuperando datos de report { p_progra } { p_varian }|.
        MESSAGE v_msg TYPE 'E'.
      ELSE.
        LEAVE PROGRAM.
      ENDIF.
  ENDTRY.

  cl_salv_bs_runtime_info=>clear_all( ).
  ASSIGN lt_data_ref->* TO <tabla>.
  IF sy-subrc <> 0.
    IF p_noerr IS INITIAL.
      v_msg = |Error recuperando datos de ALV { p_progra } { p_varian }|.
      MESSAGE v_msg TYPE 'E'.
    ELSE.
      LEAVE PROGRAM.
    ENDIF.
  ENDIF.

  IF NOT p_soloc IS INITIAL.
    DATA: datos  TYPE REF TO data,
          datos2 TYPE REF TO data.
    FIELD-SYMBOLS: <datos>  TYPE STANDARD TABLE,
                   <datos2> TYPE STANDARD TABLE.

    zcl_ap_fs=>crea_subtabla( EXPORTING tabla_principal = <tabla>
      campos = p_soloc
      estructura = p_estru
                              IMPORTING subtabla = datos ).
    ASSIGN datos->* TO <datos>.
    zcl_ap_fs=>crea_subtabla( EXPORTING tabla_principal = <datos>
      campos = p_soloc
      estructura = p_estru
                              IMPORTING subtabla = datos2 ).

    ASSIGN datos2->* TO <datos2>.

    CLEAR <datos2>.
    LOOP AT <datos> ASSIGNING FIELD-SYMBOL(<d>).
      COLLECT <d> INTO <datos2>.
    ENDLOOP.

    ASSIGN <datos2> TO <tabla>.
  ENDIF.

  DATA(o_alv) = NEW zcl_ap_alv( tabla = '' ).

  IF NOT p_layo_p IS INITIAL.
    o_alv->constructor_tabla(
      EXPORTING
        cprog = |ZXLSX_{ p_progra }{ p_estru }|
        handle = 'ZZZZ'
       CHANGING t_tabla = <tabla> ).

    IF NOT p_layout IS INITIAL.
      o_alv->set_layout( p_layo_p ).
    ENDIF.
  ELSE.
    o_alv->constructor_tabla(
      EXPORTING
        cprog = p_progra
        handle = is_variant-handle
       CHANGING t_tabla = <tabla> ).

    IF NOT is_variant-variant IS INITIAL.
      o_alv->set_layout( is_variant-variant ).
    ENDIF.
  ENDIF.

  IF p_visual = 'X'.
    o_alv->show( ).
  ENDIF.

  IF p_fiche = 'X' OR p_mail = 'X' OR p_ftp = 'X'.
    IF p_a2xls = 'X'.
      DATA(l_xstring) = zcl_ap_abap2xls=>alv_2_xls( alv = o_alv->o_alv tabla = <tabla> huge = 'X' refresh_metadata = 'X' ).
    ELSE.
      l_xstring = o_alv->o_alv->to_xml( xml_type = if_salv_bs_xml=>c_type_xlsx ).
    ENDIF.

    IF p_fiche = 'X'.
      zcl_ap_ficheros=>grabar_xstring( EXPORTING xstring = l_xstring fichero = p_file mostrar_error = ''
                                       IMPORTING mensaje = v_msg
                                                 fichero_final = l_fichero_final ).
      IF NOT v_msg IS INITIAL.
        MESSAGE v_msg TYPE 'I'.
      ELSE.
        MESSAGE |Se ha grabado el fichero { l_fichero_final }| TYPE 'S'.
      ENDIF.
    ENDIF.

    IF p_mail = 'X'.
      DATA(l_filem) = zcl_ap_ficheros=>get_fichero_var( fichero = CONV #( p_filem ) ).
      IF l_filem IS INITIAL.
        l_filem = 'fichero.xlsx'.
      ENDIF.
      zcl_ap_envio_mail=>mail( EXPORTING subject              = p_asunto
                                         direccion            = p_email
                                         nombre_fichero_tabla = l_filem
                                         texto                = p_cuerpo
                                         xstring              = l_xstring
                               IMPORTING message              = v_msg ).
      IF v_msg IS INITIAL.
        MESSAGE |Se ha enviado mail a { p_email }| TYPE 'S'.
      ELSE.
        MESSAGE v_msg TYPE 'I'.
      ENDIF.
    ENDIF.

    IF p_ftp = 'X'.
      NEW zcl_ap_ftp( )->grabar_fichero( EXPORTING user     = p_user
                                                   password = p_pass
                                                   host     = p_dftp
                                                   fichero  = p_filef
                                                   xstring  = l_xstring
                                         IMPORTING message = DATA(l_msg)
                                                   fichero_final = l_fichero_final ).
      IF NOT l_msg IS INITIAL.
        MESSAGE l_msg TYPE 'I'.
      ELSE.
        MESSAGE |Fichero { l_fichero_final } grabado en FTP| TYPE 'S'.
      ENDIF.
    ENDIF.
  ENDIF.
