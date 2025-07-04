FUNCTION Z_POPUP_MAIL.
*"--------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     REFERENCE(MOSTRAR_EMISOR) DEFAULT ' '
*"     REFERENCE(MOSTRAR_DESTINATARIO) DEFAULT 'X'
*"     REFERENCE(MOSTRAR_CCO) DEFAULT ''
*"     REFERENCE(MOSTRAR_COPIA) DEFAULT ''
*"     REFERENCE(ENVIAR_MAIL) DEFAULT 'X'
*"     REFERENCE(LISTA_DISTRIBUCION) DEFAULT ''
*"     REFERENCE(I_ADJUNTOS) TYPE  RMPS_T_POST_CONTENT OPTIONAL
*"     REFERENCE(NO_MOSTRAR_POPUP) DEFAULT ''
*"     REFERENCE(PROCESO_LOG) DEFAULT ''
*"     REFERENCE(TITULO_POPUP) DEFAULT ''
*"     REFERENCE(HTML) DEFAULT ''
*"     REFERENCE(BOTON_ADJUNTOS) DEFAULT ''
*"     REFERENCE(GOS) TYPE  SIBFTYPEID DEFAULT ''
*"     REFERENCE(CLAVE_GOS) DEFAULT ''
*"     REFERENCE(AR_OBJECT) TYPE  TOA01-AR_OBJECT DEFAULT ''
*"     REFERENCE(CLAVE) DEFAULT ''
*"     REFERENCE(PLANTILLA) TYPE  ZAP_TEXTOS_MAIL OPTIONAL
*"  EXPORTING
*"     REFERENCE(MESSAGE) TYPE  BAPI_MSG
*"  TABLES
*"      I_TEXTO STRUCTURE  SOLISTI1 OPTIONAL
*"  CHANGING
*"     REFERENCE(SUBJECT) TYPE  ANY OPTIONAL
*"     REFERENCE(DIRECCION) TYPE  ANY OPTIONAL
*"     REFERENCE(EMISOR) TYPE  ANY OPTIONAL
*"     REFERENCE(TEXTO) TYPE  ANY OPTIONAL
*"     REFERENCE(COPIA) TYPE  ANY OPTIONAL
*"     REFERENCE(CCO) TYPE  ANY OPTIONAL
*"  EXCEPTIONS
*"      ENVIO_CANCELADO
*"--------------------------------------------------------------------
DATA: l_msg    TYPE string,
        l_altura TYPE i VALUE 23.

  v_asunto = subject.
  v_emisor = emisor.
  v_copia  = copia.
  v_cco = cco.
  v_destinatario = direccion.
  v_texto = texto.

  v_mostrar_emisor = mostrar_emisor.
  v_mostrar_destinatario = mostrar_destinatario.
  v_mostrar_copia = mostrar_copia.
  v_mostrar_cco = mostrar_cco.
  IF mostrar_copia = 'X' AND strlen( copia ) > 512.
    v_mostrar_copia = 'R'.
  ENDIF.
  v_enviar_mail = enviar_mail.
  v_lista_distribucion = lista_distribucion.
  v_gos = gos.
  v_clave = clave.
  v_plantilla = plantilla.

  FIELD-SYMBOLS <adj> TYPE rmps_post_content.
  DATA l_v_adj TYPE t_adjuntos.
  CLEAR v_i_adjuntos.
  LOOP AT i_adjuntos ASSIGNING <adj>.
    CLEAR l_v_adj.
    MOVE-CORRESPONDING <adj> TO l_v_adj.
    APPEND l_v_adj TO v_i_adjuntos.
  ENDLOOP.

  CLEAR v_i_gos.
  IF NOT v_gos IS INITIAL.
    IF ar_object IS INITIAL.
      DATA: i_adj TYPE rmps_t_post_content.
      zcl_ap_gos=>get_file_list( EXPORTING typeid = v_gos instid = clave_gos get_adj_mail = 'X' CHANGING i_adjuntos_mail = i_adj ).
      LOOP AT i_adj ASSIGNING <adj>.
        CLEAR l_v_adj.
        MOVE-CORRESPONDING <adj> TO l_v_adj.
        IF l_v_adj-filename IS INITIAL.
          l_v_adj-filename = l_v_adj-subject.
        ENDIF.
        APPEND l_v_adj TO v_i_gos.
      ENDLOOP.
    ELSE.
      DATA: i_docs TYPE zal_files_m_t,
            l_adj  TYPE rmps_post_content.
      FIELD-SYMBOLS <doc> TYPE zal_files_m.
      IF ar_object = '*'.
        i_docs = zcl_ap_gos=>get_archivelink_files_m( sap_object = v_gos object_id = clave_gos ar_object = '' get_contenido = 'X' ).
      ELSE.
        i_docs = zcl_ap_gos=>get_archivelink_files_m( sap_object = v_gos object_id = clave_gos ar_object = ar_object get_contenido = 'X' ).
      ENDIF.
      LOOP AT i_docs ASSIGNING <doc>.
        l_adj = NEW zcl_ap_envio_mail( )->add_adjunto( titulo = <doc>-fichero
                                                    xstring = <doc>-contenido ).
        MOVE-CORRESPONDING l_adj TO l_v_adj.
        IF l_v_adj-filename IS INITIAL.
          l_v_adj-filename = l_v_adj-subject.
        ENDIF.
        APPEND l_v_adj TO v_i_gos.
      ENDLOOP.
    ENDIF.
  ENDIF.

  v_proceso_log = proceso_log.
  v_titulo_popup = titulo_popup.
  v_html = html.
  IF boton_adjuntos = 'X' OR NOT i_adjuntos IS INITIAL.
    v_boton_adjuntos = 'X'.
  ENDIF.

  i_textos[] = i_texto[].

  CLEAR: v_cancel.

  CLEAR o_alv_mail.

  CREATE OBJECT o_alv_mail
    EXPORTING
      tabla = ''.

  IF no_mostrar_popup = 'X'.
    IF NOT v_proceso_log IS INITIAL.
      zcl_ap_log=>set_log( proceso = v_proceso_log
                           p1 = 'Se envía mail' p2 = v_asunto p3 = 'a' p4 = v_destinatario
                           msgty = 'S' ).
    ENDIF.

    zcl_ap_envio_mail=>mail( EXPORTING subject   = v_asunto
                                       direccion = v_destinatario
                                       lista_distribucion = v_lista_distribucion
                                       dest_copia = v_copia
                                       dest_copia_oculta = v_cco
                                       emisor     = v_emisor
                                       i_adjuntos = i_adjuntos
                                       texto     = v_texto
                                       i_textos  = i_textos[]
                                       html      = v_html
                                       clave     = v_clave
                                       plantilla = v_plantilla
                             IMPORTING message   = v_message ).
    IF v_message IS INITIAL.
      MESSAGE 'Se ha enviado el mail' TYPE 'S'.
    ELSE.
      CONCATENATE 'Error enviando el mail' v_message INTO l_msg SEPARATED BY space.
      MESSAGE l_msg TYPE 'I'.
    ENDIF.
  ELSE.
    IF NOT o_texto IS INITIAL..
      o_texto->destroy( ).
      CLEAR o_texto.
    ENDIF.

    CLEAR v_info.
    IF NOT v_i_adjuntos[] IS INITIAL.
      DESCRIBE TABLE v_i_adjuntos LINES sy-tfill.
      __concat2 v_info 'Nº de adjuntos' sy-tfill.

      IF v_boton_adjuntos = 'X'.
        CONCATENATE icon_attachment v_info INTO v_info SEPARATED BY space.
        zcl_ap_dynpro=>modificar_texto( campo = 'BADJ' texto = v_info repid = sy-repid dynnr = '2000' ancho = 30 ).
        CLEAR v_info.
      ENDIF.
    ELSE.
      zcl_ap_dynpro=>modificar_texto( campo = 'BADJ' texto = 'Sin adjuntos' repid = sy-repid dynnr = '2000' ancho = 30 ).
    ENDIF.

    DATA l_buttons           TYPE zcl_ap_alv=>t_buttons.
    PERFORM get_boton IN PROGRAM zap_status CHANGING l_buttons.
    PERFORM clear_botones IN PROGRAM zap_status.
    IF v_mostrar_emisor = 'X'. ADD 3 TO l_altura. ENDIF.
    IF v_mostrar_destinatario = 'X'. ADD 3 TO l_altura. ENDIF.
    IF v_mostrar_copia = 'X'. ADD 3 TO l_altura. ENDIF.
    IF v_mostrar_cco = 'X'. ADD 3 TO l_altura. ENDIF.
    CALL SCREEN 2000 STARTING AT 1 1 ENDING AT 120 l_altura.
    PERFORM set_boton IN PROGRAM zap_status USING l_buttons.


    IF NOT o_texto IS INITIAL.
      o_texto->destroy( ).
      CLEAR o_texto.
    ENDIF.

    IF v_cancel IS INITIAL.
      texto     = v_texto.
      subject   = v_asunto.
      direccion = v_destinatario.
      emisor    = v_emisor.
      message   = v_message.
      copia     = v_copia.
    ELSE.
      IF NOT v_proceso_log IS INITIAL.
        zcl_ap_log=>set_log( proceso = v_proceso_log
                             p1 = 'Se cancela envío del mail' p2 = v_asunto p3 = 'a' p4 = v_destinatario
                             msgty = 'E' ).
      ENDIF.

      RAISE envio_cancelado.
    ENDIF.
  ENDIF.





ENDFUNCTION.
