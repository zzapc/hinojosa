FUNCTION Z_POPUP_ALV.
*"--------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     REFERENCE(TITULO) OPTIONAL
*"     REFERENCE(TEXTO) OPTIONAL
*"     REFERENCE(TEXTO2) OPTIONAL
*"     REFERENCE(CHECK) DEFAULT ''
*"     REFERENCE(ANCHO) TYPE  I DEFAULT 120
*"     REFERENCE(ALTO) TYPE  I DEFAULT 0
*"     REFERENCE(OPCION_BORRAR) DEFAULT ''
*"     REFERENCE(SOLO_ACEPTAR) DEFAULT ''
*"     REFERENCE(SELECCIONAR_FILA) DEFAULT ''
*"     REFERENCE(BOTON_IMPRIMIR) DEFAULT ''
*"     REFERENCE(CAMPOS_NOOUT) TYPE  STRING DEFAULT ''
*"     REFERENCE(TOOLBAR) DEFAULT ''
*"  EXPORTING
*"     REFERENCE(OK) TYPE  CHAR1
*"     REFERENCE(FILA) TYPE  I
*"     REFERENCE(UCOMM) TYPE  SY-UCOMM
*"  TABLES
*"      T_DATOS TYPE  TABLE OPTIONAL
*"--------------------------------------------------------------------
DATA: l_alto  TYPE i,
        l_ancho TYPE i.

  CLEAR v_inicio .
  v_texto = texto.
  v_titulo = titulo.
  IF texto IS INITIAL.
    v_texto = titulo.
  ENDIF.
  IF titulo IS INITIAL.
    v_titulo = texto.
  ENDIF.
  v_texto2 = texto2.
  v_check = check.
  v_opcion_borrar = opcion_borrar.
  v_solo_aceptar = solo_aceptar.
  v_seleccionar_fila = seleccionar_fila.
  v_boton_imprimir = boton_imprimir.
  v_toolbar = toolbar.

  IF NOT ancho IS INITIAL.
    v_max_ancho = ancho.
  ENDIF.
  l_ancho = strlen( v_titulo ).
  IF l_ancho > v_max_ancho.
    v_max_ancho = l_ancho.
  ENDIF.
  l_ancho = strlen( v_texto ).
  IF l_ancho > v_max_ancho.
    v_max_ancho = l_ancho.
  ENDIF.
  ADD 3 TO v_max_ancho.

  IF v_max_ancho > 120.
    v_max_ancho = 120.
  ENDIF.

  DESCRIBE TABLE t_datos LINES l_alto.
  IF l_alto = 0.
    l_alto = 5.
  ELSEIF l_alto < 20.
    ADD 5 TO l_alto.
  ENDIF.
  IF l_alto > 27.
    l_alto = 27.
  ENDIF.
  IF toolbar = 'X'.
    ADD 2 TO l_alto.
  ENDIF.

  v_campos_noout = campos_noout.

  ASSIGN ('T_DATOS[]') TO <tabla>.

  IF NOT o_alv_popup IS INITIAL.
    o_alv_popup->free( ).
    CLEAR o_alv_popup.
  ENDIF.

  v_max_ancho_txt = v_max_ancho - 3.
  CALL SCREEN 0600 STARTING AT 3 3 ENDING AT v_max_ancho l_alto.

  ok = v_ok.
  fila = v_row.
  ucomm = v_ucomm.





ENDFUNCTION.
