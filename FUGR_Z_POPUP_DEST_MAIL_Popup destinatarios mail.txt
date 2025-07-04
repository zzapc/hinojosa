FUNCTION Z_POPUP_DEST_MAIL.
*"--------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     REFERENCE(TITULO) TYPE  ANY DEFAULT 'Direcciones de mail'
*"     REFERENCE(VISUALIZAR) TYPE  ABAP_BOOL DEFAULT ''
*"  EXPORTING
*"     REFERENCE(CANCELADO) TYPE  CHAR1
*"  CHANGING
*"     REFERENCE(MAILS) TYPE  ANY
*"     REFERENCE(I_MAILS) TYPE  ZEST_SEL_MAIL_T OPTIONAL
*"--------------------------------------------------------------------
v_titulo = titulo.
  v_visualizar = visualizar.

  IF NOT o_grid_mails IS INITIAL.
    o_grid_mails->free( ).
    CLEAR o_grid_mails.
  ENDIF.

  IF NOT mails IS INITIAL.
    CLEAR i_mails.
    SPLIT mails AT ',' INTO TABLE DATA(i_m).
    LOOP AT i_m ASSIGNING FIELD-SYMBOL(<m>).
      APPEND VALUE #( email = <m> ) TO i_mails.
    ENDLOOP.
  ENDIF.
  DELETE i_mails WHERE email = ''.

  IF visualizar IS INITIAL.
    DO 100 TIMES.
      APPEND VALUE #( ) TO i_mails.
    ENDDO.
  ENDIF.

  i_mails_f = i_mails.

  CALL SCREEN 0300 STARTING AT 3 3 ENDING AT 136 13.

  IF sy-ucomm = 'CANCEL'.
    cancelado = 'X'.
  ELSE.
    i_mails = i_mails_f.
    DELETE i_mails WHERE email = ''.
    CLEAR mails.
    LOOP AT i_mails ASSIGNING FIELD-SYMBOL(<mail>).
      IF mails IS INITIAL.
        mails = <mail>.
      ELSE.
        mails = mails && `,` && <mail>-email.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF NOT o_grid_mails IS INITIAL.
    o_grid_mails->free( ).
    CLEAR o_grid_mails.
  ENDIF.





ENDFUNCTION.
