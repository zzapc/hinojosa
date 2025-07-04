FUNCTION z_rfc_get_codigo.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     VALUE(OBJECT) TYPE  TROBJTYPE
*"     VALUE(OBJ_NAME) TYPE  SOBJ_NAME
*"     VALUE(MODO) TYPE  CHAR1 DEFAULT ''
*"  EXPORTING
*"     VALUE(I_CODIGO) TYPE  TTTEXT255
*"     VALUE(TRDIR) TYPE  TRDIR
*"----------------------------------------------------------------------

  i_codigo = zcl_ap_dev=>get_objeto_como_texto( tipo   = object
                                                nombre = obj_name
                                                modo   = modo ).

data(l_obj_name) = obj_name.
  case object.
    when 'PROG' OR 'FUGR'.
      IF OBJECT = 'FUGR'.
  SELECT SINGLE PNAME, INCLUDE FROM tfdir
    INTO (@DATA(l_pname), @data(l_include))
   WHERE funcname = @obj_name.
  CONCATENATE l_pname+3 'U' l_include INTO l_obj_name.
  endif.

    SELECT SINGLE * FROM TRDIR
      INTO TRDIR
     WHERE NAME = l_OBJ_NAME.

     when 'CLAS'.
      CONCATENATE obj_name '%' INTO L_OBJ_NAME.
      SELECT MAX( udat ) FROM progdir
        INTO TRDIR-udat
       WHERE name LIKE L_OBJ_NAME.

      SELECT unam FROM progdir
        INTO TRDIR-unam
        UP TO 1 ROWS
       WHERE name  LIKE L_OBJ_NAME
         AND state    = 'A'
         AND udat     = TRDIR-udat
       ORDER BY PRIMARY KEY.
      ENDSELECT.
  ENDCASE.
ENDFUNCTION.
