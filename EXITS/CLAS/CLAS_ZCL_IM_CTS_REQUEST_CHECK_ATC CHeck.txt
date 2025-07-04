
CLASS zcl_im_cts_request_check DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_ex_cts_request_check.


endclass. "ZCL_IM_CTS_REQUEST_CHECK definition
class ZCL_IM_CTS_REQUEST_CHECK implementation.
  METHOD if_ex_cts_request_check~check_before_add_objects.
* No implementado "#EC EMPTY_PROCEDURE
  ENDMETHOD.
  METHOD if_ex_cts_request_check~check_before_changing_owner.
* No implementado "#EC EMPTY_PROCEDURE
  ENDMETHOD.
  METHOD if_ex_cts_request_check~check_before_creation.
* No implementado "#EC EMPTY_PROCEDURE
  ENDMETHOD.
  METHOD if_ex_cts_request_check~check_before_release.
*        DATA: lv_exit TYPE char10.
*
*    CALL FUNCTION 'TRINT_TDR_USER_COMMAND'
*      EXPORTING
*        iv_object  = request
*        iv_type    = 'TASK'
*        iv_command = 'CHAO'
*      IMPORTING
*        ev_exit    = lv_exit.
*    DATA: lv_field TYPE char70.
*    FIELD-SYMBOLS: <fs_okcode> TYPE any.
*
*    lv_field = '(SAPLSTR7)GV_OKCODE'.
*    ASSIGN (lv_field) TO <fs_okcode>.
*    IF <fs_okcode> EQ 'CANC'.
*      RAISE cancel.
*    ENDIF.

    TYPES: BEGIN OF t_obj_pend,
             padre    TYPE tmsbuffer-trkorr,
             hijo     TYPE tmsbuffer-trkorr,
             sysnam   TYPE tmsbuffer-sysnam,
             owner    TYPE tmsbuffer-owner,
             text     TYPE tmsbuffer-text,
             object   TYPE e071-object,
             obj_name TYPE e071-obj_name,
           END OF t_obj_pend.

    DATA is_ok TYPE abap_bool VALUE abap_true.
    DATA: r_object   TYPE RANGE OF e071-object,
          r_obj_name TYPE RANGE OF e071-obj_name.
    DATA: l_obj_pend TYPE t_obj_pend,
          i_obj_pend TYPE TABLE OF t_obj_pend.
    DATA l_ucomm TYPE sy-ucomm.

* Sólo para usuario APC
    IF sy-uname <> zcl_c=>usuario_ap.
      RETURN.
    ENDIF.

* Sólo en estas transacciones
    IF NOT ( sy-tcode = 'SE09' OR sy-tcode = 'YAP' ).
      RETURN.
    ENDIF.

    IF sy-tcode = 'YAP'.
      GET PARAMETER ID 'ZNO_VERIF' FIELD DATA(l_no_verif).
      IF l_no_verif = 'X'.
        RETURN.
      ENDIF.
    ENDIF.

* Si en alguna OT no queremos verificación ponemos esto
    IF text CS '¡!'.
      RETURN.
    ENDIF.

* Si hemos confirmado previamente una OT, lo miramos, si no a veces entra en bucel.
    GET PARAMETER ID 'ZOT' FIELD DATA(l_ot).
    IF NOT l_ot IS INITIAL.
      IF request = l_ot.
        RETURN.
      ELSE.
* Si es la orden superior de la que acabamos de autorizar, tampoco verificamos
        SELECT SINGLE trkorr FROM e070
          INTO l_ot
         WHERE trkorr  = l_ot
           AND strkorr = request.
        IF sy-subrc = 0.
          RETURN.
        ENDIF.
      ENDIF.
    ENDIF.

* Si existe, mejor la variante ZAPC
    SELECT SINGLE checkvname FROM scichkv_hd            "#EC CI_NOFIRST
      INTO @DATA(l_checkvname)                          "#EC CI_NOORDER
     WHERE checkvname = 'ZAPC'.
    IF sy-subrc <> 0.
      SELECT SINGLE checkvname_new FROM scichkv_alter
        INTO @l_checkvname
       WHERE checkvname_def = 'TRANSPORT'.
    ENDIF.
    IF l_checkvname IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        DATA(or_factory) = NEW cl_satc_api_factory( ).
        DATA(it_objects) = cl_satc_object_set_factory=>create_for_transport( request )->if_satc_object_set~get_object_keys( ).
        DATA(or_objects) = cl_satc_object_set_factory=>create_for_object_keys( it_objects ).

        DATA(or_variant) = NEW cl_satc_ci_check_variant( ).
        or_variant->set_name( l_checkvname ).  " check variant
        DATA(l_desc) = |Transport release { request } |.
        DATA(or_run_config) = or_factory->create_run_config_with_chk_var( i_object_set = or_objects
                                                                          i_check_variant = or_variant
                                                                          i_description = CONV #( l_desc ) ).

        DATA(or_run_controller) = or_factory->create_run_controller( or_run_config ).
        or_run_controller->run( IMPORTING e_result_access = DATA(or_result_access) ).
        or_result_access->get_findings( IMPORTING e_findings = DATA(it_f) ).

        LOOP AT it_f ASSIGNING FIELD-SYMBOL(<wa_f>) WHERE     ( kind = 'E' OR kind = 'W' ) " errors/warnings
                                                          AND exceptn <> 'P'. " pseudo comments and pragmas
          is_ok = abap_false.
*          EXIT.
          IF <wa_f>-kind = 'E'.
            DATA(l_error) = 'X'.
          ENDIF.
        ENDLOOP.

      CATCH cx_satc_failure cx_satc_not_found INTO DATA(cx).
        DATA(exc_text) = cx->get_text( ).
        MESSAGE exc_text TYPE 'E'.
        is_ok = abap_false.
      CATCH cx_satc_empty_object_set cx_satc_invalid_argument INTO cx.  " ok, if transport is empty or contains only non-checkable objects
        exc_text = cx->get_text( ).
        MESSAGE exc_text TYPE 'S'.
    ENDTRY.

    IF is_ok = abap_true.
      MESSAGE 'Validaciones correctas' TYPE 'S'.
    ELSE.
*      "we only get the execution ID with this “dirty” cast:
*      DATA(or_result_access_int) = CAST CL_SATC_AC_RESULT_ACCESS( or_result_access ).

*      or_result_access_int->display_result( ).

      SELECT display_id FROM satc_ac_resulth "#EC CI_GENBUFF "#EC CI_BYPASS
        INTO @DATA(l_display_id)
        UP TO 1 ROWS
       WHERE title            = @l_desc
         AND chk_profile_name = @l_checkvname
         AND scheduled_by     = @sy-uname
       ORDER BY scheduled_on_ts DESCENDING.
      ENDSELECT.
      IF sy-subrc = 0.
        CALL FUNCTION 'SATC_AC_DISPL_RESULT_BY_EXEC'
          EXPORTING
            i_execution_id     = l_display_id
          EXCEPTIONS
            xpt_no_results     = 1
            xpt_not_authorized = 2
            xpt_display_used   = 3
            OTHERS             = 4.
      ENDIF.
      IF sy-subrc <> 0.
        MESSAGE 'Errores de ATC revise objetos' TYPE 'I'.
      ENDIF.

* Verificamos si hay objetos pendientes de importar en producción
      IF NOT it_objects IS INITIAL.
        SELECT tmsbuffer~trkorr AS padre, e070~trkorr AS hijo, sysnam, owner, text FROM tmsbuffer JOIN e070 ON e070~strkorr = tmsbuffer~trkorr
          INTO TABLE @DATA(i_tms)
         WHERE sysnam <> @zcl_c=>entorno_desarrollo
           AND impflg  = 'k' " La orden K está pendiente del import
           AND trfunc  = 'K'
           AND umodes <> 'I'
           AND umodes <> 'FI' " Orden ya importada, pero en espera de reimportación
           AND comsys  = @sy-sysid.

        LOOP AT i_tms ASSIGNING FIELD-SYMBOL(<tms>).
          LOOP AT it_objects ASSIGNING FIELD-SYMBOL(<obj>).
            CASE <obj>-obj_type.
              WHEN 'PROG'.
                r_object = VALUE #( option = 'EQ'
                                    sign = 'I'
                                    ( low = 'PROG' )
                                    ( low = 'REPS' ) ).
                r_obj_name = VALUE #( ( option = 'EQ' sign = 'I' low = <obj>-obj_name ) ).
              WHEN 'METH' OR 'CLAS'.
                r_object = VALUE #( option = 'EQ'
                                    sign = 'I'
                                    ( low = 'METH' )
                                    ( low = 'CLAS' ) ).

                r_obj_name = VALUE #( ( option = 'CP' sign = 'I' low = |{ <obj>-obj_name(30) }*| ) ).
              WHEN 'CINC'.
                " TODO: variable is assigned but never used (ABAP cleaner)
                SPLIT <obj>-obj_name AT '=' INTO DATA(cl) DATA(otr).
                r_obj_name = VALUE #( ( option = 'EQ' sign = 'I' low = cl ) ).
              WHEN OTHERS.
                r_obj_name = VALUE #( ( option = 'EQ' sign = 'I' low = <obj>-obj_name ) ).
            ENDCASE.
            SELECT trkorr, object, obj_name FROM e071
              INTO TABLE @DATA(i_obj)
             WHERE (    trkorr = @<tms>-hijo
                     OR trkorr = @<tms>-padre )
               AND obj_name IN @r_obj_name
               AND object   IN @r_object.
            LOOP AT i_obj ASSIGNING FIELD-SYMBOL(<objt>).
              CLEAR l_obj_pend.
              MOVE-CORRESPONDING <tms> TO l_obj_pend.
              MOVE-CORRESPONDING <objt> TO l_obj_pend.
              COLLECT l_obj_pend INTO i_obj_pend.
            ENDLOOP.
          ENDLOOP.
        ENDLOOP.
        IF NOT i_obj_pend IS INITIAL.
          CALL FUNCTION 'Z_POPUP_ALV_AP'
            EXPORTING
              titulo  = 'Objetos en ordenes pendientes de subir a producción'
              texto   = 'Los siguientes objetos están pendientes de subir a producción'
              texto2  = '¿Está seguro de querer liberar la OT?'
              botones = 'OK_CANCEL'
            IMPORTING
              ucomm   = l_ucomm
            TABLES
              t_datos = i_obj_pend.
          IF l_ucomm <> 'F01'.
            RAISE cancel.
          ENDIF.
        ENDIF.
      ENDIF.

      IF l_error = 'X'.
        DATA(l_titulo) = 'Errores de verificación'.
      ELSE.
        l_titulo = 'Advertencias en verificación'.
      ENDIF.

      IF zcl_ap_popup=>confirmar( titulo = l_titulo
                                  texto  = 'Confirme si desea seguir liberando'
                                  opcion = 'N' ) = 'X'.
        IF l_error = 'X'.
          IF zcl_ap_popup=>confirmar( titulo = 'Errores de verificación'
                                      texto  = '¿Realmente estás seguro?'
                                      opcion = 'N' ) = ''.
            RAISE cancel.
          ENDIF.
        ENDIF.
      ELSE.
        RAISE cancel.
      ENDIF.
    ENDIF.

    SET PARAMETER ID 'ZOT' FIELD request.
  ENDMETHOD.
  METHOD if_ex_cts_request_check~check_before_release_slin.
* No implementado
  ENDMETHOD.
endclass. "ZCL_IM_CTS_REQUEST_CHECK definition
endclass. "ZCL_IM_CTS_REQUEST_CHECK implementation
