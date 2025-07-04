*&---------------------------------------------------------------------*
*& Report  ZMACROS_AP
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT  zmacros_ap.

TABLES trmac.

START-OF-SELECTION.

  DATA: v_new,
        v_numm TYPE i.

  DEFINE mac_new.
    CLEAR: trmac, v_new.
    CONCATENATE '__' &1 INTO trmac-name.
    TRANSLATE trmac-name TO UPPER CASE.
    SELECT SINGLE * FROM trmac
     WHERE name = trmac-name.
    IF sy-subrc NE 0.
      v_new = 'X'.
      CLEAR v_numm.
    ENDIF.
  END-OF-DEFINITION.

  DEFINE mac_add.
    IF v_new = 'X'.
      IF strlen( &1 ) > 40.
                ROLLBACK WORK.
        MESSAGE |Línea { &1 } con más de 40 carácteres| TYPE 'E'.
      ENDIF.
      trmac-numm = v_numm.
      ADD 1 TO v_numm.
      IF v_numm = 0.
        trmac-numm = '000'.
      ELSE.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = trmac-numm
          IMPORTING
            output = trmac-numm.

        trmac-numm = trmac-numm+1.
      ENDIF.
      trmac-line = &1.
      MODIFY trmac.
    ENDIF.
  END-OF-DEFINITION.

  mac_new 'DATA_SET_VART'.
  mac_add: '* Definición de variables en base a tipo estructura',
           'TYPES T_&1 TYPE &1.',
           'TYPES TT_&1 TYPE TABLE OF T_&1.',
           'DATA I_&1 TYPE TT_&1.',
           'DATA L_&1 TYPE T_&1.',
           'FIELD-SYMBOLS <&1> TYPE T_&1.'.

  mac_new 'DATA_SET_VAR'.
  mac_add: '* Definición de variables en base a tipo',
           'TYPES TT_&1 TYPE TABLE OF T_&1.',
           'DATA I_&1 TYPE TT_&1.',
           'DATA L_&1 TYPE T_&1.',
           'FIELD-SYMBOLS <&1> TYPE T_&1.'.

  mac_new 'PONER_CEROS'.
  mac_add: '* Rellenar con ceros por la izquierda',
           'ZCL_AP_STRING=>PONER_CEROS_C(          *',
           ' CHANGING  CADENA = &1 )'.

  mac_new 'QUITAR_CEROS'.
  mac_add: '* Quitar ceros de la izquierda',
           'ZCL_AP_STRING=>QUITAR_CEROS_C(         *',
           ' CHANGING  CADENA = &1 )'.

  mac_new 'FORMATEAR_MATERIAL'.
  mac_add: '* Formato interno del material',
           '&1 = ZCL_AP_MATERIAL=>FORMATO_INTERNO( *',
           '                      MATNR = &1 )'.


  mac_new 'CONCAT_A'.
  mac_add: '* Concatenar 1 parámetros sobre variabla actual',
           '&1 = ZCL_AP_UTILS=>CONCAT( p1 = &1     *',
           '                           p2 = &2 )'.

  mac_new 'CONCAT2'.
  mac_add: '* Concatenar 2 parámetros',
           '&1 = ZCL_AP_UTILS=>CONCAT( p1 = &2     *',
           '                           p2 = &3 )'.

  mac_new 'CONCAT3'.
  mac_add: '* Concatenar 3 parámetros',
           '&1 = ZCL_AP_UTILS=>CONCAT( p1 = &2     *',
             '                p2 = &3  p3 = &4 )'.

  mac_new 'CONCAT4'.
  mac_add: '* Concatenar 4 parámetros',
           '&1 = ZCL_AP_UTILS=>CONCAT( p1 = &2     *',
             '        p2 = &3  p3 = &4 p4 = &5 )'.

  mac_new 'BREAK_AP'.
  mac_add: '* BREAK-POINT usuario AP',
           'IF SY-UNAME = zcl_c=>usuario_ap.',
           '  BREAK-POINT.',
           'ENDIF.'.

  mac_new 'BREAK_AP_SE38'.
  mac_add: '* BREAK-POINT usuario AP/SE38',
           'IF SY-UNAME = zcl_c=>usuario_ap AND    *',
           '   SY-TCODE = ''SE38''.',
           '  BREAK-POINT.',
           'ENDIF.'.

  mac_new 'BREAK_AP_SE38_P'.
  mac_add: '* BREAK-POINT usuario AP/SE38',
           'IF SY-UNAME = zcl_c=>usuario_ap AND    *',
           '   SY-TCODE = ''SE38'' AND &1 = &2.',
           '  BREAK-POINT.',
           'ENDIF.'.

  mac_new 'BOTONES_PLANTILLA'.
  mac_add: '* Añade los botones por defecto pant.Sel',
           'TABLES SSCRFIELDS.',
           'SELECTION-SCREEN FUNCTION KEY 2',
           'SELECTION-SCREEN FUNCTION KEY 3',
           'SELECTION-SCREEN FUNCTION KEY 4',
           'SELECTION-SCREEN FUNCTION KEY 5'.

  mac_new 'ADD_LISTA'.
  mac_add: '* Añade elemento a lista',
           'ZCL_AP_LISTA=>ADD(                     *',
           '      EXPORTING VALOR = &2             *',
           '      CHANGING LISTA = &1 ).'.

  mac_new 'ADD_LISTA_NO0'.
  mac_add: '* Añade elemento sin ceros a lista ',
           'ZCL_AP_LISTA=>ADD(                     *',
           '      EXPORTING VALOR = &2             *',
           '                QUITAR_CEROS = ''X''     *',
           '      CHANGING LISTA = &1 ).'.

  mac_new 'RANGO_EQ'.
  mac_add: '* Añade elemento a rango',
           'CLEAR &1.',
           '&1-OPTION = ''EQ''',
           '&1-SIGN = ''I''',
           '&1-LOW = &2',
           'APPEND &1'.

  mac_new 'RANGO_NE'.
  mac_add: '* Añade elemento a rango',
           'CLEAR &1.',
           '&1-OPTION = ''EQ''',
           '&1-SIGN = ''E''',
           '&1-LOW = &2',
           'APPEND &1'.

  mac_new 'RANGO_BT'.
  mac_add: '* Añade elemento a rango',
           'CLEAR &1.',
           '&1-OPTION = ''BT''',
           '&1-SIGN = ''I''',
           '&1-LOW = &2',
           '&1-HIGH = &3',
           'APPEND &1'.

  mac_new 'DEF_RANGOC'.
  mac_add: '* Define variable rango OO',
           'DATA R_&1 TYPE RANGE OF &1.',
           'DATA LR_&1 LIKE LINE OF R_&1'.

  mac_new 'RANGOC_EQ'.
  mac_add: '* Añade elemento a rango',
           'CLEAR L&1.',
           'L&1-OPTION = ''EQ''',
           'L&1-SIGN = ''I''',
           'L&1-LOW = &2',
           'COLLECT L&1 INTO &1'.

  mac_new 'RANGOC_NE'.
  mac_add: '* Añade elemento a rango',
           'CLEAR L&1.',
           'L&1-OPTION = ''EQ''',
           'L&1-SIGN = ''E''',
           'L&1-LOW = &2',
           'COLLECT L&1 INTO &1'.

**https://github.com/ypinc/ypinc           ZAP_INCLUDE_CACHES
*DELETE FROM trmac WHERE name = '__CLASS_CACHE'.
*
*  mac_new 'CLASS_CACHE'.
*  mac_add: 'class lcl_cache_&1_by_&2 definition.',
*           'public section.',
*           'class-methods class_constructor.',
*           'class-methods get importing ',
*           ` iv_&2 type &1-&2 `,
*           ` returning value(rs_&1) type &1.`,
*           'class-methods select importing ',
*           ` iv_&2 type &1-&2 `,
*           ` returning value(rs_&1) type &1.`,
*           'class-data:',
*           'miss type i,',
*           'hits type i.',
*           'private section.',
*           'class-data:',
*           'heap type hashed table of &1 with unique ',
*           ' key &2 initial size &3,',
*           'anti type hashed table of &1-&2 with ',
*           ` unique key table_line,`,
*           'msize type i.',
*           'endclass.',
*           'class lcl_cache_&1_by_&2 implementation.',
*           'method class_constructor.',
*           'msize = &3.',
*           'endmethod.',
*           'method get.',
*           'read table heap into rs_&1 ',
*           ` with key &2 = iv_&2.`,
*           'if sy-subrc = 0. " 1. found, not at head',
*           'add 1 to hits.',
*           'else.',
*           'if anti[] is not initial.  ',
*           'read table anti with table ',
*           ` key table_line = iv_&2 `,
*           `  transporting no fields.`,
*           'if sy-subrc = 0.',
*           'add 1 to hits.',
*           'clear rs_&1. return.',
*           'endif.',
*           'endif.',
*           'add 1 to miss.',
*           'rs_&1 = select( iv_&2 ).',
*           'if rs_&1-&2 is initial.',
*           'insert iv_&2 into table anti. return.',
*           'endif.',
*           'if lines( heap ) = msize.',
*           'clear heap[].',
*           'endif.',
*           'insert rs_&1 into table heap.',
*           'endif.',
*           'endmethod.',
*           'method select.'.
