*<SCRIPT:PERSISTENT>
REPORT  rstpda_script_template.

*<SCRIPT:HEADER>
*<SCRIPTNAME>ZRSTPDA_SCRIPT_AP_WATCHPOINT</SCRIPTNAME>
*<SCRIPT_CLASS>LCL_DEBUGGER_SCRIPT</SCRIPT_CLASS>
*<SCRIPT_COMMENT>Debugger Skript: Default Template</SCRIPT_COMMENT>
*<SINGLE_STEP>X</SINGLE_STEP>

*</SCRIPT:HEADER>

*<SCRIPT:PRESETTINGS>

*</SCRIPT:PRESETTINGS>

*<SCRIPT:SCRIPT_CLASS>
*---------------------------------------------------------------------*
*       CLASS lcl_debugger_script DEFINITION
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
CLASS lcl_debugger_script DEFINITION INHERITING FROM  cl_tpda_script_class_super  .

  PUBLIC SECTION.
    METHODS: prologue  REDEFINITION,
      init    REDEFINITION,
      script  REDEFINITION,
      end     REDEFINITION.
    DATA: zparametros TYPE zparametros.
ENDCLASS.                    "lcl_debugger_script DEFINITION
*---------------------------------------------------------------------*
*       CLASS lcl_debugger_script IMPLEMENTATION
*---------------------------------------------------------------------*
*
*---------------------------------------------------------------------*
CLASS lcl_debugger_script IMPLEMENTATION.
  METHOD prologue.
*** generate abap_source (source handler for ABAP)
    super->prologue( ).
  ENDMETHOD.                    "prolog

  METHOD init.
    DATA l_return TYPE char1.
    zcl_ap_popup=>popup_usuario( EXPORTING campo1 = 'ZPARAMETROS-CAMPO'
                                           campo2 = 'ZPARAMETROS-ATRIBUTO1'
                                           titulo = 'Indique variable y valor a buscar'
                                 IMPORTING return = l_return
                                 CHANGING  valor1 = zparametros-campo
                                           valor2 = zparametros-atributo1 ).

  ENDMETHOD.                    "init
  METHOD script.

    TRY.
        CALL METHOD cl_tpda_script_data_descr=>get_simple_value
          EXPORTING
            p_var_name  = CONV #( zparametros-campo )
          RECEIVING
            p_var_value = DATA(valor).
      CATCH cx_tpda_varname .
      CATCH cx_tpda_script_no_simple_type .
    ENDTRY.
    IF zparametros-atributo1 = valor OR ( zparametros-atributo1 = '*' and zparametros-atributo2 ne valor ).
      me->break( ).
    ENDIF.
    zparametros-atributo2 = valor.

  ENDMETHOD.                    "script
  METHOD end.
*** insert your code which shall be executed at the end of the scripting (before trace is saved)
*** here

  ENDMETHOD.                    "end
ENDCLASS.                    "lcl_debugger_script IMPLEMENTATION
*</SCRIPT:SCRIPT_CLASS>

*</SCRIPT:PERSISTENT>
