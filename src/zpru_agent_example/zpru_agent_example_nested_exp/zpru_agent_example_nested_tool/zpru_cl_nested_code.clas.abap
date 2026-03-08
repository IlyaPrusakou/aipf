CLASS zpru_cl_nested_code DEFINITION
  PUBLIC
  INHERITING FROM zpru_cl_abap_executor
  CREATE PUBLIC.

  PUBLIC SECTION.

  PROTECTED SECTION.
    METHODS execute_code_int REDEFINITION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zpru_cl_nested_code IMPLEMENTATION.
  METHOD execute_code_int.
    DATA ls_input  TYPE ZPRU_S_NESTED_ABAP_INPUT.
    DATA ls_output TYPE ZPRU_S_NESTED_ABAP_output.

    ls_input = is_input->*.

    IF ls_input IS INITIAL.
      RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDIF.

    ls_output-nestedabapoutput = `Nested abap code has played`.

    ASSIGN es_output->* TO FIELD-SYMBOL(<ls_output>).
    IF sy-subrc <> 0.
      ev_error_flag = abap_true.
    ENDIF.

    <ls_output> = ls_output.
  ENDMETHOD.
ENDCLASS.
