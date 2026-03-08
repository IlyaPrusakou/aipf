CLASS zpru_cl_nested_llm DEFINITION
  PUBLIC
  INHERITING FROM zpru_cl_llm_caller
  CREATE PUBLIC .

  PUBLIC SECTION.

  PROTECTED SECTION.
    METHODS: call_large_language_model_int REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zpru_cl_nested_llm IMPLEMENTATION.

  METHOD call_large_language_model_int.
    DATA ls_input  TYPE ZPRU_S_NESTED_llm_INPUT.
    DATA ls_output TYPE ZPRU_S_NESTED_llm_output.

    ls_input = is_input->*.

    IF ls_input IS INITIAL.
      RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDIF.

    ls_output-nestedllmoutput = `Nested llm has played`.

    ASSIGN es_output->* TO FIELD-SYMBOL(<ls_output>).
    IF sy-subrc <> 0.
      ev_error_flag = abap_true.
    ENDIF.

    <ls_output> = ls_output.
  ENDMETHOD.

ENDCLASS.
