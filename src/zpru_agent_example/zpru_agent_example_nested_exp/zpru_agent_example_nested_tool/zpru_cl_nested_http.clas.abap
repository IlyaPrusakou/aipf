CLASS zpru_cl_nested_http DEFINITION
  PUBLIC
  INHERITING FROM zpru_cl_http_request_sender
  CREATE PUBLIC.

  PUBLIC SECTION.

  PROTECTED SECTION.
    METHODS send_http_int REDEFINITION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zpru_cl_nested_http IMPLEMENTATION.
  METHOD send_http_int.
    DATA ls_input  TYPE ZPRU_S_NESTED_http_INPUT.
    DATA ls_output TYPE ZPRU_S_NESTED_http_output.

    ls_input = is_input->*.

    IF ls_input IS INITIAL.
      RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDIF.

    ls_output-nestedhttpoutput = `Nested http has played`.

    ASSIGN es_output->* TO FIELD-SYMBOL(<ls_output>).
    IF sy-subrc <> 0.
      ev_error_flag = abap_true.
    ENDIF.

    <ls_output> = ls_output.
  ENDMETHOD.
ENDCLASS.
