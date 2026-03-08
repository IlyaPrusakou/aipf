CLASS zpru_cl_nested_decision DEFINITION
  PUBLIC
  INHERITING FROM zpru_cl_decision_provider
  CREATE PUBLIC.

  PUBLIC SECTION.

  PROTECTED SECTION.
    METHODS check_authorizations        REDEFINITION.
    METHODS recall_memory               REDEFINITION.
    METHODS read_data_4_thinking        REDEFINITION.
    METHODS process_thinking            REDEFINITION.
    METHODS prepare_first_tool_input    REDEFINITION.
    METHODS set_model_id                REDEFINITION.
    METHODS set_result_comment          REDEFINITION.
    METHODS set_final_response_content  REDEFINITION.
    METHODS set_final_response_metadata REDEFINITION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zpru_cl_nested_decision IMPLEMENTATION.
  METHOD check_authorizations.
    ev_allowed = abap_true.
  ENDMETHOD.

  METHOD prepare_first_tool_input.
    FIELD-SYMBOLS <ls_nested_abap_input> TYPE zpru_s_nested_abap_input.

    ASSIGN er_first_tool_input->* TO <ls_nested_abap_input>.
    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDIF.

    <ls_nested_abap_input>-nestedabapinput = `Nested ABAP input`.
  ENDMETHOD.

  METHOD process_thinking.
    ev_langu = sy-langu.
    et_execution_plan = VALUE #( agentuuid = is_agent-agentuuid
                                 ( toolname = 'NESTED_ABAP'
                                   sequence = 1 )
                                 ( toolname = 'NESTED_LLM'
                                   sequence = 2 )
                                 ( toolname = 'NESTED_HTTP'
                                   sequence = 3 ) ).
  ENDMETHOD.

  METHOD read_data_4_thinking.
  ENDMETHOD.

  METHOD recall_memory.
  ENDMETHOD.

  METHOD set_final_response_content.
    IF iv_last_output <> `Nested http has played`.
      RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDIF.

    cs_final_response_body-responsecontent = `Nested final response`.
  ENDMETHOD.

  METHOD set_final_response_metadata.
  ENDMETHOD.

  METHOD set_model_id.
    rv_model_id = `ST-GEMINI-3.0`.
  ENDMETHOD.

  METHOD set_result_comment.
    rv_result_comment = `Decision Engine processing is finished`.
  ENDMETHOD.
ENDCLASS.
