CLASS zpru_cl_nested_decision DEFINITION INHERITING FROM zpru_cl_decision_provider
  PUBLIC

  CREATE PUBLIC .

  PUBLIC SECTION.

  PROTECTED SECTION.
    METHODS: check_authorizations REDEFINITION,
      recall_memory REDEFINITION,
      read_data_4_thinking REDEFINITION,
      process_thinking REDEFINITION,
      prepare_first_tool_input REDEFINITION,
      set_model_id REDEFINITION,
      set_result_comment REDEFINITION,
      set_final_response_content REDEFINITION,
      set_final_response_metadata REDEFINITION.
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
