
CLASS lcl_decision_provider IMPLEMENTATION.
  METHOD check_authorizations.
  ENDMETHOD.

  METHOD prepare_first_tool_input.
  ENDMETHOD.

  METHOD process_thinking.
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
  ENDMETHOD.

  METHOD set_result_comment.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_short_memory_provider IMPLEMENTATION.
ENDCLASS.


CLASS lcl_long_memory_provider IMPLEMENTATION.

ENDCLASS.


CLASS lcl_agent_info_provider IMPLEMENTATION.
  METHOD get_agent_main_info.
  ENDMETHOD.

  METHOD get_free_text.
  ENDMETHOD.

  METHOD prepare_agent_domains.
  ENDMETHOD.

  METHOD set_agent_goals.
  ENDMETHOD.

  METHOD set_agent_restrictions.
  ENDMETHOD.

  METHOD set_tool_metadata.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_prompt_provider IMPLEMENTATION.
  METHOD set_primary_session_task.
  ENDMETHOD.

  METHOD set_business_rules.
  ENDMETHOD.

  METHOD set_format_guidelines.
  ENDMETHOD.

  METHOD set_prompt_restrictions.
  ENDMETHOD.

  METHOD set_reasoning_step.
  ENDMETHOD.

  METHOD set_technical_rules.
  ENDMETHOD.

  METHOD set_arbitrary_text.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_abap_executor IMPLEMENTATION.
  METHOD execute_code_int.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_knowledge_provider IMPLEMENTATION.
  METHOD lookup_knowledge_int.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_nested_agent IMPLEMENTATION.
  METHOD run_nested_agent_int.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_http_request_tool IMPLEMENTATION.
  METHOD send_http_int.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_service_cons_model_tool IMPLEMENTATION.
  METHOD consume_service_model_int.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_call_llm_tool IMPLEMENTATION.

  METHOD call_large_language_model_int.

  ENDMETHOD.

ENDCLASS.


CLASS lcl_dynamic_abap_code_tool IMPLEMENTATION.
ENDCLASS.


CLASS lcl_ml_model_inference IMPLEMENTATION.

  METHOD get_ml_inference_int.

  ENDMETHOD.

ENDCLASS.


CLASS lcl_user_tool IMPLEMENTATION.
  METHOD zpru_if_user_tool~execute_user_tool.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_tool_provider IMPLEMENTATION.
  METHOD zpru_if_tool_provider~get_tool.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_tool_info_provider IMPLEMENTATION.
  METHOD zpru_if_tool_info_provider~get_tool_info.
  ENDMETHOD.

  METHOD zpru_if_tool_info_provider~get_abap_tool_info.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_schema_provider IMPLEMENTATION.
  METHOD zpru_if_tool_schema_provider~input_json_schema.
  ENDMETHOD.

  METHOD zpru_if_tool_schema_provider~input_rtts_schema.
  ENDMETHOD.

  METHOD zpru_if_tool_schema_provider~output_json_schema.
  ENDMETHOD.

  METHOD zpru_if_tool_schema_provider~output_rtts_schema.
  ENDMETHOD.
ENDCLASS.
