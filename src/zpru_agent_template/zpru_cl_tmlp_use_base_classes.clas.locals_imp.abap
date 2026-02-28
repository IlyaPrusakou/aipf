
CLASS lcl_adf_decision_provider IMPLEMENTATION.
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


CLASS lcl_adf_short_memory_provider IMPLEMENTATION.
ENDCLASS.


CLASS lcl_adf_long_memory_provider IMPLEMENTATION.

ENDCLASS.


CLASS lcl_adf_agent_info_provider IMPLEMENTATION.
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


CLASS lcl_adf_syst_prompt_provider IMPLEMENTATION.
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


CLASS lcl_adf_abap_executor IMPLEMENTATION.
  METHOD execute_code_int.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_adf_knowledge_provider IMPLEMENTATION.
  METHOD lookup_knowledge_int.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_adf_nested_agent IMPLEMENTATION.
  METHOD run_nested_agent_int.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_adf_http_request_tool IMPLEMENTATION.
  METHOD send_http_int.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_adf_service_cons_mdl_tool IMPLEMENTATION.
  METHOD consume_service_model_int.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_adf_call_llm_tool IMPLEMENTATION.
  METHOD call_large_language_model_int.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_adf_dynamic_abap_code_tool IMPLEMENTATION.
ENDCLASS.


CLASS lcl_adf_ml_model_inference IMPLEMENTATION.
  METHOD get_ml_inference_int.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_adf_user_tool IMPLEMENTATION.
  METHOD execute_user_tool_int.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_adf_tool_provider IMPLEMENTATION.
  METHOD provide_tool_instance.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_adf_tool_info_provider IMPLEMENTATION.
  METHOD get_main_tool_info.
  ENDMETHOD.

  METHOD set_tool_parameters.
  ENDMETHOD.

  METHOD set_tool_properties.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_adf_schema_provider IMPLEMENTATION.
  METHOD get_input_abap_type.
  ENDMETHOD.

  METHOD get_input_json_schema.
  ENDMETHOD.

  METHOD get_output_abap_type.
  ENDMETHOD.

  METHOD get_output_json_schema.
  ENDMETHOD.
ENDCLASS.
