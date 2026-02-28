
CLASS lcl_adf_decision_provider IMPLEMENTATION.
  METHOD zpru_if_decision_provider~call_decision_engine.
  ENDMETHOD.

  METHOD zpru_if_decision_provider~prepare_final_response.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_adf_short_memory_provider IMPLEMENTATION.
ENDCLASS.


CLASS lcl_adf_long_memory_provider IMPLEMENTATION.

ENDCLASS.


CLASS lcl_adf_agent_info_provider IMPLEMENTATION.
  METHOD zpru_if_agent_info_provider~get_abap_agent_info.
  ENDMETHOD.

  METHOD zpru_if_agent_info_provider~get_agent_info.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_adf_syst_prompt_provider IMPLEMENTATION.
  METHOD zpru_if_prompt_provider~get_abap_system_prompt.
  ENDMETHOD.

  METHOD zpru_if_prompt_provider~get_system_prompt.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_adf_abap_executor IMPLEMENTATION.
  METHOD zpru_if_abap_executor~execute_code.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_adf_knowledge_provider IMPLEMENTATION.
  METHOD zpru_if_knowledge_provider~lookup_knowledge.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_adf_nested_agent IMPLEMENTATION.
  METHOD zpru_if_nested_agent_runner~run_nested_agent.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_adf_http_request_tool IMPLEMENTATION.
  METHOD zpru_if_http_request_sender~send_http.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_adf_service_cons_mdl_tool IMPLEMENTATION.
  METHOD zpru_if_service_model_consumer~consume_service_model.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_adf_call_llm_tool IMPLEMENTATION.
  METHOD zpru_if_llm_caller~call_large_language_model.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_adf_dynamic_abap_code_tool IMPLEMENTATION.
ENDCLASS.


CLASS lcl_adf_ml_model_inference IMPLEMENTATION.
  METHOD zpru_if_ml_model_inference~get_machine_learning_inference.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_adf_user_tool IMPLEMENTATION.
  METHOD zpru_if_user_tool~execute_user_tool.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_adf_tool_provider IMPLEMENTATION.
  METHOD zpru_if_tool_provider~get_tool.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_adf_tool_info_provider IMPLEMENTATION.
  METHOD zpru_if_tool_info_provider~get_abap_tool_info.
  ENDMETHOD.

  METHOD zpru_if_tool_info_provider~get_tool_info.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_adf_schema_provider IMPLEMENTATION.
  METHOD zpru_if_tool_schema_provider~input_json_schema.
  ENDMETHOD.

  METHOD zpru_if_tool_schema_provider~input_rtts_schema.
  ENDMETHOD.

  METHOD zpru_if_tool_schema_provider~output_json_schema.
  ENDMETHOD.

  METHOD zpru_if_tool_schema_provider~output_rtts_schema.
  ENDMETHOD.
ENDCLASS.
