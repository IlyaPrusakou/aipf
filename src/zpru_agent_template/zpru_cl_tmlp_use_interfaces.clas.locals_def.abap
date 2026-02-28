CLASS lcl_adf_decision_provider DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zpru_if_decision_provider.
ENDCLASS.


CLASS lcl_adf_short_memory_provider DEFINITION INHERITING FROM zpru_cl_short_memory_base CREATE PUBLIC.
  PUBLIC SECTION.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.


CLASS lcl_adf_long_memory_provider DEFINITION INHERITING FROM zpru_cl_long_memory_base CREATE PUBLIC.
  PUBLIC SECTION.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.


CLASS lcl_adf_agent_info_provider DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zpru_if_agent_info_provider.
ENDCLASS.


CLASS lcl_adf_syst_prompt_provider DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zpru_if_prompt_provider.
ENDCLASS.


CLASS lcl_adf_abap_executor DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zpru_if_tool_executor.
    INTERFACES zpru_if_abap_executor.
ENDCLASS.


CLASS lcl_adf_knowledge_provider DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zpru_if_tool_executor.
    INTERFACES zpru_if_knowledge_provider.
ENDCLASS.


CLASS lcl_adf_nested_agent DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zpru_if_tool_executor.
    INTERFACES zpru_if_nested_agent_runner.
ENDCLASS.


CLASS lcl_adf_http_request_tool DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zpru_if_tool_executor.
    INTERFACES zpru_if_http_request_sender.
ENDCLASS.


CLASS lcl_adf_service_cons_mdl_tool DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zpru_if_tool_executor.
    INTERFACES zpru_if_service_model_consumer.
ENDCLASS.


CLASS lcl_adf_call_llm_tool DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zpru_if_tool_executor.
    INTERFACES zpru_if_llm_caller.
ENDCLASS.


CLASS lcl_adf_dynamic_abap_code_tool DEFINITION INHERITING FROM zpru_cl_dynamic_abap_base CREATE PUBLIC.
ENDCLASS.


CLASS lcl_adf_ml_model_inference DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zpru_if_tool_executor.
    INTERFACES zpru_if_ml_model_inference.
ENDCLASS.


CLASS lcl_adf_user_tool DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zpru_if_tool_executor.
    INTERFACES zpru_if_user_tool.
ENDCLASS.


CLASS lcl_adf_tool_provider DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zpru_if_tool_provider.
ENDCLASS.


CLASS lcl_adf_tool_info_provider DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zpru_if_tool_info_provider.
ENDCLASS.


CLASS lcl_adf_schema_provider DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zpru_if_tool_schema_provider.
ENDCLASS.
