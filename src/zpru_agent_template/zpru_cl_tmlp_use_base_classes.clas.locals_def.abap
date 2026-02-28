CLASS lcl_decision_provider DEFINITION INHERITING FROM zpru_cl_decision_provider CREATE PUBLIC.
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


CLASS lcl_short_memory_provider DEFINITION INHERITING FROM zpru_cl_short_memory_base CREATE PUBLIC.
  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_long_memory_provider DEFINITION INHERITING FROM zpru_cl_long_memory_base CREATE PUBLIC.
  PUBLIC SECTION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_agent_info_provider DEFINITION INHERITING FROM zpru_cl_agent_info_provider CREATE PUBLIC.
  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS: get_agent_main_info REDEFINITION,
      set_agent_goals REDEFINITION,
      prepare_agent_domains REDEFINITION,
      set_agent_restrictions REDEFINITION,
      set_tool_metadata REDEFINITION,
      get_free_text REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_prompt_provider DEFINITION INHERITING FROM zpru_cl_syst_prmpt_prvdr_base CREATE PUBLIC.
  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS: set_primary_session_task REDEFINITION.
    METHODS: set_technical_rules REDEFINITION,
      set_business_rules REDEFINITION,
      set_format_guidelines REDEFINITION,
      set_reasoning_step REDEFINITION,
      set_prompt_restrictions REDEFINITION.
    METHODS: set_arbitrary_text REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_abap_executor DEFINITION INHERITING FROM zpru_cl_abap_executor CREATE PUBLIC.
  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS execute_code_int REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_knowledge_provider DEFINITION  INHERITING FROM zpru_cl_knowledge_provider CREATE PUBLIC.
  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS: lookup_knowledge_int REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_nested_agent DEFINITION INHERITING FROM zpru_cl_nested_agent_runner CREATE PUBLIC.
  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS: run_nested_agent_int REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_http_request_tool DEFINITION INHERITING FROM zpru_cl_http_request_sender CREATE PUBLIC.
  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS: send_http_int REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_service_cons_model_tool DEFINITION INHERITING FROM zpru_cl_service_model_consumer CREATE PUBLIC.
  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS: consume_service_model_int REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_call_llm_tool DEFINITION INHERITING FROM zpru_cl_llm_caller CREATE PUBLIC.
  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS: call_large_language_model_int REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.


CLASS lcl_dynamic_abap_code_tool DEFINITION INHERITING FROM zpru_cl_dynamic_abap_base CREATE PUBLIC.
ENDCLASS.

CLASS lcl_ml_model_inference DEFINITION INHERITING FROM zpru_cl_ml_model_inference CREATE PUBLIC.
  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS: get_ml_inference_int REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.

CLASS lcl_user_tool DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zpru_if_tool_executor.
    INTERFACES zpru_if_user_tool.

  PROTECTED SECTION.

ENDCLASS.

CLASS lcl_tool_provider DEFINITION
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zpru_if_tool_provider.
ENDCLASS.

CLASS lcl_tool_info_provider DEFINITION
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zpru_if_tool_info_provider.
ENDCLASS.

CLASS lcl_schema_provider DEFINITION
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zpru_if_tool_schema_provider.
ENDCLASS.
