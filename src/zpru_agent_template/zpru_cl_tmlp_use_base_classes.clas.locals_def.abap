CLASS lcl_adf_decision_provider DEFINITION INHERITING FROM zpru_cl_decision_provider CREATE PUBLIC.
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


CLASS lcl_adf_agent_info_provider DEFINITION INHERITING FROM zpru_cl_agent_info_provider CREATE PUBLIC.
  PROTECTED SECTION.
    METHODS get_agent_main_info    REDEFINITION.
    METHODS set_agent_goals        REDEFINITION.
    METHODS prepare_agent_domains  REDEFINITION.
    METHODS set_agent_restrictions REDEFINITION.
    METHODS set_tool_metadata      REDEFINITION.
    METHODS get_free_text          REDEFINITION.
ENDCLASS.


CLASS lcl_adf_syst_prompt_provider DEFINITION INHERITING FROM zpru_cl_syst_prmpt_prvdr_base CREATE PUBLIC.
  PROTECTED SECTION.
    METHODS set_primary_session_task REDEFINITION.
    METHODS set_technical_rules      REDEFINITION.
    METHODS set_business_rules       REDEFINITION.
    METHODS set_format_guidelines    REDEFINITION.
    METHODS set_reasoning_step       REDEFINITION.
    METHODS set_prompt_restrictions  REDEFINITION.
    METHODS set_arbitrary_text       REDEFINITION.
ENDCLASS.


CLASS lcl_adf_abap_executor DEFINITION INHERITING FROM zpru_cl_abap_executor CREATE PUBLIC.
  PROTECTED SECTION.
    METHODS execute_code_int REDEFINITION.
ENDCLASS.


CLASS lcl_adf_knowledge_provider DEFINITION INHERITING FROM zpru_cl_knowledge_provider CREATE PUBLIC.
  PROTECTED SECTION.
    METHODS lookup_knowledge_int REDEFINITION.
ENDCLASS.


CLASS lcl_adf_nested_agent DEFINITION INHERITING FROM zpru_cl_nested_agent_runner CREATE PUBLIC.
  PROTECTED SECTION.
    METHODS run_nested_agent_int REDEFINITION.
ENDCLASS.


CLASS lcl_adf_http_request_tool DEFINITION INHERITING FROM zpru_cl_http_request_sender CREATE PUBLIC.
  PROTECTED SECTION.
    METHODS send_http_int REDEFINITION.
ENDCLASS.


CLASS lcl_adf_service_cons_mdl_tool DEFINITION INHERITING FROM zpru_cl_service_model_consumer CREATE PUBLIC.
  PROTECTED SECTION.
    METHODS consume_service_model_int REDEFINITION.
ENDCLASS.


CLASS lcl_adf_call_llm_tool DEFINITION INHERITING FROM zpru_cl_llm_caller CREATE PUBLIC.
  PROTECTED SECTION.
    METHODS call_large_language_model_int REDEFINITION.
ENDCLASS.


CLASS lcl_adf_dynamic_abap_code_tool DEFINITION INHERITING FROM zpru_cl_dynamic_abap_base CREATE PUBLIC.
ENDCLASS.


CLASS lcl_adf_ml_model_inference DEFINITION INHERITING FROM zpru_cl_ml_model_inference CREATE PUBLIC.
  PROTECTED SECTION.
    METHODS get_ml_inference_int REDEFINITION.
ENDCLASS.


CLASS lcl_adf_user_tool DEFINITION INHERITING FROM zpru_cl_user_tool CREATE PUBLIC.
  PROTECTED SECTION.
    METHODS execute_user_tool_int REDEFINITION.
ENDCLASS.


CLASS lcl_adf_tool_provider DEFINITION INHERITING FROM zpru_cl_tool_provider CREATE PUBLIC.
  PROTECTED SECTION.
    METHODS provide_tool_instance REDEFINITION.
ENDCLASS.


CLASS lcl_adf_tool_info_provider DEFINITION INHERITING FROM zpru_cl_tool_info_provider CREATE PUBLIC.
  PROTECTED SECTION.
    METHODS get_main_tool_info  REDEFINITION.
    METHODS set_tool_properties REDEFINITION.
    METHODS set_tool_parameters REDEFINITION.
ENDCLASS.


CLASS lcl_adf_schema_provider DEFINITION INHERITING FROM zpru_cl_tool_schema_provider CREATE PUBLIC.

  PROTECTED SECTION.
    METHODS get_input_abap_type    REDEFINITION.
    METHODS get_input_json_schema  REDEFINITION.
    METHODS get_output_abap_type   REDEFINITION.
    METHODS get_output_json_schema REDEFINITION.
ENDCLASS.
