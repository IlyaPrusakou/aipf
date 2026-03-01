CLASS lcl_common_algorithms DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.

    CLASS-METHODS get_last_thinkingstepnumber
      IMPORTING it_thinking_step                  TYPE zpru_tt_thinking_step
      RETURNING VALUE(rv_last_thinkingstepnumber) TYPE i.

    CLASS-METHODS get_timestamp
      RETURNING VALUE(rv_now) TYPE timestampl.

    CLASS-METHODS get_llm_api_factory
      RETURNING VALUE(ro_llm_api_factory) TYPE REF TO if_aic_islm_compl_api_factory.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

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
    METHODS get_http_client
      IMPORTING iv_url                TYPE string
      RETURNING VALUE(ro_http_client) TYPE REF TO if_web_http_client.

    METHODS send_via_url
      IMPORTING io_controller TYPE REF TO zpru_if_agent_controller
                io_request    TYPE REF TO zpru_if_payload
      EXPORTING eo_response   TYPE REF TO zpru_if_payload
                ev_error_flag TYPE abap_boolean.
ENDCLASS.


CLASS lcl_adf_service_cons_mdl_tool DEFINITION INHERITING FROM zpru_cl_service_model_consumer CREATE PUBLIC.
  PROTECTED SECTION.
    METHODS consume_service_model_int REDEFINITION.
ENDCLASS.


CLASS lcl_adf_call_llm_tool DEFINITION INHERITING FROM zpru_cl_llm_caller CREATE PUBLIC.
  PUBLIC SECTION.
    TYPES: BEGIN OF ts_result_payload,
             llm_response               TYPE string,
             llm_total_tokens           TYPE i,
             llm_finish_reason          TYPE aic_finish_reason=>type,
             llm_original_finish_reason TYPE string,
           END OF ts_result_payload.
  PROTECTED SECTION.
    METHODS call_large_language_model_int REDEFINITION.
    METHODS prepare_prompt
      IMPORTING io_llm_api           TYPE REF TO if_aic_completion_api
                iv_system_role       TYPE string
                iv_user_message      TYPE string
                iv_assistant_message TYPE string
                iv_user_message_2    TYPE string
      RETURNING VALUE(ro_message)    TYPE REF TO if_aic_message_container.

    METHODS get_response_schema
      RETURNING VALUE(rv_response_schema) TYPE  string.

    METHODS preprocess_llm_request
      IMPORTING io_controller    TYPE REF TO zpru_if_agent_controller
                io_request       TYPE REF TO zpru_if_payload
                iv_islm_scenario TYPE aic_islm_scenario_id=>type
      EXPORTING eo_message       TYPE REF TO if_aic_message_container
                eo_llm_api       TYPE REF TO if_aic_completion_api
                ev_error_flag    TYPE abap_boolean.

    METHODS process_llm_request
      IMPORTING io_controller TYPE REF TO zpru_if_agent_controller
                io_request    TYPE REF TO zpru_if_payload
                io_message    TYPE REF TO if_aic_message_container
                io_llm_api    TYPE REF TO if_aic_completion_api
      EXPORTING eo_response   TYPE REF TO zpru_if_payload
                ev_error_flag TYPE abap_boolean.



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
    METHODS process_dummy_email
      IMPORTING io_controller TYPE REF TO zpru_if_agent_controller
                io_request    TYPE REF TO zpru_if_payload
      EXPORTING eo_response   TYPE REF TO zpru_if_payload
                ev_error_flag TYPE abap_boolean.

    METHODS process_prod_email
      IMPORTING io_controller TYPE REF TO zpru_if_agent_controller
                io_request    TYPE REF TO zpru_if_payload
      EXPORTING eo_response   TYPE REF TO zpru_if_payload
                ev_error_flag TYPE abap_boolean.

    METHODS prepare_dummy_email
      IMPORTING iv_sender       TYPE zpru_cl_bcs_mail_message=>ty_address
                iv_recipient    TYPE zpru_cl_bcs_mail_message=>ty_address
                iv_subject      TYPE zpru_cl_bcs_mail_message=>ty_subject
                iv_content      TYPE string
                iv_content_type TYPE char128
      EXPORTING eo_mail_manager TYPE REF TO zpru_cl_bcs_mail_message.
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
