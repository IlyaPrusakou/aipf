CLASS lcl_common_algorithms IMPLEMENTATION.
  METHOD get_last_thinkingstepnumber.
    DATA(lt_thinking_step) = it_thinking_step.
    SORT lt_thinking_step BY thinkingstepnumber DESCENDING.
    rv_last_thinkingstepnumber = VALUE i( lt_thinking_step[ 1 ]-thinkingstepnumber OPTIONAL ) + 1.
  ENDMETHOD.

  METHOD get_timestamp.
    GET TIME STAMP FIELD rv_now.
  ENDMETHOD.

  METHOD get_llm_api_factory.
    IF zpru_cl_logic_switch=>get_logic( ) = abap_true.
      ro_llm_api_factory = NEW zpru_cl_islm_compl_api_factory( ).
    ELSE.
      TRY.
          ro_llm_api_factory = cl_aic_islm_compl_api_factory=>get( ).
        CATCH cx_aic_api_factory.
          RETURN.
      ENDTRY.
    ENDIF.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_adf_decision_provider IMPLEMENTATION.
  METHOD check_authorizations.
*   AUTHORITY-CHECK OBJECT 'Test'
*   ID 'FIELD1' DUMMY.
*   IF sy-subrc <> 0.
*   ev_allowed = abap_false.
*   ELSE.
    ev_allowed = abap_true.
*   ENDIF.
  ENDMETHOD.

  METHOD prepare_first_tool_input.
    FIELD-SYMBOLS <ls_first_input> TYPE zpru_s_first_tool_input_exmpl.

    IF er_first_tool_input IS NOT BOUND.
      RETURN.
    ENDIF.

    ASSIGN er_first_tool_input->* TO <ls_first_input>.
    <ls_first_input>-firstinput = `{ 'Warehouse' : '0001' }`.

    APPEND INITIAL LINE TO cs_decision_log-thinkingsteps ASSIGNING FIELD-SYMBOL(<ls_thinking_step>).
    <ls_thinking_step>-thinkingstepnumber   = lcl_common_algorithms=>get_last_thinkingstepnumber(
                                                  cs_decision_log-thinkingsteps ).
    <ls_thinking_step>-thinkingstepdatetime = lcl_common_algorithms=>get_timestamp( ).
    <ls_thinking_step>-thinkingstepcontent  = `First Tool Input is processed`.
  ENDMETHOD.

  METHOD process_thinking.
    DATA lo_decision_request TYPE REF TO zpru_if_decision_request.

    lo_decision_request ?= zpru_cl_agent_service_mngr=>get_service(
                               iv_service = `ZPRU_IF_DECISION_REQUEST`
                               iv_context = zpru_if_agent_frw=>cs_context-standard ).

    DATA(lv_message) = lo_decision_request->get_decision_request_string( ).

    DATA(lo_factory) = lcl_common_algorithms=>get_llm_api_factory( ).

    TRY.
        DATA(lo_api) = lo_factory->create_instance( 'ST-GEMINI-3.0' ).
        DATA(lo_params) = lo_api->get_parameter_setter( ).
        lo_params->set_temperature( '0.5' ).

        " TODO: variable is assigned but never used (ABAP cleaner)
        FINAL(lv_response) = lo_api->execute_for_string( lv_message )->get_completion( ).
      CATCH cx_aic_api_factory
            cx_aic_completion_api.
        RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDTRY.

    ev_langu = sy-langu.
    et_execution_plan = VALUE #( agentuuid = is_agent-agentuuid
                                 ( toolname = 'DUMMY_CODE'
                                   sequence = 1 )
                                 ( toolname = 'NESTED_AGENT'
                                   sequence = 2 )
                                 ( toolname = 'DUMMY_ML'
                                   sequence = 3 )
                                 ( toolname = 'DUMMY_DYN_CODE'
                                   sequence = 4 )
                                 ( toolname = 'DUMMY_SCM'
                                   sequence = 5 )
                                 ( toolname = 'DUMMY_HTTP'
                                   sequence = 6 )
                                 ( toolname = 'DUMMY_LLM'
                                   sequence = 7 )
                                 ( toolname = 'DUMMY_KNOWLEDGE'
                                   sequence = 8 )
                                 ( toolname = 'DUMMY_USER_TOOL'
                                   sequence = 9 ) ).

    APPEND INITIAL LINE TO cs_decision_log-thinkingsteps ASSIGNING FIELD-SYMBOL(<ls_thinking_step>).
    <ls_thinking_step>-thinkingstepnumber   = lcl_common_algorithms=>get_last_thinkingstepnumber(
                                                  cs_decision_log-thinkingsteps ).
    <ls_thinking_step>-thinkingstepdatetime = lcl_common_algorithms=>get_timestamp( ).
    <ls_thinking_step>-thinkingstepcontent  = `LLM is called`.
  ENDMETHOD.

  METHOD read_data_4_thinking.
    et_rag_data = VALUE #(
        ( ragsourceuuid = '000000000000000000000001'
          ragsourcename = 'Good reciept instruction'
          ragchunks     = VALUE #(
              ragsourceuuid = '000000000000000000000001'
              ( ragchunkid   = 1
                chunkcontent = |1 Warehouse Clerk has the following rights during good reciept:| &&
                               |1.1 get all documentation from driver| &&
                               |1.2 provide visual inspection of goods|  )
              ( ragchunkid   = 2
                chunkcontent = |1 Warehouse Clerk has the following obligation during good reciept:| &&
                               |1.1 handle signed document, prooving good reciept, to driver| &&
                               |1.2 do not damage package during picking and loading operations|  ) ) ) ).

    APPEND INITIAL LINE TO cs_decision_log-thinkingsteps ASSIGNING FIELD-SYMBOL(<ls_thinking_step>).
    <ls_thinking_step>-thinkingstepnumber   = lcl_common_algorithms=>get_last_thinkingstepnumber(
                                                  cs_decision_log-thinkingsteps ).
    <ls_thinking_step>-thinkingstepdatetime = lcl_common_algorithms=>get_timestamp( ).
    <ls_thinking_step>-thinkingstepcontent  = `RAG data is fetched`.

    ev_user_data = `{ 'Clerk' : "John Doe", 'Resource' : 'ZZ_RALL' }`.

    APPEND INITIAL LINE TO cs_decision_log-thinkingsteps ASSIGNING <ls_thinking_step>.
    <ls_thinking_step>-thinkingstepnumber   = lcl_common_algorithms=>get_last_thinkingstepnumber(
                                                  cs_decision_log-thinkingsteps ).
    <ls_thinking_step>-thinkingstepdatetime = lcl_common_algorithms=>get_timestamp( ).
    <ls_thinking_step>-thinkingstepcontent  = `User data is fetched`.
  ENDMETHOD.

  METHOD recall_memory.
    DATA lo_msg_service TYPE REF TO zpru_if_mmsg_service.
    DATA lo_sum_service TYPE REF TO zpru_if_msum_service.

    et_session_memory = io_short_memory->get_history( ).

    APPEND INITIAL LINE TO cs_decision_log-thinkingsteps ASSIGNING FIELD-SYMBOL(<ls_thinking_step>).
    <ls_thinking_step>-thinkingstepnumber   = lcl_common_algorithms=>get_last_thinkingstepnumber(
                                                  cs_decision_log-thinkingsteps ).
    <ls_thinking_step>-thinkingstepdatetime = lcl_common_algorithms=>get_timestamp( ).
    <ls_thinking_step>-thinkingstepcontent  = `Session messages are fetched`.

    lo_msg_service ?= zpru_cl_agent_service_mngr=>get_service(
                          iv_service = `ZPRU_IF_MMSG_SERVICE`
                          iv_context = zpru_if_agent_frw=>cs_context-st_persistence_message ).

    lo_msg_service->query_mmsg( EXPORTING it_agent_uuid = VALUE #( ( sign   = `I`
                                                                     option = `EQ`
                                                                     low    = is_agent-agentuuid ) )
                                IMPORTING et_mmsg_k     = DATA(lt_mmsg_k) ).

    et_episodic_message_memory = io_long_memory->retrieve_message(
                                     it_mmsg_read_k = VALUE #( FOR <ls_m1>
                                                               IN lt_mmsg_k
                                                               ( messageuuid              = <ls_m1>-messageuuid
                                                                 control-messageuuid      = abap_true
                                                                 control-content          = abap_true
                                                                 control-messagetype      = abap_true
                                                                 control-messagecontentid = abap_true
                                                                 control-stage            = abap_true
                                                                 control-substage         = abap_true
                                                                 control-namespace        = abap_true
                                                                 control-username         = abap_true
                                                                 control-agentuuid        = abap_true
                                                                 control-runuuid          = abap_true
                                                                 control-queryuuid        = abap_true
                                                                 control-stepuuid         = abap_true
                                                                 control-messagedatetime  = abap_true
                                                                 control-createdby        = abap_true
                                                                 control-createdat        = abap_true
                                                                 control-changedby        = abap_true
                                                                 control-changedat        = abap_true  ) ) ).

    APPEND INITIAL LINE TO cs_decision_log-thinkingsteps ASSIGNING <ls_thinking_step>.
    <ls_thinking_step>-thinkingstepnumber   = lcl_common_algorithms=>get_last_thinkingstepnumber(
                                                  cs_decision_log-thinkingsteps ).
    <ls_thinking_step>-thinkingstepdatetime = lcl_common_algorithms=>get_timestamp( ).
    <ls_thinking_step>-thinkingstepcontent  = `Episodic memory messages are fetched`.

    lo_sum_service ?= zpru_cl_agent_service_mngr=>get_service(
                          iv_service = `ZPRU_IF_MSUM_SERVICE`
                          iv_context = zpru_if_agent_frw=>cs_context-st_persistence_message ).

    lo_sum_service->query_msum( EXPORTING it_agent_uuid = VALUE #( ( sign   = `I`
                                                                     option = `EQ`
                                                                     low    = is_agent-agentuuid ) )
                                IMPORTING et_msum_k     = DATA(lt_msum_k) ).

    et_episodic_summary_memory = io_long_memory->retrieve_summary( it_msum_read_k = VALUE #(
                                                                       FOR <ls_m2>
                                                                       IN lt_msum_k
                                                                       ( summaryuuid              = <ls_m2>-summaryuuid
                                                                         control-summaryuuid      = abap_true
                                                                         control-content          = abap_true
                                                                         control-summarycontentid = abap_true
                                                                         control-stage            = abap_true
                                                                         control-substage         = abap_true
                                                                         control-namespace        = abap_true
                                                                         control-username         = abap_true
                                                                         control-agentuuid        = abap_true
                                                                         control-runuuid          = abap_true
                                                                         control-queryuuid        = abap_true
                                                                         control-stepuuid         = abap_true
                                                                         control-messagedatetime  = abap_true
                                                                         control-createdby        = abap_true
                                                                         control-createdat        = abap_true
                                                                         control-changedby        = abap_true
                                                                         control-changedat        = abap_true  ) ) ).

    APPEND INITIAL LINE TO cs_decision_log-thinkingsteps ASSIGNING <ls_thinking_step>.
    <ls_thinking_step>-thinkingstepnumber   = lcl_common_algorithms=>get_last_thinkingstepnumber(
                                                  cs_decision_log-thinkingsteps ).
    <ls_thinking_step>-thinkingstepdatetime = lcl_common_algorithms=>get_timestamp( ).
    <ls_thinking_step>-thinkingstepcontent  = `Episodic summary messages are fetched`.

*  et_semantic_memory = ADD SERVICE

    APPEND INITIAL LINE TO cs_decision_log-thinkingsteps ASSIGNING <ls_thinking_step>.
    <ls_thinking_step>-thinkingstepnumber   = lcl_common_algorithms=>get_last_thinkingstepnumber(
                                                  cs_decision_log-thinkingsteps ).
    <ls_thinking_step>-thinkingstepdatetime = lcl_common_algorithms=>get_timestamp( ).
    <ls_thinking_step>-thinkingstepcontent  = `Semantic memory are fetched`.
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
    FIELD-SYMBOLS <ls_inspection_protocol> TYPE zpru_s_dummy_inspection_prtcl.

    ASSIGN eo_output->* TO <ls_inspection_protocol>.
    IF sy-subrc <> 0.
      ev_error_flag = abap_true.
    ENDIF.

    <ls_inspection_protocol> = VALUE zpru_s_dummy_inspection_prtcl(
        inspectionprotocolname        = 'GR-2026-00892'
        inspectionprotocoldescription = 'Inbound Inspection: Steel Coil Batch X-9'
        inspectionprotocoldate        = lcl_common_algorithms=>get_timestamp( )
        inspectionprotocolperson      = 'John Doe'
        text                          = 'Visual inspection passed. Certificate of Analysis (CoA) attached and verified.' ).
  ENDMETHOD.
ENDCLASS.


CLASS lcl_adf_nested_agent IMPLEMENTATION.
  METHOD run_nested_agent_int.
    " test data
    DATA(ls_safety_request) = VALUE zpru_s_dummy_safety_req(
        product_id       = 'CHEM-772-L'
        product_name     = 'Concentrated Sulfuric Acid 98%'
        material_group   = '00105'
        industry_sector  = 'C'
        quantity         = '1200.000'
        unit_of_measure  = 'KG'
        is_hazardous     = 'X'
        purchase_order   = '4500001234'
        vendor_name      = 'Global ChemCorp Solutions'
        storage_location = 'WH02'
        safety_notes_raw = 'Drums show slight surface condensation. MSDS rev 2025 attached.' ).

    DATA lo_nested_agent   TYPE REF TO zpru_if_unit_agent.
    DATA lv_final_response TYPE zpru_if_agent_frw=>ts_json.
    DATA ls_prompt         TYPE zpru_s_prompt.
    DATA lo_util           TYPE REF TO zpru_if_agent_util.
    DATA ls_final_response TYPE zpru_s_final_response.
    DATA lv_json_input TYPE zpru_if_agent_frw=>ts_json.

    FIELD-SYMBOLS <ls_safety_request>  TYPE zpru_s_dummy_safety_req.
    FIELD-SYMBOLS <ls_safety_response> TYPE zpru_s_dummy_safety_res.

    ASSIGN io_input->* TO <ls_safety_request>.
    IF sy-subrc <> 0.
      ev_error_flag = abap_true.
    ENDIF.

    <ls_safety_request> = ls_safety_request.
    lo_nested_agent = NEW zpru_cl_unit_agent( ).

    lo_util ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_AGENT_UTIL`
                                                        iv_context = zpru_if_agent_frw=>cs_context-standard ).
    lo_util->convert_to_string( EXPORTING ir_abap   = io_input
                                CHANGING  cr_string = lv_json_input ).

    ls_prompt-string_content = lo_util->wrap_to_json_markdown(
       EXPORTING
         iv_content  = lv_json_input ).

    lo_nested_agent->execute_agent( EXPORTING iv_agent_name          = 'NESTED_AGENT'
                                              is_prompt              = ls_prompt
                                              io_parent_controller   = io_controller
                                    IMPORTING ev_final_response      = lv_final_response
                                              eo_executed_controller = DATA(lo_nested_controler) ).

    ASSIGN eo_output->* TO <ls_safety_response>.
    IF sy-subrc <> 0.
      ev_error_flag = abap_true.
    ENDIF.

    lo_util->convert_to_abap( EXPORTING ir_string = REF #( lv_final_response )
                              CHANGING  cr_abap   = ls_final_response ).

    lo_util->convert_to_abap( EXPORTING ir_string = REF #( ls_final_response-finalresponsebody-responsecontent )
                              CHANGING  cr_abap   = <ls_safety_response> ).
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
