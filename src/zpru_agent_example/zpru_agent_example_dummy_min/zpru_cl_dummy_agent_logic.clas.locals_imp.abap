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
                                                               ( MessageUUID              = <ls_m1>-MessageUUID
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
    ev_agentname = `Dummy Agent`.
    ev_agentversion = `Version 1.0.0`.
    ev_agentrole = `It's demo agent, using for testing and demo purposes. The legend is that it get data for CMR and perform action on it.`.
  ENDMETHOD.

  METHOD get_free_text.
  ENDMETHOD.

  METHOD prepare_agent_domains.
    rs_agent_domains-agentdomainname    = `Demo and Testing AIPF framework.`.
    rs_agent_domains-agentdomaincontent = |It is used for my internal testing of AIPF framework.| &&
                                          |It is used for demo purposes to show basic Agent structure.|.

    APPEND INITIAL LINE TO rs_agent_domains-agentsubdomains ASSIGNING FIELD-SYMBOL(<ls_sub_domains>).
    <ls_sub_domains>-agentsubdomainname    = `Transportation Management.`.
    <ls_sub_domains>-agentsubdomaincontent = `For demo I use scenario of processing CMR document handled by driver to clerk during finishing delivery.`.

    APPEND INITIAL LINE TO rs_agent_domains-agentsubdomains ASSIGNING <ls_sub_domains>.
    <ls_sub_domains>-agentsubdomainname    = `Extended Warehouse Management.`.
    <ls_sub_domains>-agentsubdomaincontent = `For demo I use scenario of processing CMR document handled by driver to clerk during starting putaway in EWM complex.`.
  ENDMETHOD.

  METHOD set_agent_goals.
    APPEND INITIAL LINE TO rt_agent_goals ASSIGNING FIELD-SYMBOL(<ls_agent_goal>).
    <ls_agent_goal>-agentgoalid              = 1.
    <ls_agent_goal>-agentgoaldescription     = `Demo Purpose`.
    <ls_agent_goal>-agentgoalpriority        = 1.
    <ls_agent_goal>-agentgoalcontent         = `Consumer must be satisfied by Demo and get clear understanding how he can use AIPF framework in his business.`.
    <ls_agent_goal>-agentgoalsuccesscriteria = `Consumer gets clear vision of AIPF framework.`.

    APPEND INITIAL LINE TO rt_agent_goals ASSIGNING <ls_agent_goal>.
    <ls_agent_goal>-agentgoalid              = 2.
    <ls_agent_goal>-agentgoaldescription     = `Test Purpose`.
    <ls_agent_goal>-agentgoalpriority        = 1.
    <ls_agent_goal>-agentgoalcontent         = `Developer must test AIPF framework and get clear understanding how he can use AIPF framework in his development.`.
    <ls_agent_goal>-agentgoalsuccesscriteria = `Developer gets clear vision of AIPF framework.`.

    APPEND INITIAL LINE TO rt_agent_goals ASSIGNING <ls_agent_goal>.
    <ls_agent_goal>-agentgoalid              = 3.
    <ls_agent_goal>-agentgoaldescription     = `Be Close To Business Purpose`.
    <ls_agent_goal>-agentgoalpriority        = 2.
    <ls_agent_goal>-agentgoalcontent         = `You must work in a way which is the most natural for Transportation and Extended Warehouse Management`.
    <ls_agent_goal>-agentgoalsuccesscriteria = `Your behavior simulates real business process`.
  ENDMETHOD.

  METHOD set_agent_restrictions.
    APPEND INITIAL LINE TO rt_agent_restrictions ASSIGNING FIELD-SYMBOL(<ls_agent_restrictions>).
    <ls_agent_restrictions>-agentrestrictionname = `Grounding`.
    <ls_agent_restrictions>-agentrestriction     = `Strictly follow all provided instructions`.
  ENDMETHOD.

  METHOD set_tool_metadata.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_adf_syst_prompt_provider IMPLEMENTATION.
  METHOD set_primary_session_task.
    ev_primary_session_task = `Show how to build agent and how agents work in general in AIPF framework`.
  ENDMETHOD.

  METHOD set_business_rules.
    APPEND INITIAL LINE TO rt_business_rules ASSIGNING FIELD-SYMBOL(<ls_rule>).
    <ls_rule>-businessrulesname = `RULE_VALIDATE_RMA`.
    <ls_rule>-businessrule      = `IF rma_number IS INITIAL, BLOCK_GR 'Goods receipt requires a valid Return Material Authorization (RMA)'.`.

    APPEND INITIAL LINE TO rt_business_rules ASSIGNING <ls_rule>.
    <ls_rule>-businessrulesname = `RULE_QI_TRIGGER`.
    <ls_rule>-businessrule      = `IF return_reason = 'DEFECTIVE' OR return_reason = 'DAMAGED', SET stock_type = 'S' (Blocked/QI).`.

    APPEND INITIAL LINE TO rt_business_rules ASSIGNING <ls_rule>.
    <ls_rule>-businessrulesname = `RULE_RESTOCKING_FEE`.
    <ls_rule>-businessrule      = `IF return_reason = 'NOT_LIKED' AND days_since_purchase > 30, APPLY fee_pct = 15. ELSE, fee_pct = 0.`.

    APPEND INITIAL LINE TO rt_business_rules ASSIGNING <ls_rule>.
    <ls_rule>-businessrulesname = `RULE_AUTO_REPLACEMENT`.
    <ls_rule>-businessrule      = `IF customer_tier = 'GOLD' AND gr_quantity_verified = 'X', CREATE_SO_TYPE 'RE' (Immediate Replacement).`.
  ENDMETHOD.

  METHOD set_format_guidelines.
    APPEND INITIAL LINE TO rt_format_guidelines ASSIGNING FIELD-SYMBOL(<ls_format_guidelines>).
    <ls_format_guidelines>-formatguidelinename = `NO_MARKDOWN_WRAPPERS`.
    <ls_format_guidelines>-formatguideline     = `Return only raw ABAP code. Do not include triple backticks or explanatory text.`.

    APPEND INITIAL LINE TO rt_format_guidelines ASSIGNING <ls_format_guidelines>.
    <ls_format_guidelines>-formatguidelinename = `PLANE_ENGLISH`.
    <ls_format_guidelines>-formatguideline     = `Avoid corporate jargon (e.g., 'Logistics Processing') in favor of clear actions (e.g., 'We are checking your item now').`.

    APPEND INITIAL LINE TO rt_format_guidelines ASSIGNING <ls_format_guidelines>.
    <ls_format_guidelines>-formatguidelinename = `CLEAR_CALL_TO_ACTION`.
    <ls_format_guidelines>-formatguideline     = `Every customer update must conclude with a specific timeline or a 'What happens next' step.`.

    APPEND INITIAL LINE TO rt_format_guidelines ASSIGNING <ls_format_guidelines>.
    <ls_format_guidelines>-formatguidelinename = `SMS_OPTIMIZATION`.
    <ls_format_guidelines>-formatguideline     = `Provide a secondary, condensed version of the message (max 160 characters) for SMS/Push notification alerts.`.

    APPEND INITIAL LINE TO rt_format_guidelines ASSIGNING <ls_format_guidelines>.
    <ls_format_guidelines>-formatguidelinename = `INTERNAL_VS_EXTERNAL`.
    <ls_format_guidelines>-formatguideline     = `Clearly separate 'Internal Notes' (for the warehouse agent) from 'Customer Facing' text in the output.`.
  ENDMETHOD.

  METHOD set_prompt_restrictions.
    APPEND INITIAL LINE TO rt_prompt_restrictions ASSIGNING FIELD-SYMBOL(<ls_prompt_restrictions>).
    <ls_prompt_restrictions>-promptrestrictionname = `DATA_VERACITY`.
    <ls_prompt_restrictions>-promptrestriction     = `If a Material Document or RMA ID is not found in the provided context, state 'ID not found'. Do not guess.`.

    APPEND INITIAL LINE TO rt_prompt_restrictions ASSIGNING <ls_prompt_restrictions>.
    <ls_prompt_restrictions>-promptrestrictionname = `SENSITIVE_DATA_FILTER`.
    <ls_prompt_restrictions>-promptrestriction     = `Mask all Customer Tax IDs and Bank Account numbers (IBAN) using asterisks (***) before returning the response.`.

    APPEND INITIAL LINE TO rt_prompt_restrictions ASSIGNING <ls_prompt_restrictions>.
    <ls_prompt_restrictions>-promptrestrictionname = `DOMAIN_LOCK`.
    <ls_prompt_restrictions>-promptrestriction     = `Refuse to answer queries regarding HR, Payroll, or Corporate Strategy. Only process Goods Receipt and CMR data.`.

    APPEND INITIAL LINE TO rt_prompt_restrictions ASSIGNING <ls_prompt_restrictions>.
    <ls_prompt_restrictions>-promptrestrictionname = `NO_CONVERSATIONAL_FLUFF`.
    <ls_prompt_restrictions>-promptrestriction     = `If the requested output format is JSON, do not include 'Here is your data' or other conversational phrases.`.

    APPEND INITIAL LINE TO rt_prompt_restrictions ASSIGNING <ls_prompt_restrictions>.
    <ls_prompt_restrictions>-promptrestrictionname = `LIABILITY_DISCLAIMER`.
    <ls_prompt_restrictions>-promptrestriction     = `Do not provide legal advice regarding return policies. Refer the user to the 'Standard Terms & Conditions' document.`.
  ENDMETHOD.

  METHOD set_reasoning_step.
    APPEND INITIAL LINE TO rt_reasoning_step ASSIGNING FIELD-SYMBOL(<ls_step>).
    <ls_step>-reasoningstepname        = `VERIFY_RMA_SOURCE`.
    <ls_step>-reasoningstepquestion    = `Is the provided RMA number valid and linked to an existing Sales Order?`.
    <ls_step>-reasoninginstruction     = `Cross-reference the RMA ID against table VBAK/VBAP. If no match, stop and request the correct ID.`.
    <ls_step>-reasoningstepismandatory = abap_true.

    APPEND INITIAL LINE TO rt_reasoning_step ASSIGNING <ls_step>.
    <ls_step>-reasoningstepname        = `QUANTITY_CHECK`.
    <ls_step>-reasoningstepquestion    = `Does the physical arrived quantity match the expected return quantity?`.
    <ls_step>-reasoninginstruction     = `Compare 'iv_arrived_qty' with 'et_expected_qty'. Note any overages or shortages for the adjustment log.`.
    <ls_step>-reasoningstepismandatory = abap_true.

    APPEND INITIAL LINE TO rt_reasoning_step ASSIGNING <ls_step>.
    <ls_step>-reasoningstepname        = `STOCK_TYPE_DETERMINATION`.
    <ls_step>-reasoningstepquestion    = `Which warehouse bin/stock type is appropriate for the item condition?`.
    <ls_step>-reasoninginstruction     = `Analyze the 'item_condition' attribute. If 'Damaged', assign to Blocked Stock (S). If 'New', assign to Unrestricted.`.
    <ls_step>-reasoningstepismandatory = abap_true.

    APPEND INITIAL LINE TO rt_reasoning_step ASSIGNING <ls_step>.
    <ls_step>-reasoningstepname        = `REFUND_ELIGIBILITY`.
    <ls_step>-reasoningstepquestion    = `Should an immediate credit memo be issued or a replacement order created?`.
    <ls_step>-reasoninginstruction     = `Check customer master 'KNA1' for VIP status. VIPs get immediate credit; Standard customers wait for QI approval.`.
    <ls_step>-reasoningstepismandatory = abap_false.
  ENDMETHOD.

  METHOD set_technical_rules.
    APPEND INITIAL LINE TO rt_tech_rules ASSIGNING FIELD-SYMBOL(<ls_tech_rules>).
    <ls_tech_rules>-technicalrulesname = `LOCKING_PROTOCOL`.
    <ls_tech_rules>-technicalrule      = `Ensure exclusive lock on RMA_ID before processing to prevent race conditions.`.

    APPEND INITIAL LINE TO rt_tech_rules ASSIGNING <ls_tech_rules>.
    <ls_tech_rules>-technicalrulesname = `MAX_PAYLOAD_SIZE`.
    <ls_tech_rules>-technicalrule      = `IF xstring_length( iv_attachment ) > 10485760, REJECT_UPLOAD 'Attachment exceeds 10MB limit'.`.

    APPEND INITIAL LINE TO rt_tech_rules ASSIGNING <ls_tech_rules>.
    <ls_tech_rules>-technicalrulesname = `EXTERNAL_SERVICE_TIMEOUT`.
    <ls_tech_rules>-technicalrule      = `Call to Address_Validation_Service must fail-soft if response exceeds 2000ms.`.

    APPEND INITIAL LINE TO rt_tech_rules ASSIGNING <ls_tech_rules>.
    <ls_tech_rules>-technicalrulesname = `INPUT_SANITIZATION`.
    <ls_tech_rules>-technicalrule      = `Use cl_abap_doclib_util=>escape_xss for any comment fields rendered in external web views.`.

    APPEND INITIAL LINE TO rt_tech_rules ASSIGNING <ls_tech_rules>.
    <ls_tech_rules>-technicalrulesname = `DESTINATION_VALIDATION`.
    <ls_tech_rules>-technicalrule      = `If ping fails, route GR data to local recovery table (ZGR_QUEUE).`.
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
    DATA lv_json_input     TYPE zpru_if_agent_frw=>ts_json.

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

    ls_prompt-string_content = lv_json_input.

    lo_nested_agent->execute_agent( EXPORTING iv_agent_name          = 'NESTED_AGENT'
                                              is_prompt              = ls_prompt
                                              io_parent_controller   = io_controller
                                    IMPORTING ev_final_response      = lv_final_response
                                    " TODO: variable is assigned but never used (ABAP cleaner)
                                              eo_executed_controller = DATA(lo_nested_controler) ).

    ASSIGN eo_output->* TO <ls_safety_response>.
    IF sy-subrc <> 0.
      ev_error_flag = abap_true.
    ENDIF.

    lo_util->convert_to_abap( EXPORTING ir_string = REF #( lv_final_response )
                              CHANGING  cr_abap   = ls_final_response ).

    IF lo_util->is_wrapped_in_json_markdown( ls_final_response-finalresponsebody-responsecontent ) = abap_true.
      DATA(lv_response_content_json) = lo_util->unwrap_from_json_markdown(
                                           iv_markdown = ls_final_response-finalresponsebody-responsecontent ).
    ELSE.
      ev_error_flag = abap_true.
      RETURN.
    ENDIF.

    lo_util->convert_to_abap( EXPORTING ir_string = REF #( lv_response_content_json )
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
