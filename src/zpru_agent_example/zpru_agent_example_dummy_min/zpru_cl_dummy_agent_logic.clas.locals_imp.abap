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
*  DATA: ls_header         TYPE bapi2017_gm_head_01,
*        ls_code           TYPE bapi2017_gm_code,
*        lt_items          TYPE TABLE OF bapi2017_gm_item_create,
*        lt_return         TYPE TABLE OF bapiret2,
*        lv_mat_doc        TYPE mblnr.
*
*
*  ls_header-pstng_date = sy-datum.
*  ls_header-doc_date   = sy-datum.
*  ls_header-header_txt = |RMA:{ iv_rma_id }|.
*
*
*  ls_code-gm_code = '01'.
*
*
*  APPEND INITIAL LINE TO lt_items ASSIGNING FIELD-SYMBOL(<ls_item>).
*  <ls_item>-material  = iv_material.
*  <ls_item>-plant     = iv_plant.
*  <ls_item>-stge_loc  = iv_storage_location.
*  <ls_item>-move_type = '651'.
*  <ls_item>-entry_qnt = iv_quantity.
*  <ls_item>-entry_uom = iv_uom.
*
*  CALL FUNCTION 'BAPI_GOODSMVT_CREATE'
*    EXPORTING
*      goodsmvt_header  = ls_header
*      goodsmvt_code    = ls_code
*    IMPORTING
*      materialdocument = lv_mat_doc
*    TABLES
*      goodsmvt_item    = lt_items
*      return           = lt_return.
*
*  IF lv_mat_doc IS NOT INITIAL.
*    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' EXPORTING wait = abap_true.
*    ev_status = |SUCCESS: Material Document { lv_mat_doc } posted.|.
*  ELSE.
*
*    ev_status = |FAILED: | && lt_return[ 1 ]-message.
*  ENDIF.
*
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
*    send_via_url( EXPORTING io_controller = io_controller
*                            io_request    = io_request
*                  IMPORTING eo_response   = eo_response
*                            ev_error_flag = ev_error_flag ).
  ENDMETHOD.

  METHOD send_via_url.
    DATA lv_url         TYPE string.
    DATA lo_http_client TYPE REF TO if_web_http_client.
    DATA lo_response    TYPE REF TO if_web_http_response.
    DATA lo_util        TYPE REF TO zpru_if_agent_util.

    lv_url = 'https://www.youtube.com/watch?v=bkCQK-rROWk'.

    TRY.

        lo_http_client = get_http_client( lv_url ).

        lo_http_client->get_http_request( )->set_header_fields(
            i_fields = VALUE #( value = if_web_http_header=>accept_application_json
                                ( name = if_web_http_header=>content_type )
                                ( name = if_web_http_header=>accept ) ) ).

        lo_response = lo_http_client->execute( if_web_http_client=>get ).

        DATA(lv_status) = lo_response->get_status( ).
        IF lv_status-code <> '200'.
          " raise exception
        ENDIF.

        DATA(lv_response_json) = lo_response->get_text( ).

        DATA(lv_input_json) = io_request->get_data( ).

        TRY.
            lo_util ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_AGENT_UTIL`
                                                                iv_context = zpru_if_agent_frw=>cs_context-standard ).
          CATCH zpru_cx_agent_core.
            RETURN.
        ENDTRY.

        DATA(lv_output) = lo_util->append_json_to_json( iv_field_4_append = 'http_result'
                                                        iv_json_4_append  = lv_response_json
                                                        iv_json_target    = lv_input_json->*  ).

        eo_response->set_data( ir_data = NEW string( lv_output ) ).

      CATCH cx_http_dest_provider_error
            cx_web_http_client_error.
    ENDTRY.
  ENDMETHOD.

  METHOD get_http_client.
    DATA lo_http_destination TYPE REF TO if_http_destination.

    IF zpru_cl_logic_switch=>get_logic( ) = abap_true.
      ro_http_client = NEW zpru_cl_web_http_client( ).
    ELSE.
      TRY.
          lo_http_destination = cl_http_destination_provider=>create_by_url( i_url = iv_url ).
          ro_http_client = cl_web_http_client_manager=>create_by_http_destination(
                               i_destination = lo_http_destination ).
        CATCH cx_http_dest_provider_error
              cx_web_http_client_error.
      ENDTRY.

    ENDIF.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_adf_service_cons_mdl_tool IMPLEMENTATION.
  METHOD consume_service_model_int.
    DATA lt_business_data         TYPE TABLE OF zpru_storage_bin=>tys_warehouse_storage_bin_type.
    DATA lo_http_client           TYPE REF TO if_web_http_client.
    DATA lo_client_proxy          TYPE REF TO /iwbep/if_cp_client_proxy.
    DATA lo_request               TYPE REF TO /iwbep/if_cp_request_read_list.
    DATA lo_response              TYPE REF TO /iwbep/if_cp_response_read_lst.
    DATA lo_filter_factory        TYPE REF TO /iwbep/if_cp_filter_factory.
    DATA lo_filter_node_1         TYPE REF TO /iwbep/if_cp_filter_node.
    DATA lo_filter_node_2         TYPE REF TO /iwbep/if_cp_filter_node.
    DATA lo_filter_node_root      TYPE REF TO /iwbep/if_cp_filter_node.
    DATA lt_range_ewmwarehouse    TYPE RANGE OF char4.
    DATA lt_range_ewmstorage_bin  TYPE RANGE OF char18.
    DATA lv_comm_scenario         TYPE if_com_management=>ty_cscn_id.
    DATA lv_service_id            TYPE if_com_management=>ty_cscn_outb_srv_id.
    DATA lv_comm_system_id        TYPE if_com_management=>ty_cs_id.
    DATA lv_repository_id         TYPE /iwbep/if_cp_runtime_types=>ty_proxy_model_repo_id.
    DATA lv_proxy_model_id        TYPE /iwbep/if_cp_runtime_types=>ty_proxy_model_id.
    DATA lv_proxy_model_version   TYPE /iwbep/if_cp_runtime_types=>ty_proxy_model_version.
    DATA lv_relative_service_root TYPE string.
    DATA lo_util                  TYPE REF TO zpru_if_agent_util.
    DATA lv_response_json         TYPE string.

    lv_comm_scenario = 'SAP_COM_0550'.
    lv_service_id = 'API_WHSE_STORAGE_BIN_2'.
    lv_comm_system_id = 'S4H_EXT_SYSTEM'.
    lv_repository_id = 'DEFAULT'.
    lv_proxy_model_id = 'ZPRU_STORAGE_BIN'.
    lv_proxy_model_version = '0001'.
    lv_relative_service_root = '/sap/opu/odata4/sap/api_whse_storage_bin_2/srvd_a2x/sap/warehousestoragebin/0001'.

    TRY.
        DATA(lo_destination) = cl_http_destination_provider=>create_by_comm_arrangement(
                                   comm_scenario  = lv_comm_scenario
                                   comm_system_id = lv_comm_system_id
                                   service_id     = lv_service_id ).
        lo_http_client = cl_web_http_client_manager=>create_by_http_destination( lo_destination ).
        lo_client_proxy = /iwbep/cl_cp_factory_remote=>create_v4_remote_proxy(
                              is_proxy_model_key       = VALUE #( repository_id       = lv_repository_id
                                                                  proxy_model_id      = lv_proxy_model_id
                                                                  proxy_model_version = lv_proxy_model_version )
                              io_http_client           = lo_http_client
                              iv_relative_service_root = lv_relative_service_root ).
        ASSERT lo_http_client IS BOUND.

        lo_request = lo_client_proxy->create_resource_for_entity_set( 'WAREHOUSE_STORAGE_BIN' )->create_request_for_read( ).

        lo_filter_factory = lo_request->create_filter_factory( ).
        lo_filter_node_1  = lo_filter_factory->create_by_range( iv_property_path = 'EWMWAREHOUSE'
                                                                it_range         = lt_range_ewmwarehouse ).
        lo_filter_node_2  = lo_filter_factory->create_by_range( iv_property_path = 'EWMSTORAGE_BIN'
                                                                it_range         = lt_range_ewmstorage_bin ).
        lo_filter_node_root = lo_filter_node_1->and( lo_filter_node_2 ).
        lo_request->set_filter( lo_filter_node_root ).
        lo_request->set_top( 50 )->set_skip( 0 ).

        lo_response = lo_request->execute( ).
        lo_response->get_business_data( IMPORTING et_business_data = lt_business_data ).

*        DATA(lv_input_json) = io_request->get_data( ).

        TRY.
            lo_util ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_AGENT_UTIL`
                                                                iv_context = zpru_if_agent_frw=>cs_context-standard ).
          CATCH zpru_cx_agent_core.
            RETURN.
        ENDTRY.

        lo_util->convert_to_string( EXPORTING ir_abap   = REF #( lt_business_data )
                                    CHANGING  cr_string = lv_response_json ).

*        DATA(lv_output) = lo_util->append_json_to_json( iv_field_4_append = 'csm_result'
*                                                        iv_json_4_append  = lv_response_json
*                                                        iv_json_target    = lv_input_json->*  ).

*        eo_response->set_data( ir_data = NEW string( lv_output ) ).

      CATCH /iwbep/cx_cp_remote INTO DATA(lx_remote).
        RAISE SHORTDUMP lx_remote.
      CATCH /iwbep/cx_gateway INTO DATA(lx_gateway).
        RAISE SHORTDUMP lx_gateway.
      CATCH cx_http_dest_provider_error INTO DATA(lx_dest_provider_error).
        RAISE SHORTDUMP lx_dest_provider_error.
      CATCH cx_web_http_client_error INTO DATA(lx_web_http_client_error).
        RAISE SHORTDUMP lx_web_http_client_error.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_adf_call_llm_tool IMPLEMENTATION.
  METHOD call_large_language_model_int.
*    DATA lo_llm_api TYPE REF TO if_aic_completion_api.
*    DATA lo_message TYPE REF TO if_aic_message_container.

*    preprocess_llm_request( EXPORTING iv_islm_scenario = `MY_GEMINI_3`
*                                      io_controller    = io_controller
*                                      io_request       = io_request
*                            IMPORTING eo_message       = lo_message
*                                      eo_llm_api       = lo_llm_api
*                                      ev_error_flag    = ev_error_flag ).

    IF ev_error_flag = abap_true.
      RETURN.
    ENDIF.

*    process_llm_request( EXPORTING io_controller = io_controller
*                                   io_request    = io_request
*                                   io_message    = lo_message
*                                   io_llm_api    = lo_llm_api
*                         IMPORTING eo_response   = eo_response
*                                   ev_error_flag = ev_error_flag ).
  ENDMETHOD.

  METHOD prepare_prompt.
    ro_message = io_llm_api->create_message_container( ).
    ro_message->set_system_role( iv_system_role ).
    ro_message->add_user_message( iv_user_message ).
    ro_message->add_assistant_message( iv_assistant_message ).
    ro_message->add_user_message( iv_user_message_2 ).
  ENDMETHOD.

  METHOD get_response_schema.
    " {
    "   "type": "object",
    "   "properties": {
    "     "explanation": {
    "       "type": "string",
    "       "description": "Short technical explanation of the solution."
    "     },
    "     "abap_code": {
    "       "type": "string",
    "       "description": "The executable ABAP code block."
    "     },
    "     "objects_used": {
    "       "type": "array",
    "       "items": { "type": "string" },
    "       "description": "List of SAP standard tables or classes mentioned."
    "     },
    "     "confidence_score": {
    "       "type": "number",
    "       "description": "Model's certainty in this answer from 0 to 1."
    "     }
    "   },
    "   "required": ["explanation", "abap_code", "objects_used"]
    " }

    rv_response_schema =
       |\{ | &&
       |  "type": "object", | &&
       |  "properties": \{ | &&
       |    "explanation": \{ "type": "string" \}, | &&
       |    "abap_code": \{ "type": "string" \}, | &&
       |    "objects_used": \{ "type": "array", "items": \{ "type": "string" \} \} | &&
       |  \}, | &&
       |  "required": ["explanation", "abap_code", "objects_used"] | &&
       |\}|.
  ENDMETHOD.

  METHOD preprocess_llm_request.
    " TODO: parameter IO_CONTROLLER is never used (ABAP cleaner)
    " TODO: parameter IO_REQUEST is never used (ABAP cleaner)

    DATA lo_llm_parameter TYPE REF TO if_aic_completion_parameters.

    CLEAR: eo_message,
           eo_llm_api.

    ev_error_flag = abap_false.

    DATA(lo_llm_api_factory) = lcl_common_algorithms=>get_llm_api_factory( ).

    TRY.
        eo_llm_api = lo_llm_api_factory->create_instance( iv_islm_scenario ).
      CATCH cx_aic_api_factory.
        ev_error_flag = abap_true.
        RETURN.
    ENDTRY.

    IF eo_llm_api IS NOT BOUND.
      ev_error_flag = abap_true.
      RETURN.
    ENDIF.

    eo_message = prepare_prompt( io_llm_api           = eo_llm_api
                                 iv_system_role       = `You are an ABAP expert`
                                 iv_user_message      = `Does ABAP support OO programming?`
                                 iv_assistant_message = `Yes`
                                 iv_user_message_2    = `Can you build RESTful applications in ABAP?` ).

    lo_llm_parameter = eo_llm_api->get_parameter_setter( ).
    lo_llm_parameter->set_temperature( `1.0` ).
    lo_llm_parameter->set_maximum_tokens( 2000 ).
    lo_llm_parameter->set_any_parameter( name  = `responseMimeType`
                                         value = `application/json` ).
    lo_llm_parameter->set_any_parameter( name  = `thinking_level`
                                         value = `high` ).

    DATA(lv_schema) = get_response_schema( ).
    lo_llm_parameter->set_any_parameter( name  = `responseSchema`
                                         value = lv_schema ).

    lo_llm_parameter->set_any_parameter( name  = `tools`
                                         value = `[{ "google_search": {} }]` ).
  ENDMETHOD.

  METHOD process_llm_request.
    " TODO: parameter IO_CONTROLLER is never used (ABAP cleaner)
    " TODO: parameter EO_RESPONSE is never cleared or assigned (ABAP cleaner)
    " TODO: parameter EV_ERROR_FLAG is never cleared or assigned (ABAP cleaner)

    DATA ls_result_payload TYPE ts_result_payload.
    DATA lo_llm_result     TYPE REF TO if_aic_completion_api_result.
    DATA lo_util           TYPE REF TO zpru_if_agent_util.
    DATA lv_json_2_append  TYPE zpru_if_agent_frw=>ts_json.

    TRY.
        lo_llm_result = io_llm_api->execute_for_messages( io_message ).
        ls_result_payload-llm_response = lo_llm_result->get_completion( ).

      CATCH cx_aic_completion_api.
        RETURN.
    ENDTRY.

    ls_result_payload-llm_total_tokens           = lo_llm_result->get_total_token_count( ).
    ls_result_payload-llm_finish_reason          = lo_llm_result->get_finish_reason( ).
    ls_result_payload-llm_original_finish_reason = lo_llm_result->get_original_finish_reason( ).

    lo_util->convert_to_string( EXPORTING ir_abap   = REF #( ls_result_payload )
                                CHANGING  cr_string = lv_json_2_append ).

    DATA(lv_input_json) = io_request->get_data( ).

    TRY.
        lo_util ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_AGENT_UTIL`
                                                            iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RETURN.
    ENDTRY.

    DATA(lv_output) = lo_util->append_json_to_json( iv_field_4_append = 'llm_result'
                                                    iv_json_4_append  = lv_json_2_append
                                                    iv_json_target    = lv_input_json->*  ).

    eo_response->set_data( ir_data = NEW string( lv_output ) ).
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
*    IF zpru_cl_logic_switch=>get_logic( ) = abap_true.
*
*      process_dummy_email( EXPORTING io_controller = io_controller
*                                     io_request    = io_request
*                           IMPORTING eo_response   = eo_response
*                                     ev_error_flag = ev_error_flag ).
*
*    ELSE.
*      process_prod_email( EXPORTING io_controller = io_controller
*                                    io_request    = io_request
*                          IMPORTING eo_response   = eo_response
*                                    ev_error_flag = ev_error_flag ).
*
*    ENDIF.
  ENDMETHOD.

  METHOD process_dummy_email.
    " TODO: parameter IO_CONTROLLER is never used (ABAP cleaner)
    " TODO: parameter EO_RESPONSE is never cleared or assigned (ABAP cleaner)

    DATA lv_sender       TYPE cl_bcs_mail_message=>ty_address.
    DATA lv_recipient    TYPE cl_bcs_mail_message=>ty_address.
    DATA lv_subject      TYPE cl_bcs_mail_message=>ty_subject.
    DATA lv_content      TYPE string.
    DATA lv_content_type TYPE cl_bcs_mail_bodypart=>ty_content_type.
    DATA lo_util         TYPE REF TO zpru_if_agent_util.
    DATA lv_input        TYPE zpru_if_agent_frw=>ts_json.

    ev_error_flag = abap_false.

    TRY.
        lo_util ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_AGENT_UTIL`
                                                            iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RETURN.
    ENDTRY.

    lv_input =  io_request->get_data( )->*.

    lv_sender = lo_util->search_node_in_json( iv_json           = lv_input
                                              iv_field_2_search = 'sender' ).

    lv_recipient = lo_util->search_node_in_json( iv_json           = lv_input
                                                 iv_field_2_search = 'recipient' ).

    lv_subject = lo_util->search_node_in_json( iv_json           = lv_input
                                               iv_field_2_search = 'subject' ).

    lv_content = lo_util->search_node_in_json( iv_json           = lv_input
                                               iv_field_2_search = 'content' ).

    lv_content = lo_util->search_node_in_json( iv_json           = lv_input
                                               iv_field_2_search = 'content_type' ).

    TRY.
        prepare_dummy_email( EXPORTING iv_sender       = lv_sender
                                       iv_recipient    = lv_recipient
                                       iv_subject      = lv_subject
                                       iv_content      = lv_content
                                       iv_content_type = lv_content_type
                             IMPORTING eo_mail_manager = DATA(lo_mail_manager) ).

        lo_mail_manager->send( IMPORTING ev_mail_status = DATA(lv_email_status) ).

        IF lv_email_status = 'E'.
          ev_error_flag = abap_true.
          RETURN.
        ENDIF.

        DATA(lv_response) = |EMAIL TO { lv_recipient } HAS BEEN SENT SUCCESSFULLY|.

        DATA(lv_output) = lo_util->append_json_to_json( iv_field_4_append = 'email_response'
                                                        iv_json_4_append  = lv_response
                                                        iv_json_target    = lv_input ).

        eo_response->set_data( NEW string( lv_output ) ).

      CATCH cx_bcs_mail.
    ENDTRY.
  ENDMETHOD.

  METHOD prepare_dummy_email.
    TRY.
        eo_mail_manager = zpru_cl_bcs_mail_message=>create_instance( ).
        eo_mail_manager->set_sender( iv_sender ).
        eo_mail_manager->add_recipient( iv_recipient ).
        eo_mail_manager->set_subject( iv_subject ).

        DATA(lo_email_message) = zpru_cl_mail_bodypart=>create_instance( iv_content      = iv_content
                                                                         iv_content_type = iv_content_type ).

        eo_mail_manager->set_main( lo_email_message ).
      CATCH cx_bcs_mail.
    ENDTRY.
  ENDMETHOD.

  METHOD process_prod_email.
*    DATA lo_mail_manager TYPE REF TO cl_bcs_mail_message.
*    DATA lv_sender       TYPE cl_bcs_mail_message=>ty_address.
*    DATA lv_recipient    TYPE cl_bcs_mail_message=>ty_address.
*    DATA lv_subject      TYPE cl_bcs_mail_message=>ty_subject.
*    DATA lv_content      TYPE string.
*    DATA lv_content_type TYPE cl_bcs_mail_bodypart=>ty_content_type.
*
*    lv_sender = `my.email@gmail.com`.
*    lv_recipient = `your.email@gmail.com`.
*    lv_subject = `CMR processing`.
*    lv_content = `Let's discuss CMR process`.
*    lv_content_type = 'text/html'.
*
*
*    TRY.
*        lo_mail_manager = cl_bcs_mail_message=>create_instance( ).
*        lo_mail_manager->set_sender( lv_sender ).
*        lo_mail_manager->add_recipient( lv_recipient ).
*        lo_mail_manager->set_subject( lv_subject ).
*
*        DATA(lo_email_message) = cl_bcs_mail_textpart=>create_instance( iv_content      = lv_content
*                                                                        iv_content_type = lv_content_type ).
*
*        lo_mail_manager->set_main( lo_email_message ).
*
*        lo_mail_manager->send( IMPORTING et_status      = DATA(lt_status)
*                                         ev_mail_status = DATA(lv_email_status) ).
*
*      CATCH cx_bcs_mail.
*    ENDTRY.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_adf_tool_provider IMPLEMENTATION.
  METHOD provide_tool_instance.
    CASE is_tool_master_data-toolname.

      WHEN zpru_if_adf_type_and_constant=>cs_step_type-nested_agent.
        ro_executor = NEW lcl_adf_nested_agent( ).

      WHEN zpru_if_adf_type_and_constant=>cs_step_type-knowledge_source.
        ro_executor = NEW lcl_adf_knowledge_provider( ).

      WHEN zpru_if_adf_type_and_constant=>cs_step_type-abap_code.
        ro_executor = NEW lcl_adf_abap_executor( ).

      WHEN zpru_if_adf_type_and_constant=>cs_step_type-http_request.
        ro_executor = NEW lcl_adf_http_request_tool( ).

      WHEN zpru_if_adf_type_and_constant=>cs_step_type-service_consumption_model.
        ro_executor = NEW lcl_adf_service_cons_mdl_tool( ).

      WHEN zpru_if_adf_type_and_constant=>cs_step_type-call_llm.
        ro_executor = NEW lcl_adf_call_llm_tool( ).

      WHEN zpru_if_adf_type_and_constant=>cs_step_type-dynamic_abap_code.
        ro_executor = NEW lcl_adf_dynamic_abap_code_tool( ).

      WHEN zpru_if_adf_type_and_constant=>cs_step_type-infer_ml_model.
        ro_executor = NEW lcl_adf_ml_model_inference( ).

      WHEN zpru_if_adf_type_and_constant=>cs_step_type-user_tool.
        ro_executor = NEW lcl_adf_user_tool( ).

      WHEN OTHERS.
        RETURN.
    ENDCASE.
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
