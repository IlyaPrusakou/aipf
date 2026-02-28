CLASS zpru_cl_decision_provider DEFINITION
  PUBLIC ABSTRACT
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zpru_if_agent_frw.
    INTERFACES zpru_if_decision_provider.

  PROTECTED SECTION.
    METHODS check_authorizations    ABSTRACT
      IMPORTING is_agent               TYPE zpru_if_adf_type_and_constant=>ts_agent
                it_tool                TYPE zpru_if_adf_type_and_constant=>tt_agent_tool
                io_controller          TYPE REF TO zpru_if_agent_controller
                io_input               TYPE REF TO zpru_if_payload
                io_system_prompt       TYPE REF TO zpru_if_prompt_provider       OPTIONAL
                io_short_memory        TYPE REF TO zpru_if_short_memory_provider OPTIONAL
                io_long_memory         TYPE REF TO zpru_if_long_memory_provider  OPTIONAL
                io_agent_info_provider TYPE REF TO zpru_if_agent_info_provider   OPTIONAL
      EXPORTING ev_allowed             TYPE abap_boolean
      CHANGING  cs_decision_log        TYPE zpru_s_decision_log
      RAISING   zpru_cx_agent_core.

    METHODS recall_memory ABSTRACT
      IMPORTING is_agent                   TYPE zpru_if_adf_type_and_constant=>ts_agent
                it_tool                    TYPE zpru_if_adf_type_and_constant=>tt_agent_tool
                io_controller              TYPE REF TO zpru_if_agent_controller
                io_input                   TYPE REF TO zpru_if_payload
                io_system_prompt           TYPE REF TO zpru_if_prompt_provider OPTIONAL
                io_short_memory            TYPE REF TO zpru_if_short_memory_provider OPTIONAL
                io_long_memory             TYPE REF TO zpru_if_long_memory_provider OPTIONAL
                io_agent_info_provider     TYPE REF TO zpru_if_agent_info_provider OPTIONAL
      EXPORTING et_session_memory          TYPE zpru_if_short_memory_provider=>tt_message
                et_episodic_message_memory TYPE zpru_tt_export_mem_msg
                et_episodic_summary_memory TYPE zpru_tt_export_mem_sum
                et_semantic_memory         TYPE zpru_tt_semantic_memory_comb
      CHANGING  cs_decision_log            TYPE zpru_s_decision_log
      RAISING   zpru_cx_agent_core.

    METHODS read_data_4_thinking ABSTRACT
      IMPORTING is_agent               TYPE zpru_if_adf_type_and_constant=>ts_agent
                it_tool                TYPE zpru_if_adf_type_and_constant=>tt_agent_tool
                io_controller          TYPE REF TO zpru_if_agent_controller
                io_input               TYPE REF TO zpru_if_payload
                io_system_prompt       TYPE REF TO zpru_if_prompt_provider       OPTIONAL
                io_short_memory        TYPE REF TO zpru_if_short_memory_provider OPTIONAL
                io_long_memory         TYPE REF TO zpru_if_long_memory_provider  OPTIONAL
                io_agent_info_provider TYPE REF TO zpru_if_agent_info_provider   OPTIONAL
      EXPORTING et_rag_data            TYPE zpru_tt_rag_header
                ev_user_data           TYPE zpru_de_json
      CHANGING  cs_decision_log        TYPE zpru_s_decision_log
      RAISING   zpru_cx_agent_core.

    METHODS process_thinking ABSTRACT
      IMPORTING is_agent                   TYPE zpru_if_adf_type_and_constant=>ts_agent
                it_tool                    TYPE zpru_if_adf_type_and_constant=>tt_agent_tool
                io_controller              TYPE REF TO zpru_if_agent_controller
                io_input                   TYPE REF TO zpru_if_payload
                io_decision_request        TYPE REF TO zpru_if_decision_request
                io_system_prompt           TYPE REF TO zpru_if_prompt_provider            OPTIONAL
                io_short_memory            TYPE REF TO zpru_if_short_memory_provider      OPTIONAL
                io_long_memory             TYPE REF TO zpru_if_long_memory_provider       OPTIONAL
                io_agent_info_provider     TYPE REF TO zpru_if_agent_info_provider        OPTIONAL
                it_session_memory          TYPE zpru_if_short_memory_provider=>tt_message OPTIONAL
                it_episodic_message_memory TYPE zpru_tt_export_mem_msg                    OPTIONAL
                it_episodic_summary_memory TYPE zpru_tt_export_mem_sum                    OPTIONAL
                it_semantic_memory         TYPE zpru_tt_semantic_memory_comb              OPTIONAL
                it_rag_data                TYPE zpru_tt_rag_header                        OPTIONAL
                iv_user_data               TYPE zpru_de_json                              OPTIONAL
      EXPORTING et_execution_plan          TYPE zpru_if_decision_provider=>tt_execution_plan
                ev_langu                   TYPE sylangu
      CHANGING  cs_decision_log            TYPE zpru_s_decision_log
      RAISING   zpru_cx_agent_core.

    METHODS prepare_first_tool_input ABSTRACT
      IMPORTING is_agent                   TYPE zpru_if_adf_type_and_constant=>ts_agent
                it_tool                    TYPE zpru_if_adf_type_and_constant=>tt_agent_tool
                io_controller              TYPE REF TO zpru_if_agent_controller
                io_input                   TYPE REF TO zpru_if_payload
                io_system_prompt           TYPE REF TO zpru_if_prompt_provider            OPTIONAL
                io_short_memory            TYPE REF TO zpru_if_short_memory_provider      OPTIONAL
                io_long_memory             TYPE REF TO zpru_if_long_memory_provider       OPTIONAL
                io_agent_info_provider     TYPE REF TO zpru_if_agent_info_provider        OPTIONAL
                it_session_memory          TYPE zpru_if_short_memory_provider=>tt_message OPTIONAL
                it_episodic_message_memory TYPE zpru_tt_export_mem_msg                    OPTIONAL
                it_episodic_summary_memory TYPE zpru_tt_export_mem_sum                    OPTIONAL
                it_semantic_memory         TYPE zpru_tt_semantic_memory_comb              OPTIONAL
                it_rag_data                TYPE zpru_tt_rag_header                        OPTIONAL
                iv_user_data               TYPE zpru_de_json                              OPTIONAL
      EXPORTING er_first_tool_input        TYPE REF TO data
      CHANGING  cs_decision_log            TYPE zpru_s_decision_log
      RAISING   zpru_cx_agent_core.

    METHODS set_model_id ABSTRACT
      RETURNING VALUE(rv_model_id) TYPE char100.

    METHODS set_result_comment ABSTRACT
      RETURNING VALUE(rv_result_comment) TYPE string.

    METHODS set_final_response_content ABSTRACT
      IMPORTING iv_run_uuid            TYPE sysuuid_x16
                iv_query_uuid          TYPE sysuuid_x16
                io_controller          TYPE REF TO zpru_if_agent_controller
                io_last_output         TYPE REF TO zpru_if_payload OPTIONAL
      CHANGING  cs_final_response_body TYPE zpru_s_final_response_body
      RAISING   zpru_cx_agent_core.

    METHODS set_final_response_metadata ABSTRACT
      IMPORTING iv_run_uuid        TYPE sysuuid_x16
                iv_query_uuid      TYPE sysuuid_x16
                io_controller      TYPE REF TO zpru_if_agent_controller
                io_last_output     TYPE REF TO zpru_if_payload OPTIONAL
      CHANGING  cs_reasoning_trace TYPE zpru_s_reasoning_trace
      RAISING   zpru_cx_agent_core.

    METHODS get_timestamp
      RETURNING VALUE(rv_now) TYPE timestampl.

    METHODS get_last_thinkingstepnumber
      IMPORTING it_thinking_step                  TYPE zpru_tt_thinking_step
      RETURNING VALUE(rv_last_thinkingstepnumber) TYPE i.

  PRIVATE SECTION.
ENDCLASS.


CLASS zpru_cl_decision_provider IMPLEMENTATION.
  METHOD zpru_if_decision_provider~call_decision_engine.
    DATA ls_decision_log         TYPE zpru_s_decision_log.
    DATA lv_decision_log_string  TYPE string.
    DATA lv_first_input_string   TYPE string.
    DATA lo_tool_schema_provider TYPE REF TO zpru_if_tool_schema_provider.
    DATA lr_first_input          TYPE REF TO data.
    DATA lo_decision_request     TYPE REF TO zpru_if_decision_request.
    DATA ls_decision_request     TYPE zpru_s_decision_request.
    DATA lo_agent_info_provider  TYPE REF TO zpru_if_agent_info_provider.
    DATA lo_syst_prompt_provider TYPE REF TO zpru_if_prompt_provider.
    DATA lo_tool_info_provider   TYPE REF TO zpru_if_tool_info_provider.
    DATA lo_util                 TYPE REF TO zpru_if_agent_util.

    ls_decision_log-agentuuid            = is_agent-agentuuid.
    ls_decision_log-modelid              = set_model_id( ).
    ls_decision_log-inputprompt          = io_input->get_data( )->*.
    ls_decision_log-thinkigstartdatetime = get_timestamp( ).

    check_authorizations( EXPORTING is_agent               = is_agent
                                    it_tool                = it_tool
                                    io_controller          = io_controller
                                    io_input               = io_input
                                    io_system_prompt       = io_system_prompt
                                    io_short_memory        = io_short_memory
                                    io_long_memory         = io_long_memory
                                    io_agent_info_provider = io_agent_info_provider
                          IMPORTING ev_allowed             = DATA(lv_allowed)
                          CHANGING  cs_decision_log        = ls_decision_log ).

    IF lv_allowed = abap_false.
      RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDIF.

    APPEND INITIAL LINE TO ls_decision_log-thinkingsteps ASSIGNING FIELD-SYMBOL(<ls_thinking_step>).
    <ls_thinking_step>-thinkingstepnumber   = 1.
    <ls_thinking_step>-thinkingstepdatetime = get_timestamp( ).
    <ls_thinking_step>-thinkingstepcontent  = `Decision authorizaton is passed successfully`.

    recall_memory( EXPORTING is_agent                   = is_agent
                             it_tool                    = it_tool
                             io_controller              = io_controller
                             io_input                   = io_input
                             io_system_prompt           = io_system_prompt
                             io_short_memory            = io_short_memory
                             io_long_memory             = io_long_memory
                             io_agent_info_provider     = io_agent_info_provider
                   IMPORTING et_session_memory          = DATA(lt_session_memory)
                             et_episodic_message_memory = DATA(lt_episodic_message_memory)
                             et_episodic_summary_memory = DATA(lt_episodic_summary_memory)
                             et_semantic_memory         = DATA(lt_semantic_memory)
                   CHANGING  cs_decision_log            = ls_decision_log ).

    APPEND INITIAL LINE TO ls_decision_log-thinkingsteps ASSIGNING <ls_thinking_step>.
    <ls_thinking_step>-thinkingstepnumber   = get_last_thinkingstepnumber( ls_decision_log-thinkingsteps ).
    <ls_thinking_step>-thinkingstepdatetime = get_timestamp( ).
    <ls_thinking_step>-thinkingstepcontent  = `Recall memory is finished`.

    read_data_4_thinking( EXPORTING is_agent               = is_agent
                                    it_tool                = it_tool
                                    io_controller          = io_controller
                                    io_input               = io_input
                                    io_system_prompt       = io_system_prompt
                                    io_short_memory        = io_short_memory
                                    io_long_memory         = io_long_memory
                                    io_agent_info_provider = io_agent_info_provider
                          IMPORTING et_rag_data            = DATA(lt_rag_data)
                                    ev_user_data           = DATA(lv_user_data)
                          CHANGING  cs_decision_log        = ls_decision_log ).

    APPEND INITIAL LINE TO ls_decision_log-thinkingsteps ASSIGNING <ls_thinking_step>.
    <ls_thinking_step>-thinkingstepnumber   = get_last_thinkingstepnumber( ls_decision_log-thinkingsteps ).
    <ls_thinking_step>-thinkingstepdatetime = get_timestamp( ).
    <ls_thinking_step>-thinkingstepcontent  = `Read data for thinking is finished`.

    lo_decision_request ?= zpru_cl_agent_service_mngr=>get_service(
                               iv_service = `ZPRU_IF_DECISION_REQUEST`
                               iv_context = zpru_if_agent_frw=>cs_context-standard ).

    CREATE OBJECT lo_agent_info_provider TYPE (is_agent-agentinfoprovider).
    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDIF.

    CREATE OBJECT lo_syst_prompt_provider TYPE (is_agent-systempromptprovider).
    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDIF.

    ls_decision_request-agentmetadata = lo_agent_info_provider->get_abap_agent_info( iv_agent_uuid = is_agent-agentuuid ).

    LOOP AT it_tool ASSIGNING FIELD-SYMBOL(<ls_tool>).

      CREATE OBJECT lo_tool_info_provider TYPE (<ls_tool>-toolinfoprovider).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      APPEND INITIAL LINE TO ls_decision_request-agentmetadata-agenttools ASSIGNING FIELD-SYMBOL(<ls_tool_metadata>).
      <ls_tool_metadata> = lo_tool_info_provider->get_abap_tool_info( is_tool_master_data = <ls_tool> ).

    ENDLOOP.

    ls_decision_request-systemprompt          = lo_syst_prompt_provider->get_abap_system_prompt( ).
    ls_decision_request-sessionmemory         = lt_session_memory.
    ls_decision_request-episodicmessagememory = lt_episodic_message_memory.
    ls_decision_request-episodicsummarymemory = lt_episodic_summary_memory.
    ls_decision_request-semanticmemory        = lt_semantic_memory.
    ls_decision_request-ragdata               = lt_rag_data.
    ls_decision_request-userdata              = lv_user_data.
    ls_decision_request-userprompt            = io_input->get_data( )->*.

    lo_decision_request->zpru_if_payload~set_data( ir_data = NEW zpru_s_decision_request( ls_decision_request ) ).

    process_thinking( EXPORTING is_agent                   = is_agent
                                it_tool                    = it_tool
                                io_controller              = io_controller
                                io_input                   = io_input
                                io_decision_request        = lo_decision_request
                                io_system_prompt           = io_system_prompt
                                io_short_memory            = io_short_memory
                                io_long_memory             = io_long_memory
                                io_agent_info_provider     = io_agent_info_provider
                                it_session_memory          = lt_session_memory
                                it_episodic_message_memory = lt_episodic_message_memory
                                it_episodic_summary_memory = lt_episodic_summary_memory
                                it_semantic_memory         = lt_semantic_memory
                                it_rag_data                = lt_rag_data
                                iv_user_data               = lv_user_data
                      IMPORTING et_execution_plan          = DATA(lt_execution_plan)
                                ev_langu                   = DATA(lv_langu)
                      CHANGING  cs_decision_log            = ls_decision_log ).

    APPEND INITIAL LINE TO ls_decision_log-thinkingsteps ASSIGNING <ls_thinking_step>.
    <ls_thinking_step>-thinkingstepnumber   = get_last_thinkingstepnumber( ls_decision_log-thinkingsteps ).
    <ls_thinking_step>-thinkingstepdatetime = get_timestamp( ).
    <ls_thinking_step>-thinkingstepcontent  = `Thinking is finished`.

    IF eo_execution_plan IS NOT BOUND.
      eo_execution_plan ?= zpru_cl_agent_service_mngr=>get_service(
                               iv_service = `ZPRU_IF_PAYLOAD`
                               iv_context = zpru_if_agent_frw=>cs_context-standard ).
    ENDIF.
    eo_execution_plan->set_data( ir_data = NEW zpru_if_decision_provider=>tt_execution_plan( lt_execution_plan ) ).

    IF eo_langu IS NOT BOUND.
      eo_langu ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_PAYLOAD`
                                                           iv_context = zpru_if_agent_frw=>cs_context-standard ).
    ENDIF.
    eo_langu->set_data( ir_data = NEW sylangu( lv_langu ) ).

    ASSIGN lt_execution_plan[ sequence = 1 ] TO FIELD-SYMBOL(<ls_first_tool>).
    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDIF.

    ASSIGN it_tool[ agentuuid = <ls_first_tool>-agentuuid
                    toolname  = <ls_first_tool>-toolname ] TO FIELD-SYMBOL(<ls_tool_master_data>).
    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDIF.

    CREATE OBJECT lo_tool_schema_provider TYPE (<ls_tool_master_data>-toolschemaprovider).
    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDIF.

    DATA(lo_structure_input) = lo_tool_schema_provider->input_rtts_schema(
                                   is_tool_master_data = <ls_tool_master_data>
                                   is_execution_step   = VALUE #( tooluuid     = <ls_tool_master_data>-tooluuid
                                                                  stepsequence = <ls_first_tool>-sequence ) ).

    CREATE DATA lr_first_input TYPE HANDLE lo_structure_input.

    prepare_first_tool_input( EXPORTING is_agent                   = is_agent
                                        it_tool                    = it_tool
                                        io_controller              = io_controller
                                        io_input                   = io_input
                                        io_system_prompt           = io_system_prompt
                                        io_short_memory            = io_short_memory
                                        io_long_memory             = io_long_memory
                                        io_agent_info_provider     = io_agent_info_provider
                                        it_session_memory          = lt_session_memory
                                        it_episodic_message_memory = lt_episodic_message_memory
                                        it_episodic_summary_memory = lt_episodic_summary_memory
                                        it_semantic_memory         = lt_semantic_memory
                                        it_rag_data                = lt_rag_data
                                        iv_user_data               = lv_user_data
                              IMPORTING er_first_tool_input        = lr_first_input
                              CHANGING  cs_decision_log            = ls_decision_log ).

    APPEND INITIAL LINE TO ls_decision_log-thinkingsteps ASSIGNING <ls_thinking_step>.
    <ls_thinking_step>-thinkingstepnumber   = get_last_thinkingstepnumber( ls_decision_log-thinkingsteps ).
    <ls_thinking_step>-thinkingstepdatetime = get_timestamp( ).
    <ls_thinking_step>-thinkingstepcontent  = `Preparation of first input is finished`.

    lo_util ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_AGENT_UTIL`
                                                        iv_context = zpru_if_agent_frw=>cs_context-standard ).

    IF eo_first_tool_input IS NOT BOUND.
      eo_first_tool_input ?= zpru_cl_agent_service_mngr=>get_service(
                                 iv_service = `ZPRU_IF_PAYLOAD`
                                 iv_context = zpru_if_agent_frw=>cs_context-standard ).
    ENDIF.

    lo_util->convert_to_string( EXPORTING ir_abap   = lr_first_input
                                CHANGING  cr_string = lv_first_input_string ).

    eo_first_tool_input->set_data( ir_data = NEW string( lv_first_input_string ) ).

    ls_decision_log-resultcomment = set_result_comment( ).

    IF eo_decision_log IS NOT BOUND.
      eo_decision_log ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_PAYLOAD`
                                                                  iv_context = zpru_if_agent_frw=>cs_context-standard ).
    ENDIF.

    lo_util->convert_to_string( EXPORTING ir_abap   = REF #(  ls_decision_log )
                                CHANGING  cr_string = lv_decision_log_string ).

    eo_decision_log->set_data( ir_data = NEW string( lv_decision_log_string ) ).
  ENDMETHOD.

  METHOD zpru_if_decision_provider~prepare_final_response.
    DATA ls_final_response      TYPE zpru_s_final_response.
    DATA lv_final_response_json TYPE zpru_de_json.
    DATA lo_axc_service         TYPE REF TO zpru_if_axc_service.
    DATA lo_util                TYPE REF TO zpru_if_agent_util.

    lo_axc_service ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_AXC_SERVICE`
                                                               iv_context = zpru_if_agent_frw=>cs_context-standard ).

    lo_axc_service->read_header( EXPORTING it_head_read_k = VALUE #( ( runuuid                  = iv_run_uuid
                                                                       control-runuuid          = abap_true
                                                                       control-runid            = abap_true
                                                                       control-agentuuid        = abap_true
                                                                       control-userid           = abap_true
                                                                       control-runstartdatetime = abap_true
                                                                       control-runenddatetime   = abap_true
                                                                       control-createdby        = abap_true
                                                                       control-createdat        = abap_true
                                                                       control-changedby        = abap_true
                                                                       control-lastchanged      = abap_true
                                                                       control-locallastchanged = abap_true ) )
                                 IMPORTING et_axc_head    = DATA(lt_axc_head) ).

    lo_axc_service->read_query( EXPORTING it_query_read_k = VALUE #( ( queryuuid                   = iv_query_uuid
                                                                       control-runuuid             = abap_true
                                                                       control-querynumber         = abap_true
                                                                       control-queryuuid           = abap_true
                                                                       control-querylanguage       = abap_true
                                                                       control-querystatus         = abap_true
                                                                       control-querystartdatetime  = abap_true
                                                                       control-queryenddatetime    = abap_true
                                                                       control-queryinputprompt    = abap_true
                                                                       control-querydecisionlog    = abap_true
                                                                       control-queryoutputresponse = abap_true ) )
                                IMPORTING et_axc_query    = DATA(lt_axc_query) ).

    ASSIGN lt_axc_head[ 1 ] TO FIELD-SYMBOL(<ls_axc_head>).
    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDIF.

    ASSIGN lt_axc_query[ 1 ] TO FIELD-SYMBOL(<ls_axc_query>).
    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDIF.

    TRY.
        ls_final_response-finalresponseheader-responseid = cl_system_uuid=>create_uuid_x16_static( ).
      CATCH cx_uuid_error.
        RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDTRY.

    ls_final_response-finalresponseheader-generationdatetime = get_timestamp( ).
    ls_final_response-finalresponseheader-agentuuid          = <ls_axc_head>-agentuuid.
    ls_final_response-finalresponseheader-runuuid            = <ls_axc_head>-runuuid.
    ls_final_response-finalresponseheader-queryuuid          = <ls_axc_query>-queryuuid.

    set_final_response_content( EXPORTING iv_run_uuid            = iv_run_uuid
                                          iv_query_uuid          = iv_query_uuid
                                          io_controller          = io_controller
                                          io_last_output         = io_last_output
                                CHANGING  cs_final_response_body = ls_final_response-finalresponsebody  ).

    set_final_response_metadata( EXPORTING iv_run_uuid        = iv_run_uuid
                                           iv_query_uuid      = iv_query_uuid
                                           io_controller      = io_controller
                                           io_last_output     = io_last_output
                                 CHANGING  cs_reasoning_trace = ls_final_response-reasoning_trace ).

    lo_util ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_AGENT_UTIL`
                                                        iv_context = zpru_if_agent_frw=>cs_context-standard ).

    lo_util->convert_to_string( EXPORTING ir_abap   = NEW zpru_s_final_response( ls_final_response )
                                CHANGING  cr_string = lv_final_response_json ).

    IF eo_final_response IS NOT BOUND.
      eo_final_response ?= zpru_cl_agent_service_mngr=>get_service(
                               iv_service = `ZPRU_IF_PAYLOAD`
                               iv_context = zpru_if_agent_frw=>cs_context-standard ).
    ENDIF.

    eo_final_response->set_data( ir_data = NEW zpru_de_json( lv_final_response_json ) ).
  ENDMETHOD.

  METHOD get_timestamp.
    GET TIME STAMP FIELD rv_now.
  ENDMETHOD.

  METHOD get_last_thinkingstepnumber.
    DATA(lt_thinking_step) = it_thinking_step.
    SORT lt_thinking_step BY thinkingstepnumber DESCENDING.
    rv_last_thinkingstepnumber = VALUE i( lt_thinking_step[ 1 ]-thinkingstepnumber OPTIONAL ) + 1.
  ENDMETHOD.
ENDCLASS.
