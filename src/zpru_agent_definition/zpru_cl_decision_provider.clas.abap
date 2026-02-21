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
      CHANGING  cs_decision_log        TYPE zpru_s_decision_log.

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
      CHANGING  cs_decision_log            TYPE zpru_s_decision_log.

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
                eo_thinking_data       TYPE REF TO zpru_if_payload
      CHANGING  cs_decision_log        TYPE zpru_s_decision_log.

    METHODS process_thinking ABSTRACT
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
                io_thinking_data           TYPE REF TO zpru_if_payload                    OPTIONAL
      EXPORTING et_execution_plan          TYPE zpru_if_decision_provider=>tt_execution_plan
                ev_langu                   TYPE sylangu
      CHANGING  cs_decision_log            TYPE zpru_s_decision_log.

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
                io_thinking_data           TYPE REF TO zpru_if_payload                    OPTIONAL
      EXPORTING ev_first_tool_input        TYPE REF TO data
      CHANGING  cs_decision_log            TYPE zpru_s_decision_log.

    METHODS set_model_id ABSTRACT
      RETURNING VALUE(rv_model_id) TYPE char100.

    METHODS set_result_comment ABSTRACT
      RETURNING VALUE(rv_result_comment) TYPE string.

    METHODS precheck_final_response ABSTRACT.
    METHODS set_final_response_content ABSTRACT.
    METHODS set_final_response_metadata ABSTRACT.

    METHODS get_timestamp
      RETURNING VALUE(rv_now) TYPE timestampl.

    METHODS get_last_thinkingstepnumber
      IMPORTING it_thinking_step                  TYPE zpru_tt_thinking_step
      RETURNING VALUE(rv_last_thinkingstepnumber) TYPE i.

  PRIVATE SECTION.
ENDCLASS.


CLASS zpru_cl_decision_provider IMPLEMENTATION.
  METHOD zpru_if_decision_provider~call_decision_engine.
    DATA lo_thinking_data        TYPE REF TO zpru_if_payload.
    DATA ls_decision_log         TYPE zpru_s_decision_log.
    DATA lo_tool_schema_provider TYPE REF TO zpru_if_tool_schema_provider.
    DATA lr_first_input          TYPE REF TO data.

    ls_decision_log-agentUUID            = is_agent-agentuuid.
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

    TRY.
        lo_thinking_data ?= zpru_cl_agent_service_mngr=>get_service(
                                iv_service = `ZPRU_IF_PAYLOAD`
                                iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDTRY.

    read_data_4_thinking( EXPORTING is_agent               = is_agent
                                    it_tool                = it_tool
                                    io_controller          = io_controller
                                    io_input               = io_input
                                    io_system_prompt       = io_system_prompt
                                    io_short_memory        = io_short_memory
                                    io_long_memory         = io_long_memory
                                    io_agent_info_provider = io_agent_info_provider
                          IMPORTING et_rag_data            = DATA(lt_rag_data)
                                    eo_thinking_data       = lo_thinking_data
                          CHANGING  cs_decision_log        = ls_decision_log ).

    APPEND INITIAL LINE TO ls_decision_log-thinkingsteps ASSIGNING <ls_thinking_step>.
    <ls_thinking_step>-thinkingstepnumber   = get_last_thinkingstepnumber( ls_decision_log-thinkingsteps ).
    <ls_thinking_step>-thinkingstepdatetime = get_timestamp( ).
    <ls_thinking_step>-thinkingstepcontent  = `Read data for thinking is finished`.


*zpru_s_decision_request qqq

    process_thinking( EXPORTING is_agent                   = is_agent
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
                                io_thinking_data           = lo_thinking_data
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
                                        io_thinking_data           = lo_thinking_data
                              IMPORTING ev_first_tool_input        = lr_first_input
                              CHANGING  cs_decision_log            = ls_decision_log ).

    APPEND INITIAL LINE TO ls_decision_log-thinkingsteps ASSIGNING <ls_thinking_step>.
    <ls_thinking_step>-thinkingstepnumber   = get_last_thinkingstepnumber( ls_decision_log-thinkingsteps ).
    <ls_thinking_step>-thinkingstepdatetime = get_timestamp( ).
    <ls_thinking_step>-thinkingstepcontent  = `Preparation of first input is finished`.

    IF eo_first_tool_input IS NOT BOUND.
      eo_first_tool_input ?= zpru_cl_agent_service_mngr=>get_service(
                                 iv_service = `ZPRU_IF_PAYLOAD`
                                 iv_context = zpru_if_agent_frw=>cs_context-standard ).
    ENDIF.
    eo_first_tool_input->set_data( ir_data = lr_first_input ).

    ls_decision_log-resultcomment = set_result_comment( ).

    IF eo_decision_log IS NOT BOUND.
      eo_decision_log ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_PAYLOAD`
                                                                  iv_context = zpru_if_agent_frw=>cs_context-standard ).
    ENDIF.
    eo_decision_log->set_data( ir_data = NEW zpru_s_decision_log( ls_decision_log ) ).
  ENDMETHOD.

  METHOD zpru_if_decision_provider~prepare_final_response.
    precheck_final_response( ).
    set_final_response_content( ).
    set_final_response_metadata( ).
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
