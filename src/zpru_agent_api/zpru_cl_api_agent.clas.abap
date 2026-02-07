CLASS zpru_cl_api_agent DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zpru_if_agent_frw.
    INTERFACES zpru_if_api_agent.

  PROTECTED SECTION.
    DATA mo_controller           TYPE REF TO zpru_if_agent_controller.
    DATA mo_short_memory         TYPE REF TO zpru_if_short_memory_provider.
    DATA mo_long_memory          TYPE REF TO zpru_if_long_memory_provider.
    DATA mv_input_query          TYPE zpru_if_agent_frw=>ts_json.
    DATA mv_output_response      TYPE zpru_if_agent_frw=>ts_json.
    DATA mv_output_response_prev TYPE zpru_if_agent_frw=>ts_json.

    METHODS get_controller
      RETURNING VALUE(ro_controller) TYPE REF TO zpru_if_agent_controller
      RAISING   zpru_cx_agent_core.

    METHODS get_short_memory
      IMPORTING iv_agent_uuid   TYPE sysuuid_x16
      EXPORTING eo_short_memory TYPE REF TO zpru_if_short_memory_provider
      CHANGING  cs_reported     TYPE zpru_if_agent_frw=>ts_adf_reported
                cs_failed       TYPE zpru_if_agent_frw=>ts_adf_failed
      RAISING   zpru_cx_agent_core.

    METHODS get_long_memory
      IMPORTING iv_agent_uuid  TYPE sysuuid_x16
      EXPORTING eo_long_memory TYPE REF TO zpru_if_long_memory_provider
      CHANGING  cs_reported    TYPE zpru_if_agent_frw=>ts_adf_reported
                cs_failed      TYPE zpru_if_agent_frw=>ts_adf_failed
      RAISING   zpru_cx_agent_core.

    METHODS process_execution_steps
      IMPORTING is_agent            TYPE zpru_if_adf_type_and_constant=>ts_agent
                is_execution_header TYPE zpru_axc_head
                is_execution_query  TYPE zpru_if_axc_type_and_constant=>ts_axc_query
                it_execution_steps  TYPE zpru_if_axc_type_and_constant=>tt_axc_step
                it_agent_tools      TYPE zpru_if_adf_type_and_constant=>tt_agent_tool
      EXPORTING eo_final_response   TYPE REF TO zpru_if_payload
      CHANGING  cs_axc_reported     TYPE zpru_if_agent_frw=>ts_axc_reported
                cs_axc_failed       TYPE zpru_if_agent_frw=>ts_axc_failed
                cs_adf_reported     TYPE zpru_if_agent_frw=>ts_adf_reported
                cs_adf_failed       TYPE zpru_if_agent_frw=>ts_adf_failed
      RAISING   zpru_cx_agent_core.

    METHODS prepare_execution
      IMPORTING iv_run_uuid         TYPE sysuuid_x16
                iv_query_uuid       TYPE sysuuid_x16 OPTIONAL
      EXPORTING es_agent            TYPE zpru_if_adf_type_and_constant=>ts_agent
                es_execution_header TYPE zpru_axc_head
                es_execution_query  TYPE zpru_if_axc_type_and_constant=>ts_axc_query
                et_execution_steps  TYPE zpru_if_axc_type_and_constant=>tt_axc_step
                et_agent_tools      TYPE zpru_if_adf_type_and_constant=>tt_agent_tool
      CHANGING  cs_axc_reported     TYPE zpru_if_agent_frw=>ts_axc_reported
                cs_axc_failed       TYPE zpru_if_agent_frw=>ts_axc_failed
                cs_adf_reported     TYPE zpru_if_agent_frw=>ts_adf_reported
                cs_adf_failed       TYPE zpru_if_agent_frw=>ts_adf_failed
      RAISING   zpru_cx_agent_core.

    METHODS execute_mini_loop
      IMPORTING is_agent            TYPE zpru_if_adf_type_and_constant=>ts_agent
                is_execution_header TYPE zpru_axc_head
                is_execution_query  TYPE zpru_if_axc_type_and_constant=>ts_axc_query
                it_additional_steps TYPE zpru_if_axc_type_and_constant=>tt_axc_step
                it_additional_tools TYPE zpru_if_adf_type_and_constant=>tt_agent_tool
                is_tool_master_data TYPE zpru_if_adf_type_and_constant=>ts_agent_tool
                is_current_step     TYPE zpru_if_axc_type_and_constant=>ts_axc_step
                iv_output_prompt    TYPE string
      EXPORTING eo_final_response   TYPE REF TO zpru_if_payload
      CHANGING  cs_axc_reported     TYPE zpru_if_agent_frw=>ts_axc_reported
                cs_axc_failed       TYPE zpru_if_agent_frw=>ts_axc_failed
                cs_adf_reported     TYPE zpru_if_agent_frw=>ts_adf_reported
                cs_adf_failed       TYPE zpru_if_agent_frw=>ts_adf_failed
      RAISING   zpru_cx_agent_core.

    METHODS execute_tool_logic
      IMPORTING io_controller       TYPE REF TO zpru_if_agent_controller
                io_input            TYPE REF TO zpru_if_payload
                is_tool_master_data TYPE zpru_if_adf_type_and_constant=>ts_agent_tool
                is_execution_step   TYPE zpru_if_axc_type_and_constant=>ts_axc_step
      EXPORTING eo_response         TYPE REF TO zpru_if_payload
                ev_error_flag       TYPE abap_boolean
                et_additional_steps TYPE zpru_if_axc_type_and_constant=>tt_axc_step
                et_additional_tools TYPE zpru_if_adf_type_and_constant=>tt_agent_tool
      RAISING   zpru_cx_agent_core.

    METHODS finalize_successful_query
      IMPORTING is_agent            TYPE zpru_if_adf_type_and_constant=>ts_agent
                is_execution_header TYPE zpru_axc_head
                is_execution_query  TYPE zpru_if_axc_type_and_constant=>ts_axc_query
                io_last_output      TYPE REF TO zpru_if_payload
                io_short_memory     TYPE REF TO zpru_if_short_memory_provider
                io_controller       TYPE REF TO zpru_if_agent_controller
      EXPORTING eo_final_response   TYPE REF TO zpru_if_payload
      CHANGING  cs_axc_reported     TYPE zpru_if_agent_frw=>ts_axc_reported
                cs_axc_failed       TYPE zpru_if_agent_frw=>ts_axc_failed
                cs_adf_reported     TYPE zpru_if_agent_frw=>ts_adf_reported
                cs_adf_failed       TYPE zpru_if_agent_frw=>ts_adf_failed
      RAISING   zpru_cx_agent_core.

    METHODS log_step_execution
      IMPORTING is_agent            TYPE zpru_if_adf_type_and_constant=>ts_agent
                is_execution_header TYPE zpru_axc_head
                is_execution_query  TYPE zpru_if_axc_type_and_constant=>ts_axc_query
                is_execution_step   TYPE zpru_if_axc_type_and_constant=>ts_axc_step
                is_tool_master_data TYPE zpru_if_adf_type_and_constant=>ts_agent_tool
                iv_input_prompt     TYPE string
                iv_output_prompt    TYPE string
                iv_error_flag       TYPE abap_boolean
                iv_count            TYPE i
      RETURNING VALUE(rs_message)   TYPE zpru_if_short_memory_provider=>ts_message.

    METHODS setup_agent_context
      IMPORTING iv_agent_uuid             TYPE sysuuid_x16
      EXPORTING es_agent                  TYPE zpru_if_adf_type_and_constant=>ts_agent
                et_agent_tools            TYPE zpru_if_adf_type_and_constant=>tt_agent_tool
                eo_decision_provider      TYPE REF TO zpru_if_decision_provider
                eo_short_memory           TYPE REF TO zpru_if_short_memory_provider
                eo_long_memory            TYPE REF TO zpru_if_long_memory_provider
                eo_agent_info_provider    TYPE REF TO zpru_if_agent_info_provider
                eo_system_prompt_provider TYPE REF TO zpru_if_prompt_provider
      CHANGING  cs_adf_reported           TYPE zpru_if_agent_frw=>ts_adf_reported
                cs_adf_failed             TYPE zpru_if_agent_frw=>ts_adf_failed
      RAISING   zpru_cx_agent_core.

    METHODS process_decision_engine
      IMPORTING is_agent                  TYPE zpru_if_adf_type_and_constant=>ts_agent
                it_agent_tools            TYPE zpru_if_adf_type_and_constant=>tt_agent_tool
                io_controller             TYPE REF TO zpru_if_agent_controller
                iv_input_query            TYPE string
                Io_decision_provider      TYPE REF TO zpru_if_decision_provider
                io_system_prompt_provider TYPE REF TO zpru_if_prompt_provider
                io_short_memory           TYPE REF TO zpru_if_short_memory_provider
                io_long_memory            TYPE REF TO zpru_if_long_memory_provider
                io_agent_info_provider    TYPE REF TO zpru_if_agent_info_provider
                iv_stage                  TYPE string
      EXPORTING et_execution_plan         TYPE zpru_if_decision_provider=>tt_execution_plan
                ev_first_tool_input       TYPE zpru_if_agent_frw=>ts_json
                ev_langu                  TYPE sylangu
                ev_decision_log           TYPE zpru_if_agent_frw=>ts_json
      RAISING   zpru_cx_agent_core.

    METHODS construct_execution_steps
      IMPORTING it_agent_tools      TYPE zpru_if_adf_type_and_constant=>tt_agent_tool
                is_agent            TYPE zpru_if_adf_type_and_constant=>ts_agent
                is_execution_header TYPE zpru_axc_head
                is_execution_query  TYPE zpru_axc_query
                iv_first_tool_input TYPE zpru_if_agent_frw=>ts_json
                io_short_memory     TYPE REF TO zpru_if_short_memory_provider
                iv_stage            TYPE string
                iv_count            TYPE i
      EXPORTING et_execution_steps  TYPE zpru_if_axc_type_and_constant=>tt_axc_step
      CHANGING  ct_execution_plan   TYPE zpru_if_decision_provider=>tt_execution_plan
                cs_axc_reported     TYPE zpru_if_agent_frw=>ts_axc_reported
                cs_axc_failed       TYPE zpru_if_agent_frw=>ts_axc_failed
                cs_axc_mapped       TYPE zpru_if_agent_frw=>ts_axc_mapped OPTIONAL     " qqq
      RAISING   zpru_cx_agent_core.

    METHODS resequence_steps
      IMPORTING it_additional_steps TYPE zpru_if_axc_type_and_constant=>tt_axc_step
                is_execution_header TYPE zpru_axc_head
                is_execution_query  TYPE zpru_if_axc_type_and_constant=>ts_axc_query
                iv_output_prompt    TYPE string
      CHANGING  ct_step_before      TYPE zpru_if_axc_type_and_constant=>tt_axc_step
                ct_step_after       TYPE zpru_if_axc_type_and_constant=>tt_axc_step
                ct_step_update_imp  TYPE zpru_if_axc_type_and_constant=>tt_step_update_imp.

    METHODS clear_internal_state.

    METHODS assign_controller_context
      IMPORTING io_parent_controller TYPE REF TO zpru_if_agent_controller OPTIONAL.

    METHODS fetch_agent_configuration
      IMPORTING iv_agent_name TYPE zpru_if_api_agent=>tv_agent_name
      EXPORTING es_agent      TYPE zpru_if_adf_type_and_constant=>ts_agent
                eo_service    TYPE REF TO zpru_if_adf_service
                et_agent_k    TYPE zpru_if_adf_type_and_constant=>tt_agent_k
      CHANGING  cs_reported   TYPE zpru_if_agent_frw=>ts_adf_reported
                cs_failed     TYPE zpru_if_agent_frw=>ts_adf_failed
      RAISING   zpru_cx_agent_core.

    METHODS ensure_agent_is_active
      IMPORTING is_agent    TYPE zpru_if_adf_type_and_constant=>ts_agent
                io_service  TYPE REF TO zpru_if_adf_service
      CHANGING  cs_reported TYPE zpru_if_agent_frw=>ts_adf_reported
                cs_failed   TYPE zpru_if_agent_frw=>ts_adf_failed
      RAISING   zpru_cx_agent_core.

    METHODS identify_available_tools
      IMPORTING it_agent_k  TYPE zpru_if_adf_type_and_constant=>tt_agent_k
                io_service  TYPE REF TO zpru_if_adf_service
      EXPORTING et_tools    TYPE zpru_if_adf_type_and_constant=>tt_agent_tool
      CHANGING  cs_reported TYPE zpru_if_agent_frw=>ts_adf_reported
                cs_failed   TYPE zpru_if_agent_frw=>ts_adf_failed
      RAISING   zpru_cx_agent_core.

    METHODS prepare_memory_provider
      IMPORTING iv_agent_uuid   TYPE sysuuid_x16
      EXPORTING eo_short_memory TYPE REF TO zpru_if_short_memory_provider
      CHANGING  cs_reported     TYPE zpru_if_agent_frw=>ts_adf_reported
                cs_failed       TYPE zpru_if_agent_frw=>ts_adf_failed
      RAISING   zpru_cx_agent_core.

    METHODS record_initialization_event
      IMPORTING is_agent        TYPE zpru_if_adf_type_and_constant=>ts_agent
                it_tools        TYPE zpru_if_adf_type_and_constant=>tt_agent_tool
                io_short_memory TYPE REF TO zpru_if_short_memory_provider.

    METHODS fetch_agent_definition_by_uuid
      IMPORTING iv_agent_uuid TYPE sysuuid_x16
      EXPORTING es_agent      TYPE zpru_if_adf_type_and_constant=>ts_agent
                eo_service    TYPE REF TO zpru_if_adf_service
      CHANGING  cs_reported   TYPE zpru_if_agent_frw=>ts_adf_reported
                cs_failed     TYPE zpru_if_agent_frw=>ts_adf_failed
      RAISING   zpru_cx_agent_core.

    METHODS update_query_internal_state
      IMPORTING iv_input_query TYPE string.

    METHODS append_query_to_controller
      IMPORTING iv_input_query TYPE string.

    METHODS record_query_event
      IMPORTING is_agent        TYPE zpru_if_adf_type_and_constant=>ts_agent
                io_short_memory TYPE REF TO zpru_if_short_memory_provider.

  PRIVATE SECTION.

ENDCLASS.


CLASS zpru_cl_api_agent IMPLEMENTATION.
  METHOD zpru_if_api_agent~save_execution.
    DATA lo_axc_service TYPE REF TO zpru_if_axc_service.

    TRY.
        lo_axc_service ?= zpru_cl_agent_service_mngr=>get_service(
                              iv_service = `ZPRU_IF_AXC_SERVICE`
                              iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDTRY.

    lo_axc_service->do_save( EXPORTING iv_do_commit = abap_false
                             CHANGING  cs_reported  = cs_axc_reported
                                       cs_failed    = cs_axc_failed
                                       cs_mapped    = cs_axc_mapped ).

    IF    cs_axc_failed-header IS NOT INITIAL
       OR cs_axc_failed-query  IS NOT INITIAL
       OR cs_axc_failed-step   IS NOT INITIAL.
      RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDIF.

    IF iv_do_commit = abap_false.
      RETURN.
    ENDIF.

    lo_axc_service->do_save( EXPORTING iv_do_commit = abap_true
                             CHANGING  cs_reported  = cs_axc_reported
                                       cs_failed    = cs_axc_failed
                                       cs_mapped    = cs_axc_mapped ).
  ENDMETHOD.

  METHOD zpru_if_api_agent~build_execution.
    CLEAR ev_built_run_uuid.
    CLEAR ev_built_query_uuid.

    IF    iv_agent_uuid  IS INITIAL
       OR mv_input_query IS INITIAL.
      RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDIF.

    setup_agent_context( EXPORTING iv_agent_uuid             = iv_agent_uuid
                         IMPORTING es_agent                  = DATA(ls_agent)
                                   et_agent_tools            = DATA(lt_agent_tools)
                                   eo_decision_provider      = DATA(lo_decision_provider)
                                   eo_short_memory           = DATA(lo_short_memory)
                                   eo_long_memory            = DATA(lo_long_memory)
                                   eo_agent_info_provider    = DATA(lo_agent_info_provider)
                                   eo_system_prompt_provider = DATA(lo_system_prompt_provider)
                         CHANGING  cs_adf_reported           = cs_adf_reported
                                   cs_adf_failed             = cs_adf_failed ).

    DATA(lo_controller) = get_controller( ).
    lo_controller->mv_agent_uuid = ls_agent-agent_uuid.

    process_decision_engine( EXPORTING is_agent                  = ls_agent
                                       it_agent_tools            = lt_agent_tools
                                       io_controller             = lo_controller
                                       iv_input_query            = mv_input_query
                                       io_decision_provider      = lo_decision_provider
                                       io_system_prompt_provider = lo_system_prompt_provider
                                       io_short_memory           = lo_short_memory
                                       io_long_memory            = lo_long_memory
                                       io_agent_info_provider    = lo_agent_info_provider
                                       iv_stage                  = 'BUILD_EXECUTION'
                             IMPORTING et_execution_plan         = DATA(lt_execution_plan)
                                       ev_first_tool_input       = DATA(lv_first_tool_input)
                                       ev_langu                  = DATA(lv_langu)
                                       ev_decision_log           = DATA(lv_decision_log) ).

    TRY.
        DATA(lo_axc_service) = CAST zpru_if_axc_service( zpru_cl_agent_service_mngr=>get_service(
                                                             iv_service = `ZPRU_IF_AXC_SERVICE`
                                                             iv_context = zpru_if_agent_frw=>cs_context-standard ) ).
      CATCH zpru_cx_agent_core.
        RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDTRY.

    GET TIME STAMP FIELD DATA(lv_now).
    TRY.
        DATA(ls_execution_header) = VALUE zpru_if_axc_type_and_constant=>ts_head_create_imp(
                                              run_uuid           = cl_system_uuid=>create_uuid_x16_static( )
                                              run_id             = lo_axc_service->generate_run_id( )
                                              agent_uuid         = ls_agent-agent_uuid
                                              user_id            = sy-uname
                                              start_timestamp    = lv_now
                                              created_by         = sy-uname
                                              created_at         = lv_now
                                              changed_by         = sy-uname
                                              last_changed       = lv_now
                                              local_last_changed = lv_now
                                              control            = VALUE #( run_uuid           = abap_true
                                                                            run_id             = abap_true
                                                                            agent_uuid         = abap_true
                                                                            user_id            = abap_true
                                                                            start_timestamp    = abap_true
                                                                            end_timestamp      = abap_true
                                                                            created_by         = abap_true
                                                                            created_at         = abap_true
                                                                            changed_by         = abap_true
                                                                            last_changed       = abap_true
                                                                            local_last_changed = abap_true ) ).
      CATCH cx_uuid_error.
        RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDTRY.

    lo_axc_service->create_header( EXPORTING it_head_create_imp = VALUE #( ( ls_execution_header ) )
                                   CHANGING  cs_reported        = cs_axc_reported
                                             cs_failed          = cs_axc_failed
                                             cs_mapped          = cs_axc_mapped ).

    TRY.
        DATA(lo_utility) = CAST zpru_if_agent_util( zpru_cl_agent_service_mngr=>get_service(
                                                        iv_service = `ZPRU_IF_AGENT_UTIL`
                                                        iv_context = zpru_if_agent_frw=>cs_context-standard ) ).
      CATCH zpru_cx_agent_core.
        RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDTRY.

    DATA(lv_decision_log_message) = |\{ "USER": "{ sy-uname }", "TOPIC" : "DECISION_LOG", "TIMESTAMP" : "{ lv_now }",| &&
                                    | "CONTENT" : "{ lv_decision_log }" \}|.

    TRY.
        DATA(ls_execution_query) = VALUE zpru_if_axc_type_and_constant=>ts_query_create_imp(
            query_uuid       = cl_system_uuid=>create_uuid_x16_static( )
            query_number     = lo_axc_service->generate_query_number( iv_run_uuid = ls_execution_header-run_uuid )
            run_uuid         = ls_execution_header-run_uuid
            language         = COND #( WHEN lv_langu IS NOT INITIAL THEN lv_langu ELSE sy-langu )
            execution_status = zpru_if_axc_type_and_constant=>sc_query_status-new
            start_timestamp  = lv_now
            input_prompt     = lo_utility->search_node_in_json( iv_json           = mv_input_query
                                                                iv_field_2_search = 'CONTENT' )
            decision_log     = lo_utility->search_node_in_json( iv_json           = lv_decision_log_message
                                                                iv_field_2_search = 'CONTENT' )
            control          = VALUE #( query_uuid       = abap_true
                                        query_number     = abap_true
                                        run_uuid         = abap_true
                                        language         = abap_true
                                        execution_status = abap_true
                                        start_timestamp  = abap_true
                                        end_timestamp    = abap_true
                                        input_prompt     = abap_true
                                        decision_log     = abap_true
                                        output_response  = abap_true ) ).
      CATCH cx_uuid_error.
        RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDTRY.

    lo_axc_service->cba_query( EXPORTING it_axc_query_imp = VALUE #( ( ls_execution_query ) )
                               CHANGING  cs_reported      = cs_axc_reported
                                         cs_failed        = cs_axc_failed
                                         cs_mapped        = cs_axc_mapped ).

    DATA(lt_message_in) = VALUE zpru_if_short_memory_provider=>tt_message(
        ( message_cid  = |{ lv_now }-{ sy-uname }-BUILD_EXECUTION_3|
          stage        = 'BUILD_EXECUTION'
          sub_stage    = 'AFTER_QUERY_CREATION'
          namespace    = |{ sy-uname }.{ ls_agent-agent_name }.{ ls_execution_header-run_id }|
          user_name    = sy-uname
          agent_uuid   = ls_agent-agent_uuid
          run_uuid     = ls_execution_header-run_uuid
          query_uuid   = ls_execution_query-query_uuid
          message_time = lv_now
          content      = |\{ "AGENT_NAME" : "{ ls_agent-agent_name }", | &&
                         | "RUN_ID" : "{ ls_execution_header-run_id }", | &&
                         | "QUERY_NUMBER" : "{ ls_execution_query-query_number }", | &&
                         | "LANGUAGE" : "{ ls_execution_query-language }", | &&
                         | "QUERY" : { mv_input_query }, | &&
                         | "DECISION LOG" : { lv_decision_log_message } \}|
          message_type = zpru_if_short_memory_provider=>cs_msg_type-query ) ).
    lo_short_memory->save_message( lt_message_in ).

    construct_execution_steps( EXPORTING it_agent_tools      = lt_agent_tools
                                         is_agent            = ls_agent
                                         is_execution_header = CORRESPONDING #( ls_execution_header )
                                         is_execution_query  = CORRESPONDING #( ls_execution_query )
                                         iv_first_tool_input = lv_first_tool_input
                                         io_short_memory     = lo_short_memory
                                         iv_stage            = 'BUILD_EXECUTION'
                                         iv_count            = 4
                               CHANGING  ct_execution_plan   = lt_execution_plan
                                         cs_axc_reported     = cs_axc_reported
                                         cs_axc_failed       = cs_axc_failed ).

    ev_built_run_uuid = ls_execution_header-run_uuid.
    ev_built_query_uuid = ls_execution_query-query_uuid.
  ENDMETHOD.

  METHOD zpru_if_api_agent~initialize.
    CLEAR: es_agent,
           et_tools.

    IF iv_agent_name IS INITIAL.
      RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDIF.

    clear_internal_state( ).

    assign_controller_context( io_parent_controller ).

    fetch_agent_configuration( EXPORTING iv_agent_name = iv_agent_name
                               IMPORTING es_agent      = es_agent
                                         eo_service    = DATA(lo_adf_service)
                                         et_agent_k    = DATA(lt_agent_k)
                               CHANGING  cs_reported   = cs_adf_reported
                                         cs_failed     = cs_adf_failed ).

    ensure_agent_is_active( EXPORTING is_agent    = es_agent
                                      io_service  = lo_adf_service
                            CHANGING  cs_reported = cs_adf_reported
                                      cs_failed   = cs_adf_failed ).

    identify_available_tools( EXPORTING it_agent_k  = lt_agent_k
                                        io_service  = lo_adf_service
                              IMPORTING et_tools    = et_tools
                              CHANGING  cs_reported = cs_adf_reported
                                        cs_failed   = cs_adf_failed ).

    prepare_memory_provider( EXPORTING iv_agent_uuid   = es_agent-agent_uuid
                             IMPORTING eo_short_memory = DATA(lo_short_memory)
                             CHANGING  cs_reported     = cs_adf_reported
                                       cs_failed       = cs_adf_failed ).

    record_initialization_event( is_agent        = es_agent
                                 it_tools        = et_tools
                                 io_short_memory = lo_short_memory ).
  ENDMETHOD.

  METHOD zpru_if_api_agent~rerun.
    prepare_execution( EXPORTING iv_run_uuid         = iv_run_uuid
                                 iv_query_uuid       = iv_query_uuid
                       IMPORTING es_agent            = DATA(ls_agent)
                                 es_execution_header = DATA(ls_execution_header)
                                 es_execution_query  = DATA(ls_execution_query)
                                 et_execution_steps  = DATA(lt_execution_steps)
                                 et_agent_tools      = DATA(lt_agent_tools)
                       CHANGING  cs_axc_reported     = cs_axc_reported
                                 cs_axc_failed       = cs_axc_failed
                                 cs_adf_reported     = cs_adf_reported
                                 cs_adf_failed       = cs_adf_failed ).

    IF    lt_execution_steps IS INITIAL
       OR lt_agent_tools     IS INITIAL
       OR ls_execution_query IS INITIAL.
      RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDIF.

    IF    ls_execution_query-execution_status = zpru_if_axc_type_and_constant=>sc_query_status-new
       OR ls_execution_query-execution_status = zpru_if_axc_type_and_constant=>sc_query_status-complete.
      RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDIF.

    process_execution_steps( EXPORTING is_agent            = ls_agent
                                       is_execution_header = ls_execution_header
                                       is_execution_query  = ls_execution_query
                                       it_execution_steps  = lt_execution_steps
                                       it_agent_tools      = lt_agent_tools
                             IMPORTING eo_final_response   = eo_final_response
                             CHANGING  cs_axc_reported     = cs_axc_reported
                                       cs_axc_failed       = cs_axc_failed
                                       cs_adf_reported     = cs_adf_reported
                                       cs_adf_failed       = cs_adf_failed ).
  ENDMETHOD.

  METHOD zpru_if_api_agent~rerun_from_step.
    DATA lt_valid_steps TYPE zpru_if_axc_type_and_constant=>tt_axc_step.

    IF iv_starting_step_uuid IS INITIAL.
      RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDIF.

    prepare_execution( EXPORTING iv_run_uuid         = iv_run_uuid
                                 iv_query_uuid       = iv_query_uuid
                       IMPORTING es_agent            = DATA(ls_agent)
                                 es_execution_header = DATA(ls_execution_header)
                                 es_execution_query  = DATA(ls_execution_query)
                                 et_execution_steps  = DATA(lt_execution_steps)
                                 et_agent_tools      = DATA(lt_agent_tools)
                       CHANGING  cs_axc_reported     = cs_axc_reported
                                 cs_axc_failed       = cs_axc_failed
                                 cs_adf_reported     = cs_adf_reported
                                 cs_adf_failed       = cs_adf_failed ).

    IF    lt_execution_steps IS INITIAL
       OR lt_agent_tools     IS INITIAL
       OR ls_execution_query IS INITIAL.
      RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDIF.

    IF ls_execution_query-execution_status = zpru_if_axc_type_and_constant=>sc_query_status-complete.
      RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDIF.

    ASSIGN lt_execution_steps[ step_uuid = iv_starting_step_uuid ] TO FIELD-SYMBOL(<ls_step_2_start>).
    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDIF.

    LOOP AT lt_execution_steps TRANSPORTING NO FIELDS
         WHERE execution_seq < <ls_step_2_start>-execution_seq.
      EXIT.
    ENDLOOP.
    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDIF.

    IF <ls_step_2_start>-step_status = zpru_if_axc_type_and_constant=>sc_step_status-complete.
      RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDIF.

    LOOP AT lt_execution_steps ASSIGNING FIELD-SYMBOL(<ls_step_2_exec>)
         WHERE execution_seq >= <ls_step_2_start>-execution_seq.
      APPEND INITIAL LINE TO lt_valid_steps ASSIGNING FIELD-SYMBOL(<ls_valid_step>).
      <ls_valid_step> = <ls_step_2_exec>.

      CLEAR <ls_valid_step>-input_prompt.
      CLEAR <ls_valid_step>-output_prompt.

      IF     iv_new_step_prompt        IS NOT INITIAL
         AND <ls_valid_step>-step_uuid  = iv_starting_step_uuid.
        <ls_valid_step>-input_prompt = iv_new_step_prompt.
      ENDIF.

    ENDLOOP.

    IF lt_valid_steps IS INITIAL.
      RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDIF.

    process_execution_steps( EXPORTING is_agent            = ls_agent
                                       is_execution_header = ls_execution_header
                                       is_execution_query  = ls_execution_query
                                       it_execution_steps  = lt_execution_steps
                                       it_agent_tools      = lt_agent_tools
                             IMPORTING eo_final_response   = eo_final_response
                             CHANGING  cs_axc_reported     = cs_axc_reported
                                       cs_axc_failed       = cs_axc_failed
                                       cs_adf_reported     = cs_adf_reported
                                       cs_adf_failed       = cs_adf_failed ).
  ENDMETHOD.

  METHOD zpru_if_api_agent~run.
    prepare_execution( EXPORTING iv_run_uuid         = iv_run_uuid
                                 iv_query_uuid       = iv_query_uuid
                       IMPORTING es_agent            = DATA(ls_agent)
                                 es_execution_header = DATA(ls_execution_header)
                                 es_execution_query  = DATA(ls_execution_query)
                                 et_execution_steps  = DATA(lt_execution_steps)
                                 et_agent_tools      = DATA(lt_agent_tools)
                       CHANGING  cs_axc_reported     = cs_axc_reported
                                 cs_axc_failed       = cs_axc_failed
                                 cs_adf_reported     = cs_adf_reported
                                 cs_adf_failed       = cs_adf_failed ).

    IF    lt_execution_steps IS INITIAL
       OR lt_agent_tools     IS INITIAL
       OR ls_execution_query IS INITIAL.
      RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDIF.

    IF    ls_execution_query-execution_status = zpru_if_axc_type_and_constant=>sc_query_status-complete
       OR ls_execution_query-execution_status = zpru_if_axc_type_and_constant=>sc_query_status-error.
      RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDIF.

    process_execution_steps( EXPORTING is_agent            = ls_agent
                                       is_execution_header = ls_execution_header
                                       is_execution_query  = ls_execution_query
                                       it_execution_steps  = lt_execution_steps
                                       it_agent_tools      = lt_agent_tools
                             IMPORTING eo_final_response   = eo_final_response
                             CHANGING  cs_axc_reported     = cs_axc_reported
                                       cs_axc_failed       = cs_axc_failed
                                       cs_adf_reported     = cs_adf_reported
                                       cs_adf_failed       = cs_adf_failed ).
  ENDMETHOD.

  METHOD zpru_if_api_agent~set_input_query.
    IF    iv_input_query IS INITIAL
       OR iv_agent_uuid  IS INITIAL.
      RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDIF.

    fetch_agent_definition_by_uuid( EXPORTING iv_agent_uuid = iv_agent_uuid
                                    IMPORTING es_agent      = DATA(ls_agent)
                                              eo_service    = DATA(lo_adf_service)
                                    CHANGING  cs_reported   = cs_adf_reported
                                              cs_failed     = cs_adf_failed ).

    ensure_agent_is_active( EXPORTING is_agent    = ls_agent
                                      io_service  = lo_adf_service
                            CHANGING  cs_reported = cs_adf_reported
                                      cs_failed   = cs_adf_failed ).

    update_query_internal_state( iv_input_query ).

    append_query_to_controller( iv_input_query ).

    prepare_memory_provider( EXPORTING iv_agent_uuid   = iv_agent_uuid
                             IMPORTING eo_short_memory = DATA(lo_short_memory)
                             CHANGING  cs_reported     = cs_adf_reported
                                       cs_failed       = cs_adf_failed ).

    record_query_event( is_agent        = ls_agent
                        io_short_memory = lo_short_memory ).
  ENDMETHOD.

  METHOD get_short_memory.
    DATA lo_discard_strategy TYPE REF TO zpru_if_discard_strategy.
    DATA lo_summary_strategy TYPE REF TO zpru_if_summarization.

    IF mo_short_memory IS BOUND.
      eo_short_memory = mo_short_memory.
      RETURN.
    ENDIF.

    IF iv_agent_uuid IS INITIAL.
      RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDIF.

    fetch_agent_definition_by_uuid( EXPORTING iv_agent_uuid = iv_agent_uuid
                                    IMPORTING es_agent      = DATA(ls_agent)
                                    " TODO: variable is assigned but never used (ABAP cleaner)
                                              eo_service    = DATA(lo_adf_service)
                                    CHANGING  cs_reported   = cs_reported
                                              cs_failed     = cs_failed ).

    CREATE OBJECT mo_short_memory TYPE (ls_agent-short_memory_provider).
    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDIF.

    DATA(lo_controller) = get_controller( ).
    lo_controller->mo_short_memory = mo_short_memory.

    CREATE OBJECT mo_long_memory TYPE (ls_agent-long_memory_provider).
    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDIF.

    lo_controller->mo_long_memory = mo_long_memory.

    mo_short_memory->set_long_memory( io_long_memory = mo_long_memory ).

    eo_short_memory = mo_short_memory.

    SELECT SINGLE type~agent_type,
                  type~discard_strategy,
                  type~summary_strategy,
                  disc~strategy_provider AS disc_provider,
                  summ~strategy_provider AS summ_provider,
                  short_mem_volume
      FROM zpru_agent_type AS type
             LEFT OUTER JOIN
               zpru_disc_strat AS disc ON disc~discard_strategy = type~discard_strategy
                 LEFT OUTER JOIN
                   zpru_summ_strat AS summ ON summ~summary_strategy = type~summary_strategy
      WHERE type~agent_type = @ls_agent-agent_type
      INTO @DATA(ls_agent_config).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    mo_short_memory->set_mem_volume( iv_mem_volume = ls_agent_config-short_mem_volume ).

    CREATE OBJECT lo_discard_strategy TYPE (ls_agent_config-disc_provider).
    IF sy-subrc = 0.
      mo_short_memory->set_discard_strategy( io_discard_strategy = lo_discard_strategy ).
    ELSE.

      TRY.
          lo_discard_strategy ?= zpru_cl_agent_service_mngr=>get_service(
                                     iv_service = `ZPRU_IF_DISCARD_STRATEGY`
                                     iv_context = zpru_if_agent_frw=>cs_context-st_discard_strategy_delete ).
        CATCH zpru_cx_agent_core.
          RAISE EXCEPTION NEW zpru_cx_agent_core( ).
      ENDTRY.

      mo_short_memory->set_discard_strategy( io_discard_strategy = lo_discard_strategy ).
    ENDIF.

    CREATE OBJECT lo_summary_strategy TYPE (ls_agent_config-summ_provider).
    IF sy-subrc = 0.
      mo_long_memory->set_summarization( io_summarization = lo_summary_strategy ).
    ELSE.

      TRY.
          lo_summary_strategy ?= zpru_cl_agent_service_mngr=>get_service(
                                     iv_service = `ZPRU_IF_SUMMARIZATION`
                                     iv_context = zpru_if_agent_frw=>cs_context-st_summarize ).
        CATCH zpru_cx_agent_core.
          RAISE EXCEPTION NEW zpru_cx_agent_core( ).
      ENDTRY.

      mo_long_memory->set_summarization( io_summarization = lo_summary_strategy ).
    ENDIF.
  ENDMETHOD.

  METHOD process_execution_steps.
    DATA lo_input            TYPE REF TO zpru_if_payload.
    DATA lo_output           TYPE REF TO zpru_if_payload.
    DATA lo_last_output      TYPE REF TO zpru_if_payload.
    DATA lo_axc_service      TYPE REF TO zpru_if_axc_service.
    DATA lt_query_update_imp TYPE zpru_if_axc_type_and_constant=>tt_query_update_imp.
    DATA lt_step_update_imp  TYPE zpru_if_axc_type_and_constant=>tt_step_update_imp.
    DATA lv_error_flag       TYPE abap_boolean.
    DATA lt_message          TYPE zpru_if_short_memory_provider=>tt_message.
    DATA lv_input_prompt     TYPE string.
    DATA lv_output_prompt    TYPE string.
    DATA lo_short_memory     TYPE REF TO zpru_if_short_memory_provider.

    TRY.
        lo_axc_service ?= zpru_cl_agent_service_mngr=>get_service(
                              iv_service = `ZPRU_IF_AXC_SERVICE`
                              iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDTRY.

    prepare_memory_provider( EXPORTING iv_agent_uuid   = is_agent-agent_uuid
                             IMPORTING eo_short_memory = lo_short_memory
                             CHANGING  cs_reported     = cs_adf_reported
                                       cs_failed       = cs_adf_failed ).

    DATA(lo_controller) = get_controller( ).
    CLEAR lo_controller->mt_run_context.
    lo_controller->mt_execution_steps = it_execution_steps.

    ASSIGN lo_controller->mt_input_output[ number = lines( lo_controller->mt_input_output ) ] TO FIELD-SYMBOL(<ls_input_output>).
    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDIF.

    <ls_input_output>-execution_steps = it_execution_steps.

    DATA(lv_count) = 1.
    LOOP AT it_execution_steps ASSIGNING FIELD-SYMBOL(<ls_execution_step>).

      ASSIGN it_agent_tools[ tool_uuid = <ls_execution_step>-tool_uuid ] TO FIELD-SYMBOL(<ls_tool_master_data>).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      APPEND INITIAL LINE TO lo_controller->mt_run_context ASSIGNING FIELD-SYMBOL(<ls_run_context>).
      <ls_run_context>-tool_master_data = <ls_tool_master_data>.
      <ls_run_context>-execution_step   = <ls_execution_step>.

      IF lv_count = 1.
        TRY.
            lo_input ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_PAYLOAD`
                                                                 iv_context = zpru_if_agent_frw=>cs_context-standard ).
          CATCH zpru_cx_agent_core.
            RAISE EXCEPTION NEW zpru_cx_agent_core( ).
        ENDTRY.

        lo_input->set_data( ir_data = REF #( <ls_execution_step>-input_prompt ) ).
      ELSE.
        IF lv_output_prompt IS NOT INITIAL.
          lo_input->set_data( ir_data = REF #( lv_output_prompt ) ).
        ELSE.
          lo_input->clear_data( ).
        ENDIF.
      ENDIF.

      lv_error_flag = abap_false.

      execute_tool_logic( EXPORTING io_controller       = lo_controller
                                    io_input            = lo_input
                                    is_tool_master_data = <ls_tool_master_data>
                                    is_execution_step   = <ls_execution_step>
                          IMPORTING eo_response         = lo_output
                                    ev_error_flag       = lv_error_flag
                                    et_additional_steps = DATA(lt_additional_steps)
                                    et_additional_tools = DATA(lt_additional_tools) ).

      IF     lt_additional_steps IS NOT INITIAL
         AND lt_additional_tools IS NOT INITIAL.
        execute_mini_loop(
          EXPORTING
            is_agent            = is_agent
            is_execution_header = is_execution_header
            is_execution_query  = is_execution_query
            it_additional_steps = lt_additional_steps
            it_additional_tools = lt_additional_tools
            is_tool_master_data = <ls_tool_master_data>
            is_current_step     = <ls_execution_step>
            iv_output_prompt    = lo_output->get_data( )->*
          IMPORTING
            eo_final_response   = lo_output
          CHANGING
            cs_axc_reported     = cs_axc_reported
            cs_axc_failed       = cs_axc_failed
            cs_adf_reported     = cs_adf_reported
            cs_adf_failed       = cs_adf_failed ).
      ENDIF.

      lv_input_prompt  = lo_input->get_data( )->*.
      lv_output_prompt = lo_output->get_data( )->*.

      APPEND log_step_execution( is_agent            = is_agent
                                 is_execution_header = is_execution_header
                                 is_execution_query  = is_execution_query
                                 is_execution_step   = <ls_execution_step>
                                 is_tool_master_data = <ls_tool_master_data>
                                 iv_input_prompt     = lv_input_prompt
                                 iv_output_prompt    = lv_output_prompt
                                 iv_error_flag       = lv_error_flag
                                 iv_count            = lv_count ) TO lt_message.

      lv_count += 1.

      GET TIME STAMP FIELD DATA(lv_now).

      IF lv_error_flag = abap_true.
        APPEND INITIAL LINE TO lt_step_update_imp ASSIGNING FIELD-SYMBOL(<ls_step_2_upd>).
        <ls_step_2_upd>-step_uuid     = <ls_execution_step>-step_uuid.
        <ls_step_2_upd>-query_uuid    = <ls_execution_step>-query_uuid.
        <ls_step_2_upd>-run_uuid      = <ls_execution_step>-run_uuid.
        <ls_step_2_upd>-step_status   = zpru_if_axc_type_and_constant=>sc_step_status-error.
        <ls_step_2_upd>-end_timestamp = lv_now.
        <ls_step_2_upd>-input_prompt  = lv_input_prompt.
        <ls_step_2_upd>-output_prompt = lv_output_prompt.
        <ls_step_2_upd>-control-step_status   = abap_true.
        <ls_step_2_upd>-control-end_timestamp = abap_true.
        <ls_step_2_upd>-control-input_prompt  = abap_true.
        <ls_step_2_upd>-control-output_prompt = abap_true.

        <ls_run_context>-execution_step-step_status   = zpru_if_axc_type_and_constant=>sc_step_status-error.
        <ls_run_context>-execution_step-end_timestamp = lv_now.
        <ls_run_context>-execution_step-input_prompt  = lv_input_prompt.
        <ls_run_context>-execution_step-output_prompt = lv_output_prompt.

        APPEND INITIAL LINE TO lt_query_update_imp ASSIGNING FIELD-SYMBOL(<ls_query_2_upd>).
        <ls_query_2_upd>-query_uuid       = is_execution_query-query_uuid.
        <ls_query_2_upd>-run_uuid         = is_execution_query-run_uuid.
        <ls_query_2_upd>-execution_status = zpru_if_axc_type_and_constant=>sc_query_status-error.
        <ls_query_2_upd>-end_timestamp    = lv_now.
        <ls_query_2_upd>-control-execution_status = abap_true.
        <ls_query_2_upd>-control-end_timestamp    = abap_true.
        EXIT.
      ENDIF.

      APPEND INITIAL LINE TO lt_step_update_imp ASSIGNING <ls_step_2_upd>.
      <ls_step_2_upd>-step_uuid     = <ls_execution_step>-step_uuid.
      <ls_step_2_upd>-query_uuid    = <ls_execution_step>-query_uuid.
      <ls_step_2_upd>-run_uuid      = <ls_execution_step>-run_uuid.
      <ls_step_2_upd>-step_status   = zpru_if_axc_type_and_constant=>sc_step_status-complete.
      <ls_step_2_upd>-end_timestamp = lv_now.
      <ls_step_2_upd>-input_prompt  = lv_input_prompt.
      <ls_step_2_upd>-output_prompt = lv_output_prompt.
      <ls_step_2_upd>-control-step_status   = abap_true.
      <ls_step_2_upd>-control-end_timestamp = abap_true.
      <ls_step_2_upd>-control-input_prompt  = abap_true.
      <ls_step_2_upd>-control-output_prompt = abap_true.

      <ls_run_context>-execution_step-step_status   = zpru_if_axc_type_and_constant=>sc_step_status-complete.
      <ls_run_context>-execution_step-end_timestamp = lv_now.
      <ls_run_context>-execution_step-input_prompt  = lv_input_prompt.
      <ls_run_context>-execution_step-output_prompt = lv_output_prompt.

      IF lo_controller->mv_stop_agent = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.

    <ls_input_output>-run_context = lo_controller->mt_run_context.

    DATA(lv_last_output) = lv_output_prompt.

    TRY.
        lo_last_output ?= zpru_cl_agent_service_mngr=>get_service(
                              iv_service = `ZPRU_IF_PAYLOAD`
                              iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDTRY.

    lo_last_output->set_data( ir_data = NEW string( lv_last_output ) ).

    IF lt_message IS NOT INITIAL.
      lo_short_memory->save_message( it_message = lt_message ).
    ENDIF.

    IF lt_step_update_imp IS NOT INITIAL.
      lo_axc_service->update_step( EXPORTING it_step_update_imp = lt_step_update_imp
                                   CHANGING  cs_reported        = cs_axc_reported
                                             cs_failed          = cs_axc_failed ).
    ENDIF.

    IF lv_error_flag = abap_true.
      IF lt_query_update_imp IS NOT INITIAL.
        lo_axc_service->update_query( EXPORTING it_query_update_imp = lt_query_update_imp
                                      CHANGING  cs_reported         = cs_axc_reported
                                                cs_failed           = cs_axc_failed ).
      ENDIF.
      RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ELSE.
      finalize_successful_query( EXPORTING is_agent            = is_agent
                                           is_execution_header = is_execution_header
                                           is_execution_query  = is_execution_query
                                           io_last_output      = lo_last_output
                                           io_short_memory     = lo_short_memory
                                           io_controller       = lo_controller
                                 IMPORTING eo_final_response   = eo_final_response
                                 CHANGING  cs_axc_reported     = cs_axc_reported
                                           cs_axc_failed       = cs_axc_failed
                                           cs_adf_reported     = cs_adf_reported
                                           cs_adf_failed       = cs_adf_failed ).
    ENDIF.
  ENDMETHOD.

  METHOD prepare_execution.
    DATA lv_query_to_run     TYPE sysuuid_x16.
    DATA lo_axc_service      TYPE REF TO zpru_if_axc_service.
    DATA ls_execution_header TYPE zpru_axc_head.
    DATA ls_execution_query  TYPE zpru_axc_query.
    DATA lo_adf_service      TYPE REF TO zpru_if_adf_service.

    CLEAR: et_agent_tools,
           et_execution_steps,
           es_execution_query,
           es_agent,
           es_execution_header.

    IF iv_run_uuid IS INITIAL.
      RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDIF.

    TRY.
        lo_axc_service ?= zpru_cl_agent_service_mngr=>get_service(
                              iv_service = `ZPRU_IF_AXC_SERVICE`
                              iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDTRY.

    lo_axc_service->read_header( EXPORTING it_head_read_k = VALUE #( ( run_uuid = iv_run_uuid
                                                                       control  = VALUE #(
                                                                           run_uuid           = abap_true
                                                                           run_id             = abap_true
                                                                           agent_uuid         = abap_true
                                                                           user_id            = abap_true
                                                                           start_timestamp    = abap_true
                                                                           end_timestamp      = abap_true
                                                                           created_by         = abap_true
                                                                           created_at         = abap_true
                                                                           changed_by         = abap_true
                                                                           last_changed       = abap_true
                                                                           local_last_changed = abap_true ) ) )
                                 IMPORTING et_axc_head    = DATA(lt_axc_head)
                                 CHANGING  cs_reported    = cs_axc_reported
                                           cs_failed      = cs_axc_failed ).

    ASSIGN lt_axc_head[ 1 ] TO FIELD-SYMBOL(<ls_execution_header>).
    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDIF.

    ls_execution_header = <ls_execution_header>.
    es_execution_header = <ls_execution_header>.

    IF iv_query_uuid IS INITIAL.

      lo_axc_service->get_actual_query( EXPORTING it_axc_head_k          = CORRESPONDING #( lt_axc_head )
                                        IMPORTING et_axc_head_query_link = DATA(lt_query_to_read)
                                        CHANGING  cs_reported            = cs_axc_reported
                                                  cs_failed              = cs_axc_failed ).

      lv_query_to_run = VALUE #( lt_query_to_read[ 1 ]-query_uuid OPTIONAL ).
    ELSE.
      lv_query_to_run = iv_query_uuid.
    ENDIF.

    lo_axc_service->read_query( EXPORTING it_query_read_k = VALUE #( ( query_uuid = lv_query_to_run
                                                                       control    = VALUE #(
                                                                           run_uuid         = abap_true
                                                                           query_number     = abap_true
                                                                           query_uuid       = abap_true
                                                                           language         = abap_true
                                                                           execution_status = abap_true
                                                                           start_timestamp  = abap_true
                                                                           end_timestamp    = abap_true
                                                                           input_prompt     = abap_true
                                                                           decision_log     = abap_true
                                                                           output_response  = abap_true ) ) )
                                IMPORTING et_axc_query    = DATA(lt_axc_query)
                                CHANGING  cs_reported     = cs_axc_reported
                                          cs_failed       = cs_axc_failed ).

    ASSIGN lt_axc_query[ 1 ] TO FIELD-SYMBOL(<ls_execution_query>).
    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDIF.

    ls_execution_query = <ls_execution_query>.
    es_execution_query = ls_execution_query.

    lo_axc_service->rba_step( EXPORTING it_rba_step_k = VALUE #( ( query_uuid = ls_execution_query-query_uuid
                                                                   control    = VALUE #(
                                                                       step_uuid       = abap_true
                                                                       step_number     = abap_true
                                                                       query_uuid      = abap_true
                                                                       run_uuid        = abap_true
                                                                       tool_uuid       = abap_true
                                                                       execution_seq   = abap_true
                                                                       step_status     = abap_true
                                                                       start_timestamp = abap_true
                                                                       end_timestamp   = abap_true
                                                                       input_prompt    = abap_true
                                                                       output_prompt   = abap_true ) ) )
                              IMPORTING et_axc_step   = DATA(lt_execution_steps)
                              CHANGING  cs_reported   = cs_axc_reported
                                        cs_failed     = cs_axc_failed ).

    IF    ls_execution_header IS INITIAL
       OR ls_execution_query  IS INITIAL
       OR lt_execution_steps  IS INITIAL.
      RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDIF.

    fetch_agent_definition_by_uuid( EXPORTING iv_agent_uuid = ls_execution_header-agent_uuid
                                    IMPORTING es_agent      = es_agent
                                              eo_service    = lo_adf_service
                                    CHANGING  cs_reported   = cs_adf_reported
                                              cs_failed     = cs_adf_failed ).

    IF es_agent-status <> zpru_if_adf_type_and_constant=>cs_agent_status-active.
      RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDIF.

    lo_adf_service->rba_tool( EXPORTING it_rba_tool_k = VALUE #( ( agent_uuid                   = es_agent-agent_uuid
                                                                   control-tool_uuid            = abap_true
                                                                   control-agent_uuid           = abap_true
                                                                   control-tool_name            = abap_true
                                                                   control-tool_provider        = abap_true
                                                                   control-step_type            = abap_true
                                                                   control-tool_schema_provider = abap_true
                                                                   control-tool_info_provider   = abap_true
                                                                   control-is_borrowed          = abap_true
                                                                   control-is_transient         = abap_true    ) )
                              IMPORTING et_tool       = DATA(lt_agent_tools)
                              CHANGING  cs_reported   = cs_adf_reported
                                        cs_failed     = cs_adf_failed ).

    et_agent_tools = lt_agent_tools.
    et_execution_steps = lt_execution_steps.
  ENDMETHOD.

  METHOD zpru_if_api_agent~add_query_2_run.
    DATA lo_axc_service TYPE REF TO zpru_if_axc_service.

    CLEAR ev_run_uuid.
    CLEAR ev_query_uuid.

    IF    iv_run_uuid    IS INITIAL
       OR iv_input_query IS INITIAL.
      RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDIF.

    TRY.
        lo_axc_service ?= zpru_cl_agent_service_mngr=>get_service(
                              iv_service = `ZPRU_IF_AXC_SERVICE`
                              iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDTRY.

    lo_axc_service->read_header(
      EXPORTING it_head_read_k = VALUE #( ( run_uuid = iv_run_uuid
                                            control  = VALUE #( run_uuid           = abap_true
                                                                run_id             = abap_true
                                                                agent_uuid         = abap_true
                                                                user_id            = abap_true
                                                                start_timestamp    = abap_true
                                                                created_by         = abap_true
                                                                created_at         = abap_true
                                                                changed_by         = abap_true
                                                                last_changed       = abap_true
                                                                local_last_changed = abap_true ) ) )
      IMPORTING et_axc_head    = DATA(lt_axc_head)
      CHANGING  cs_reported    = cs_axc_reported
                cs_failed      = cs_axc_failed ).

    DATA(ls_execution_header) = VALUE #( lt_axc_head[ 1 ] OPTIONAL ).
    IF ls_execution_header IS INITIAL.
      RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDIF.

    setup_agent_context( EXPORTING iv_agent_uuid             = ls_execution_header-agent_uuid
                         IMPORTING es_agent                  = DATA(ls_agent)
                                   et_agent_tools            = DATA(lt_agent_tools)
                                   eo_decision_provider      = DATA(lo_decision_provider)
                                   eo_short_memory           = DATA(lo_short_memory)
                                   eo_long_memory            = DATA(lo_long_memory)
                                   eo_agent_info_provider    = DATA(lo_agent_info_provider)
                                   eo_system_prompt_provider = DATA(lo_system_prompt_provider)
                         CHANGING  cs_adf_reported           = cs_adf_reported
                                   cs_adf_failed             = cs_adf_failed ).

    DATA(lo_controller) = get_controller( ).
    lo_controller->mv_agent_uuid = ls_agent-agent_uuid.

    process_decision_engine( EXPORTING is_agent                  = ls_agent
                                       it_agent_tools            = lt_agent_tools
                                       io_controller             = lo_controller
                                       iv_input_query            = iv_input_query
                                       io_decision_provider      = lo_decision_provider
                                       io_system_prompt_provider = lo_system_prompt_provider
                                       io_short_memory           = lo_short_memory
                                       io_long_memory            = lo_long_memory
                                       io_agent_info_provider    = lo_agent_info_provider
                                       iv_stage                  = 'ADD_QUERY_2_RUN'
                             IMPORTING et_execution_plan         = DATA(lt_execution_plan)
                                       ev_first_tool_input       = DATA(lv_first_tool_input)
                                       ev_langu                  = DATA(lv_langu)
                                       ev_decision_log           = DATA(lv_decision_log) ).

    TRY.
        DATA(lo_utility) = CAST zpru_if_agent_util( zpru_cl_agent_service_mngr=>get_service(
                                                        iv_service = `ZPRU_IF_AGENT_UTIL`
                                                        iv_context = zpru_if_agent_frw=>cs_context-standard ) ).
      CATCH zpru_cx_agent_core.
        RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDTRY.

    GET TIME STAMP FIELD DATA(lv_now).

    DATA(lv_decision_log_message) = |\{ "USER": "{ sy-uname }", "TOPIC" : "DECISION_LOG", "TIMESTAMP" : "{ lv_now }",| &&
                                    | "CONTENT" : "{ lv_decision_log }" \}|.

    TRY.
        DATA(ls_execution_query) = VALUE zpru_if_axc_type_and_constant=>ts_query_create_imp(
            query_uuid       = cl_system_uuid=>create_uuid_x16_static( )
            query_number     = lo_axc_service->generate_query_number( iv_run_uuid = ls_execution_header-run_uuid )
            run_uuid         = ls_execution_header-run_uuid
            language         = COND #( WHEN lv_langu IS NOT INITIAL THEN lv_langu ELSE sy-langu )
            execution_status = zpru_if_axc_type_and_constant=>sc_query_status-new
            start_timestamp  = lv_now
            input_prompt     = lo_utility->search_node_in_json( iv_json           = iv_input_query
                                                                iv_field_2_search = 'CONTENT' )
            decision_log     = lo_utility->search_node_in_json( iv_json           = lv_decision_log_message
                                                                iv_field_2_search = 'CONTENT' )
            control          = VALUE #( query_uuid       = abap_true
                                        query_number     = abap_true
                                        run_uuid         = abap_true
                                        language         = abap_true
                                        execution_status = abap_true
                                        start_timestamp  = abap_true
                                        end_timestamp    = abap_true
                                        input_prompt     = abap_true
                                        decision_log     = abap_true
                                        output_response  = abap_true ) ).
      CATCH cx_uuid_error.
        RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDTRY.

    lo_axc_service->cba_query( EXPORTING it_axc_query_imp = VALUE #( ( ls_execution_query ) )
                               CHANGING  cs_reported      = cs_axc_reported
                                         cs_failed        = cs_axc_failed
                                         cs_mapped        = cs_axc_mapped ).

    DATA(lt_message_in) = VALUE zpru_if_short_memory_provider=>tt_message(
        ( message_cid  = |{ lv_now }-{ sy-uname }-ADD_QUERY_2_RUN_3|
          stage        = 'ADD_QUERY_2_RUN'
          sub_stage    = 'AFTER_QUERY_CREATION'
          namespace    = |{ sy-uname }.{ ls_agent-agent_name }.{ ls_execution_header-run_id }|
          user_name    = sy-uname
          agent_uuid   = ls_agent-agent_uuid
          run_uuid     = ls_execution_header-run_uuid
          query_uuid   = ls_execution_query-query_uuid
          message_time = lv_now
          content      = |\{ "AGENT_NAME" : "{ ls_agent-agent_name }", | &&
                         | "RUN_ID" : "{ ls_execution_header-run_id }", | &&
                         | "QUERY_NUMBER" : "{ ls_execution_query-query_number }", | &&
                         | "LANGUAGE" : "{ ls_execution_query-language }", | &&
                         | "QUERY" : { iv_input_query }, | &&
                         | "DECISION LOG" : { lv_decision_log_message } \}|
          message_type = zpru_if_short_memory_provider=>cs_msg_type-query ) ).
    lo_short_memory->save_message( lt_message_in ).

    construct_execution_steps( EXPORTING it_agent_tools      = lt_agent_tools
                                         is_agent            = ls_agent
                                         is_execution_header = ls_execution_header
                                         is_execution_query  = CORRESPONDING #( ls_execution_query )
                                         iv_first_tool_input = lv_first_tool_input
                                         io_short_memory     = lo_short_memory
                                         iv_stage            = 'ADD_QUERY_2_RUN'
                                         iv_count            = 4
                               CHANGING  ct_execution_plan   = lt_execution_plan
                                         cs_axc_reported     = cs_axc_reported
                                         cs_axc_failed       = cs_axc_failed ).

    ev_run_uuid   = ls_execution_header-run_uuid.
    ev_query_uuid = ls_execution_query-query_uuid.
  ENDMETHOD.

  METHOD get_long_memory.
    IF iv_agent_uuid IS INITIAL.
      RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDIF.

    prepare_memory_provider( EXPORTING iv_agent_uuid   = iv_agent_uuid
                             IMPORTING eo_short_memory = DATA(lo_short_memory)
                             CHANGING  cs_reported     = cs_reported
                                       cs_failed       = cs_failed ).

    IF lo_short_memory IS NOT BOUND.
      RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDIF.

    eo_long_memory = lo_short_memory->get_long_memory( ).
  ENDMETHOD.

  METHOD get_controller.
    IF mo_controller IS BOUND.
      ro_controller = mo_controller.
      RETURN.
    ENDIF.

    TRY.
        mo_controller ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_AGENT_CONTROLLER`
                                                                  iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDTRY.

    mo_controller->mo_api_agent = me.
    ro_controller = mo_controller.
  ENDMETHOD.

  METHOD execute_mini_loop.
    DATA lo_input            TYPE REF TO zpru_if_payload.
    DATA lo_output           TYPE REF TO zpru_if_payload.
    DATA lo_last_output      TYPE REF TO zpru_if_payload.
    DATA lo_axc_service      TYPE REF TO zpru_if_axc_service.
    DATA lo_agty_service     TYPE REF TO zpru_if_agty_service.
    DATA lt_query_update_imp TYPE zpru_if_axc_type_and_constant=>tt_query_update_imp.
    DATA lt_step_update_imp  TYPE zpru_if_axc_type_and_constant=>tt_step_update_imp.
    DATA lv_error_flag       TYPE abap_boolean.
    DATA lt_message          TYPE zpru_if_short_memory_provider=>tt_message.
    DATA lv_input_prompt     TYPE string.
    DATA lv_output_prompt    TYPE string.
    DATA lo_short_memory     TYPE REF TO zpru_if_short_memory_provider.

    TRY.
        lo_axc_service ?= zpru_cl_agent_service_mngr=>get_service(
                              iv_service = `ZPRU_IF_AXC_SERVICE`
                              iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDTRY.

    prepare_memory_provider( EXPORTING iv_agent_uuid   = is_agent-agent_uuid
                             IMPORTING eo_short_memory = lo_short_memory
                             CHANGING  cs_reported     = cs_adf_reported
                                       cs_failed       = cs_adf_failed ).

    DATA(lo_controller) = get_controller( ).

    DATA(lt_step_before) = lo_controller->mt_execution_steps.
    DELETE lt_step_before WHERE execution_seq > is_current_step-execution_seq.
    DATA(lt_step_after) = lo_controller->mt_execution_steps.
    DELETE lt_step_after WHERE execution_seq <= is_current_step-execution_seq.

    resequence_steps( EXPORTING it_additional_steps = it_additional_steps
                                is_execution_header = is_execution_header
                                is_execution_query  = is_execution_query
                                iv_output_prompt    = iv_output_prompt
                      CHANGING  ct_step_before      = lt_step_before
                                ct_step_after       = lt_step_after
                                ct_step_update_imp  = lt_step_update_imp ).

    IF lt_step_update_imp IS NOT INITIAL.
      lo_axc_service->update_step( EXPORTING it_step_update_imp = lt_step_update_imp
                                   CHANGING  cs_reported        = cs_axc_reported
                                             cs_failed          = cs_axc_failed ).
    ENDIF.

    CLEAR lt_step_update_imp.

    lo_controller->mt_execution_steps = lt_step_before.

    ASSIGN lo_controller->mt_input_output[ number = lines( lo_controller->mt_input_output ) ] TO FIELD-SYMBOL(<ls_input_output>).
    IF sy-subrc = 0.
      RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDIF.

    <ls_input_output>-execution_steps = lt_step_before.

    TRY.
        lo_agty_service ?= zpru_cl_agent_service_mngr=>get_service(
                               iv_service = `ZPRU_IF_AGTY_SERVICE`
                               iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDTRY.

    IF lo_controller->mv_max_number_of_loops IS INITIAL.
      lo_agty_service->read_agent_type(
        EXPORTING it_agty_read_k = VALUE #( ( agent_type            = is_agent-agent_type
                                              control-max_numb_loop = abap_true ) )
        IMPORTING et_agty        = DATA(lt_agty) ).

      DATA(lv_number_of_loops) = VALUE #( lt_agty[ 1 ]-max_numb_loop OPTIONAL ).
      IF lv_number_of_loops IS INITIAL.
        lv_number_of_loops = 4.
      ENDIF.

      lo_controller->mv_max_number_of_loops = lv_number_of_loops.
    ENDIF.

    lo_controller->mv_real_number_of_loops += 1.

    IF lo_controller->mv_real_number_of_loops = lo_controller->mv_max_number_of_loops.
      CLEAR lo_controller->mv_real_number_of_loops.
      RETURN.
    ENDIF.

    DATA(lv_count) = 1.
    LOOP AT it_additional_steps ASSIGNING FIELD-SYMBOL(<ls_additional_step>).

      ASSIGN it_additional_tools[ tool_uuid = <ls_additional_step>-tool_uuid ] TO FIELD-SYMBOL(<ls_additional_tool>).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      APPEND INITIAL LINE TO lo_controller->mt_run_context ASSIGNING FIELD-SYMBOL(<ls_run_context>).
      <ls_run_context>-tool_master_data = <ls_additional_tool>.
      <ls_run_context>-execution_step   = <ls_additional_step>.

      IF lv_count = 1.
        TRY.
            lo_input ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_PAYLOAD`
                                                                 iv_context = zpru_if_agent_frw=>cs_context-standard ).
          CATCH zpru_cx_agent_core.
            RAISE EXCEPTION NEW zpru_cx_agent_core( ).
        ENDTRY.

        lo_input->set_data( ir_data = REF #( iv_output_prompt ) ).

        TRY.
            lo_output ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_PAYLOAD`
                                                                  iv_context = zpru_if_agent_frw=>cs_context-standard ).
          CATCH zpru_cx_agent_core.
            RAISE EXCEPTION NEW zpru_cx_agent_core( ).
        ENDTRY.
      ELSE.
        IF lv_output_prompt IS NOT INITIAL.
          lo_input->set_data( ir_data = REF #( lv_output_prompt ) ).
        ELSE.
          lo_input->clear_data( ).
        ENDIF.
        lo_output->clear_data( ).
      ENDIF.

      lv_error_flag = abap_false.

      GET TIME STAMP FIELD DATA(lv_now).

      execute_tool_logic( EXPORTING io_controller       = lo_controller
                                    io_input            = lo_input
                                    is_tool_master_data = <ls_additional_tool>
                                    is_execution_step   = <ls_additional_step>
                          IMPORTING eo_response         = lo_output
                                    ev_error_flag       = lv_error_flag
                                    et_additional_steps = DATA(lt_additional_steps)
                                    et_additional_tools = DATA(lt_additional_tools) ).

      IF     lt_additional_steps IS NOT INITIAL
         AND lt_additional_tools IS NOT INITIAL.
        execute_mini_loop(
          EXPORTING
            is_agent            = is_agent
            is_execution_header = is_execution_header
            is_execution_query  = is_execution_query
            it_additional_steps = lt_additional_steps
            it_additional_tools = lt_additional_tools
            is_tool_master_data = <ls_additional_tool>
            is_current_step     = <ls_additional_step>
            iv_output_prompt    = lo_output->get_data( )->*
          IMPORTING
            eo_final_response   = lo_output
          CHANGING
            cs_axc_reported     = cs_axc_reported
            cs_axc_failed       = cs_axc_failed
            cs_adf_reported     = cs_adf_reported
            cs_adf_failed       = cs_adf_failed ).
      ENDIF.

      lv_input_prompt  = lo_input->get_data( )->*.
      lv_output_prompt = lo_output->get_data( )->*.

      APPEND log_step_execution( is_agent            = is_agent
                                 is_execution_header = is_execution_header
                                 is_execution_query  = is_execution_query
                                 is_execution_step   = <ls_additional_step>
                                 is_tool_master_data = <ls_additional_tool>
                                 iv_input_prompt     = lv_input_prompt
                                 iv_output_prompt    = lv_output_prompt
                                 iv_error_flag       = lv_error_flag
                                 iv_count            = lv_count ) TO lt_message.

      lv_count += 1.

      IF lv_error_flag = abap_true.
        APPEND INITIAL LINE TO lt_step_update_imp ASSIGNING FIELD-SYMBOL(<ls_step_2_upd>).
        <ls_step_2_upd>-step_uuid     = <ls_additional_step>-step_uuid.
        <ls_step_2_upd>-query_uuid    = <ls_additional_step>-query_uuid.
        <ls_step_2_upd>-run_uuid      = <ls_additional_step>-run_uuid.
        <ls_step_2_upd>-step_status   = zpru_if_axc_type_and_constant=>sc_step_status-error.
        <ls_step_2_upd>-end_timestamp = lv_now.
        <ls_step_2_upd>-input_prompt  = lv_input_prompt.
        <ls_step_2_upd>-output_prompt = lv_output_prompt.
        <ls_step_2_upd>-control-step_status   = abap_true.
        <ls_step_2_upd>-control-end_timestamp = abap_true.
        <ls_step_2_upd>-control-input_prompt  = abap_true.
        <ls_step_2_upd>-control-output_prompt = abap_true.

        <ls_run_context>-execution_step-step_status   = zpru_if_axc_type_and_constant=>sc_step_status-error.
        <ls_run_context>-execution_step-end_timestamp = lv_now.
        <ls_run_context>-execution_step-input_prompt  = lv_input_prompt.
        <ls_run_context>-execution_step-output_prompt = lv_output_prompt.

        APPEND INITIAL LINE TO lt_query_update_imp ASSIGNING FIELD-SYMBOL(<ls_query_2_upd>).
        <ls_query_2_upd>-query_uuid       = is_execution_query-query_uuid.
        <ls_query_2_upd>-run_uuid         = is_execution_query-run_uuid.
        <ls_query_2_upd>-execution_status = zpru_if_axc_type_and_constant=>sc_query_status-error.
        <ls_query_2_upd>-end_timestamp    = lv_now.
        <ls_query_2_upd>-control-execution_status = abap_true.
        <ls_query_2_upd>-control-end_timestamp    = abap_true.

        EXIT.
      ENDIF.

      APPEND INITIAL LINE TO lt_step_update_imp ASSIGNING <ls_step_2_upd>.
      <ls_step_2_upd>-step_uuid     = <ls_additional_step>-step_uuid.
      <ls_step_2_upd>-query_uuid    = <ls_additional_step>-query_uuid.
      <ls_step_2_upd>-run_uuid      = <ls_additional_step>-run_uuid.
      <ls_step_2_upd>-step_status   = zpru_if_axc_type_and_constant=>sc_step_status-complete.
      <ls_step_2_upd>-end_timestamp = lv_now.
      <ls_step_2_upd>-input_prompt  = lv_input_prompt.
      <ls_step_2_upd>-output_prompt = lv_output_prompt.
      <ls_step_2_upd>-control-step_status   = abap_true.
      <ls_step_2_upd>-control-end_timestamp = abap_true.
      <ls_step_2_upd>-control-input_prompt  = abap_true.
      <ls_step_2_upd>-control-output_prompt = abap_true.

      <ls_run_context>-execution_step-step_status   = zpru_if_axc_type_and_constant=>sc_step_status-complete.
      <ls_run_context>-execution_step-end_timestamp = lv_now.
      <ls_run_context>-execution_step-input_prompt  = lv_input_prompt.
      <ls_run_context>-execution_step-output_prompt = lv_output_prompt.

      IF lo_controller->mv_stop_agent = abap_true.
        EXIT.
      ENDIF.

    ENDLOOP.

    <ls_input_output>-run_context = lo_controller->mt_run_context.

    DATA(lv_last_output) = lv_output_prompt.

    TRY.
        lo_last_output ?= zpru_cl_agent_service_mngr=>get_service(
                              iv_service = `ZPRU_IF_PAYLOAD`
                              iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDTRY.

    lo_last_output->set_data( ir_data = NEW string( lv_last_output ) ).

    IF lt_message IS NOT INITIAL.
      lo_short_memory->save_message( it_message = lt_message ).
    ENDIF.

    IF lt_step_update_imp IS NOT INITIAL.
      lo_axc_service->update_step( EXPORTING it_step_update_imp = lt_step_update_imp
                                   CHANGING  cs_reported        = cs_axc_reported
                                             cs_failed          = cs_axc_failed ).
    ENDIF.

    IF lv_error_flag = abap_true.
      IF lt_query_update_imp IS NOT INITIAL.
        lo_axc_service->update_query( EXPORTING it_query_update_imp = lt_query_update_imp
                                      CHANGING  cs_reported         = cs_axc_reported
                                                cs_failed           = cs_axc_failed ).

      ENDIF.
      RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ELSE.
      eo_final_response = lo_last_output.
    ENDIF.
  ENDMETHOD.

  METHOD execute_tool_logic.
    DATA lo_tool_provider TYPE REF TO zpru_if_tool_provider.
    DATA lo_executor      TYPE REF TO zpru_if_tool_executor.

    CREATE OBJECT lo_tool_provider TYPE (is_tool_master_data-tool_provider).
    lo_executor = lo_tool_provider->get_tool( is_tool_master_data = is_tool_master_data
                                              is_execution_step   = is_execution_step ).

    CASE is_tool_master_data-step_type.
      WHEN zpru_if_adf_type_and_constant=>cs_step_type-abap_code.
        CAST zpru_if_abap_executor( lo_executor )->execute_code( EXPORTING io_controller       = io_controller
                                                                           io_request          = io_input
                                                                           is_tool_master_data = is_tool_master_data
                                                                           is_execution_step   = is_execution_step
                                                                 IMPORTING eo_response         = eo_response
                                                                           ev_error_flag       = ev_error_flag
                                                                           et_additional_steps = et_additional_steps
                                                                           et_additional_tools = et_additional_tools ).

      WHEN zpru_if_adf_type_and_constant=>cs_step_type-knowledge_source.
        CAST zpru_if_knowledge_provider( lo_executor )->lookup_knowledge(
                                                       EXPORTING io_controller       = io_controller
                                                                 io_request          = io_input
                                                                 is_tool_master_data = is_tool_master_data
                                                                 is_execution_step   = is_execution_step
                                                       IMPORTING eo_response         = eo_response
                                                                 ev_error_flag       = ev_error_flag
                                                                 et_additional_steps = et_additional_steps
                                                                 et_additional_tools = et_additional_tools ).

      WHEN zpru_if_adf_type_and_constant=>cs_step_type-nested_agent.
        CAST zpru_if_nested_agent_runner( lo_executor )->run_nested_agent(
                                                        EXPORTING io_controller       = io_controller
                                                                  io_request          = io_input
                                                                  is_tool_master_data = is_tool_master_data
                                                                  is_execution_step   = is_execution_step
                                                        IMPORTING eo_response         = eo_response
                                                                  ev_error_flag       = ev_error_flag
                                                                  et_additional_steps = et_additional_steps
                                                                  et_additional_tools = et_additional_tools ).

      WHEN zpru_if_adf_type_and_constant=>cs_step_type-http_request.
        CAST zpru_if_http_request_sender( lo_executor )->send_http(
                                                        EXPORTING io_controller       = io_controller
                                                                  io_request          = io_input
                                                                  is_tool_master_data = is_tool_master_data
                                                                  is_execution_step   = is_execution_step
                                                        IMPORTING eo_response         = eo_response
                                                                  ev_error_flag       = ev_error_flag
                                                                  et_additional_steps = et_additional_steps
                                                                  et_additional_tools = et_additional_tools ).

      WHEN zpru_if_adf_type_and_constant=>cs_step_type-service_consumption_model.
        CAST zpru_if_service_model_consumer( lo_executor )->consume_service_model(
                                                           EXPORTING io_controller       = io_controller
                                                                     io_request          = io_input
                                                                     is_tool_master_data = is_tool_master_data
                                                                     is_execution_step   = is_execution_step
                                                           IMPORTING eo_response         = eo_response
                                                                     ev_error_flag       = ev_error_flag
                                                                     et_additional_steps = et_additional_steps
                                                                     et_additional_tools = et_additional_tools ).

      WHEN zpru_if_adf_type_and_constant=>cs_step_type-call_llm.
        CAST zpru_if_llm_caller( lo_executor )->call_large_language_model(
                                               EXPORTING io_controller       = io_controller
                                                         io_request          = io_input
                                                         is_tool_master_data = is_tool_master_data
                                                         is_execution_step   = is_execution_step
                                               IMPORTING eo_response         = eo_response
                                                         ev_error_flag       = ev_error_flag
                                                         et_additional_steps = et_additional_steps
                                                         et_additional_tools = et_additional_tools ).

      WHEN zpru_if_adf_type_and_constant=>cs_step_type-dynamic_abap_code.
        CAST zpru_if_dynamic_abap_processor( lo_executor )->process_dynamic_abap(
                                                           EXPORTING io_controller       = io_controller
                                                                     io_request          = io_input
                                                                     is_tool_master_data = is_tool_master_data
                                                                     is_execution_step   = is_execution_step
                                                           IMPORTING eo_response         = eo_response
                                                                     ev_error_flag       = ev_error_flag
                                                                     et_additional_steps = et_additional_steps
                                                                     et_additional_tools = et_additional_tools ).

      WHEN zpru_if_adf_type_and_constant=>cs_step_type-infer_ml_model.
        CAST zpru_if_ml_model_inference( lo_executor )->get_machine_learning_inference(
                                                       EXPORTING io_controller       = io_controller
                                                                 io_request          = io_input
                                                                 is_tool_master_data = is_tool_master_data
                                                                 is_execution_step   = is_execution_step
                                                       IMPORTING eo_response         = eo_response
                                                                 ev_error_flag       = ev_error_flag
                                                                 et_additional_steps = et_additional_steps
                                                                 et_additional_tools = et_additional_tools ).

      WHEN zpru_if_adf_type_and_constant=>cs_step_type-user_tool.
        CAST zpru_if_user_tool( lo_executor )->execute_user_tool( EXPORTING io_controller       = io_controller
                                                                            io_request          = io_input
                                                                            is_tool_master_data = is_tool_master_data
                                                                            is_execution_step   = is_execution_step
                                                                  IMPORTING eo_response         = eo_response
                                                                            ev_error_flag       = ev_error_flag
                                                                            et_additional_steps = et_additional_steps
                                                                            et_additional_tools = et_additional_tools ).
    ENDCASE.
  ENDMETHOD.

  METHOD finalize_successful_query.
    DATA lt_query_update_imp  TYPE zpru_if_axc_type_and_constant=>tt_query_update_imp.
    DATA lo_decision_provider TYPE REF TO zpru_if_decision_provider.
    DATA lo_axc_service       TYPE REF TO zpru_if_axc_service.
    DATA lt_step_final_state  TYPE zpru_if_axc_type_and_constant=>tt_axc_step.

    TRY.
        lo_axc_service ?= zpru_cl_agent_service_mngr=>get_service(
                              iv_service = `ZPRU_IF_AXC_SERVICE`
                              iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDTRY.

    lo_axc_service->rba_step( EXPORTING it_rba_step_k = VALUE #( ( query_uuid          = is_execution_query-query_uuid
                                                                   control-run_uuid    = abap_true
                                                                   control-query_uuid  = abap_true
                                                                   control-step_status = abap_true  ) )
                              IMPORTING et_axc_step   = lt_step_final_state
                              CHANGING  cs_reported   = cs_axc_reported
                                        cs_failed     = cs_axc_failed ).

    LOOP AT lt_step_final_state TRANSPORTING NO FIELDS WHERE step_status <> zpru_if_axc_type_and_constant=>sc_step_status-complete.
      RETURN.
    ENDLOOP.

    GET TIME STAMP FIELD DATA(lv_now).

    APPEND INITIAL LINE TO lt_query_update_imp ASSIGNING FIELD-SYMBOL(<ls_query_2_upd>).
    <ls_query_2_upd>-query_uuid       = is_execution_query-query_uuid.
    <ls_query_2_upd>-run_uuid         = is_execution_query-run_uuid.
    <ls_query_2_upd>-execution_status = zpru_if_axc_type_and_constant=>sc_query_status-complete.
    <ls_query_2_upd>-end_timestamp    = lv_now.
    <ls_query_2_upd>-control-execution_status = abap_true.
    <ls_query_2_upd>-control-end_timestamp    = abap_true.

    CREATE OBJECT lo_decision_provider TYPE (is_agent-decision_provider).

    lo_decision_provider->prepare_final_response( EXPORTING iv_run_uuid       = is_execution_query-run_uuid
                                                            iv_query_uuid     = is_execution_query-query_uuid
                                                            io_last_output    = io_last_output
                                                  IMPORTING eo_final_response = eo_final_response
                                                  CHANGING  cs_axc_reported   = cs_axc_reported
                                                            cs_axc_failed     = cs_axc_failed
                                                            cs_adf_reported   = cs_adf_reported
                                                            cs_adf_failed     = cs_adf_failed   ).
    IF eo_final_response IS BOUND.
      <ls_query_2_upd>-output_response = eo_final_response->get_data( )->*.
      <ls_query_2_upd>-control-output_response = abap_true.
    ENDIF.

    lo_axc_service->update_query( EXPORTING it_query_update_imp = lt_query_update_imp
                                  CHANGING  cs_reported         = cs_axc_reported
                                            cs_failed           = cs_axc_failed ).

    DATA(lv_final_response_message) = |\{ "USER": "{ sy-uname }", "TOPIC" : "FINAL_RESPONSE", "TIMESTAMP" : "{ lv_now }",| &&
                                      | "CONTENT" : "{ <ls_query_2_upd>-output_response }" \}|.

    DATA(lv_last_number) = lines( io_controller->mt_input_output ).
    ASSIGN io_controller->mt_input_output[ number = lv_last_number ] TO FIELD-SYMBOL(<ls_input_output>).
    IF sy-subrc = 0.
      <ls_input_output>-output_response = lv_final_response_message.
    ENDIF.

    DATA(lt_message) = VALUE zpru_if_short_memory_provider=>tt_message(
        ( message_cid  = |{ lv_now }-{ sy-uname }-PROCESS_EXECUTION_STEPS_FINAL|
          stage        = 'PROCESS_EXECUTION_STEPS'
          sub_stage    = |FINAL_RESPONSE|
          namespace    = |{ sy-uname }.{ is_agent-agent_name }.{ is_execution_header-run_id }.{ is_execution_query-query_number }|
          user_name    = sy-uname
          agent_uuid   = is_agent-agent_uuid
          run_uuid     = is_execution_query-run_uuid
          query_uuid   = is_execution_query-query_uuid
          message_time = lv_now
          content      = |\{ "RUN_ID" : "{ is_execution_header-run_id }", | &&
                         | "QUERY_NUMBER" : "{ is_execution_query-query_number }", | &&
                         | "FINAL_RESPONSE" : { lv_final_response_message }  \}|
          message_type = zpru_if_short_memory_provider=>cs_msg_type-step_output  ) ).

    io_short_memory->save_message( it_message = lt_message ).
  ENDMETHOD.

  METHOD resequence_steps.
    DATA(lv_count_before) = VALUE #( ct_step_before[ lines( ct_step_before ) ]-execution_seq OPTIONAL ).
    DATA(lv_first_iteration) = abap_true.

    LOOP AT it_additional_steps ASSIGNING FIELD-SYMBOL(<ls_add_exec_step>).
      lv_count_before += 1.

      GET TIME STAMP FIELD DATA(lv_now).

      APPEND INITIAL LINE TO ct_step_update_imp ASSIGNING FIELD-SYMBOL(<ls_step_2_upd>).
      <ls_step_2_upd>-step_uuid       = <ls_add_exec_step>-step_uuid.
      <ls_step_2_upd>-step_number     = <ls_add_exec_step>-step_number.
      <ls_step_2_upd>-query_uuid      = is_execution_query-query_uuid.
      <ls_step_2_upd>-run_uuid        = is_execution_header-run_uuid.
      <ls_step_2_upd>-tool_uuid       = <ls_add_exec_step>-tool_uuid.
      <ls_step_2_upd>-execution_seq   = lv_count_before.
      <ls_step_2_upd>-step_status     = zpru_if_axc_type_and_constant=>sc_step_status-new.
      <ls_step_2_upd>-start_timestamp = lv_now.

      IF lv_first_iteration = abap_true.
        <ls_step_2_upd>-input_prompt = iv_output_prompt.
      ENDIF.

      <ls_step_2_upd>-control-step_uuid       = abap_true.
      <ls_step_2_upd>-control-step_number     = abap_true.
      <ls_step_2_upd>-control-query_uuid      = abap_true.
      <ls_step_2_upd>-control-run_uuid        = abap_true.
      <ls_step_2_upd>-control-tool_uuid       = abap_true.
      <ls_step_2_upd>-control-execution_seq   = abap_true.
      <ls_step_2_upd>-control-step_status     = abap_true.
      <ls_step_2_upd>-control-start_timestamp = abap_true.
      IF lv_first_iteration = abap_true.
        <ls_step_2_upd>-control-input_prompt = abap_true.
      ENDIF.

      APPEND INITIAL LINE TO ct_step_before ASSIGNING FIELD-SYMBOL(<ls_step_before>).
      <ls_step_before> = CORRESPONDING #( <ls_step_2_upd> ).

      lv_first_iteration = abap_false.
    ENDLOOP.

    LOOP AT ct_step_after ASSIGNING FIELD-SYMBOL(<ls_step_after>).
      lv_count_before += 1.

      APPEND INITIAL LINE TO ct_step_update_imp ASSIGNING <ls_step_2_upd>.
      <ls_step_2_upd>-step_uuid     = <ls_step_after>-step_uuid.
      <ls_step_2_upd>-query_uuid    = is_execution_query-query_uuid.
      <ls_step_2_upd>-run_uuid      = is_execution_header-run_uuid.
      <ls_step_2_upd>-execution_seq = lv_count_before.
      <ls_step_2_upd>-control-execution_seq = abap_true.

      APPEND INITIAL LINE TO ct_step_before ASSIGNING <ls_step_before>.
      <ls_step_before> = CORRESPONDING #( <ls_step_2_upd> ).

    ENDLOOP.
  ENDMETHOD.

  METHOD log_step_execution.
    GET TIME STAMP FIELD DATA(lv_now).

    DATA(lv_input_tool_prompt_message) = |\{ "USER": "{ sy-uname }", "TOPIC" : "TOOL_INPUT_PROMPT", "TIMESTAMP" : "{ lv_now }",| &&
                                         | "CONTENT" : "{ iv_input_prompt }" \}|.

    DATA(lv_output_tool_prompt_message) = |\{ "USER": "{ sy-uname }", "TOPIC" : "TOOL_OUTPUT_PROMPT", "TIMESTAMP" : "{ lv_now }",| &&
                                          | "CONTENT" : "{ iv_output_prompt }" \}|.

    rs_message = VALUE #(
        message_cid  = |{ lv_now }-{ sy-uname }-PROCESS_EXECUTION_STEPS_{ iv_count }|
        stage        = 'PROCESS_EXECUTION_STEPS'
        sub_stage    = |STEP_{ is_execution_step-execution_seq }|
        namespace    = |{ sy-uname }.{ is_agent-agent_name }.{ is_execution_header-run_id }.{ is_execution_query-query_number }|
        user_name    = sy-uname
        agent_uuid   = is_agent-agent_uuid
        run_uuid     = is_execution_step-run_uuid
        query_uuid   = is_execution_step-query_uuid
        step_uuid    = is_execution_step-step_uuid
        message_time = lv_now
        content      = |\{ "RUN_ID" : "{ is_execution_header-run_id }", | &&
                       | "QUERY_NUMBER" : "{ is_execution_query-query_number }", | &&
                       | "STEP_NUMBER" : "{ is_execution_step-step_number }", | &&
                       | "EXECUTION_SEQ" : "{ is_execution_step-execution_seq }", | &&
                       | "TOOL_NAME" : "{ is_tool_master_data-tool_name }", | &&
                       | "STEP_TYPE" : "{ is_tool_master_data-step_type }", | &&
                       | "INPUT_PROMPT" : { lv_input_tool_prompt_message }, | &&
                       | "OUTPUT_PROMPT" : { lv_output_tool_prompt_message }, | &&
                       | "ERROR" : "{ iv_error_flag }"  \}|
        message_type = zpru_if_short_memory_provider=>cs_msg_type-step_output ).
  ENDMETHOD.

  METHOD setup_agent_context.
    DATA lo_adf_service TYPE REF TO zpru_if_adf_service.

    fetch_agent_definition_by_uuid( EXPORTING iv_agent_uuid = iv_agent_uuid
                                    IMPORTING es_agent      = es_agent
                                              eo_service    = lo_adf_service
                                    CHANGING  cs_reported   = cs_adf_reported
                                              cs_failed     = cs_adf_failed ).

    IF es_agent-status <> zpru_if_adf_type_and_constant=>cs_agent_status-active.
      RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDIF.

    lo_adf_service->rba_tool( EXPORTING it_rba_tool_k = VALUE #( ( agent_uuid                   = es_agent-agent_uuid
                                                                   control-tool_uuid            = abap_true
                                                                   control-agent_uuid           = abap_true
                                                                   control-tool_name            = abap_true
                                                                   control-tool_provider        = abap_true
                                                                   control-step_type            = abap_true
                                                                   control-tool_schema_provider = abap_true
                                                                   control-tool_info_provider   = abap_true
                                                                   control-is_borrowed          = abap_true
                                                                   control-is_transient         = abap_true    ) )
                              IMPORTING et_tool       = et_agent_tools
                              CHANGING  cs_reported   = cs_adf_reported
                                        cs_failed     = cs_adf_failed ).

    CREATE OBJECT eo_decision_provider TYPE (es_agent-decision_provider).

    prepare_memory_provider( EXPORTING iv_agent_uuid   = es_agent-agent_uuid
                             IMPORTING eo_short_memory = eo_short_memory
                             CHANGING  cs_reported     = cs_adf_reported
                                       cs_failed       = cs_adf_failed ).

    get_long_memory( EXPORTING iv_agent_uuid  = es_agent-agent_uuid
                     IMPORTING eo_long_memory = eo_long_memory
                     CHANGING  cs_reported    = cs_adf_reported
                               cs_failed      = cs_adf_failed ).

    IF es_agent-agent_info_provider IS NOT INITIAL.
      CREATE OBJECT eo_agent_info_provider TYPE (es_agent-agent_info_provider).
    ENDIF.

    IF es_agent-system_prompt_provider IS NOT INITIAL.
      CREATE OBJECT eo_system_prompt_provider TYPE (es_agent-system_prompt_provider).
    ENDIF.
  ENDMETHOD.

  METHOD process_decision_engine.
    DATA lo_query            TYPE REF TO zpru_if_payload.
    DATA lo_utility          TYPE REF TO zpru_if_agent_util.
    DATA lo_execution_plan   TYPE REF TO zpru_if_payload.
    DATA lo_first_tool_input TYPE REF TO zpru_if_payload.
    DATA lo_langu            TYPE REF TO zpru_if_payload.
    DATA lo_decision_log     TYPE REF TO zpru_if_payload.

    TRY.
        lo_utility ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_AGENT_UTIL`
                                                               iv_context = zpru_if_agent_frw=>cs_context-standard ).

        lo_query ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_PAYLOAD`
                                                             iv_context = zpru_if_agent_frw=>cs_context-standard ).

        lo_first_tool_input ?= zpru_cl_agent_service_mngr=>get_service(
                                   iv_service = `ZPRU_IF_PAYLOAD`
                                   iv_context = zpru_if_agent_frw=>cs_context-standard ).

        lo_execution_plan ?= zpru_cl_agent_service_mngr=>get_service(
                                 iv_service = `ZPRU_IF_PAYLOAD`
                                 iv_context = zpru_if_agent_frw=>cs_context-standard ).

        lo_langu ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_PAYLOAD`
                                                             iv_context = zpru_if_agent_frw=>cs_context-standard ).

        lo_decision_log ?= zpru_cl_agent_service_mngr=>get_service(
                               iv_service = `ZPRU_IF_PAYLOAD`
                               iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDTRY.

    lo_query->set_data( ir_data = NEW zpru_if_agent_frw=>ts_json( lo_utility->search_node_in_json(
                                                                      iv_json           = iv_input_query
                                                                      iv_field_2_search = 'CONTENT' ) ) ).

    GET TIME STAMP FIELD DATA(lv_now).

    DATA(lv_system_prompt) = |\{ "USER": "{ sy-uname }", "TOPIC" : "SYSTEM_PROMPT", "TIMESTAMP" : "{ lv_now }",| &&
                             | "CONTENT" : "{ io_system_prompt_provider->get_system_prompt( ) }" \}|.

    DATA(lv_agent_info) = |\{ "USER": "{ sy-uname }", "TOPIC" : "AGENT_INFO", "TIMESTAMP" : "{ lv_now }",| &&
                             | "CONTENT" : "{ io_agent_info_provider->get_agent_info( ) }" \}|.

    DATA(lt_message_in) = VALUE zpru_if_short_memory_provider=>tt_message(
                                    ( message_cid  = |{ lv_now }-{ sy-uname }-{ iv_stage }_{ 1 }|
                                      stage        = iv_stage
                                      sub_stage    = 'BEFORE_DECISION'
                                      namespace    = |{ sy-uname }.{ is_agent-agent_name }|
                                      user_name    = sy-uname
                                      agent_uuid   = is_agent-agent_uuid
                                      message_time = lv_now
                                      content      = |\{ "AGENT_NAME" : "{ is_agent-agent_name }", | &&
                                                     | "DECISION_PROVIDER" : "{ is_agent-decision_provider }", | &&
                                                     | "QUERY" : { iv_input_query }, | &&
                                                     | "SYSTEM PROMPT" : { lv_system_prompt }, | &&
                                                     | "AGENT INFO" : "{ lv_agent_info }" \}|
                                      message_type = zpru_if_short_memory_provider=>cs_msg_type-info ) ).

    io_short_memory->save_message( lt_message_in ).

    io_decision_provider->call_decision_engine( EXPORTING is_agent               = is_agent
                                                          it_tool                = it_agent_tools
                                                          io_controller          = io_controller
                                                          io_input               = lo_query
                                                          io_system_prompt       = io_system_prompt_provider
                                                          io_short_memory        = io_short_memory
                                                          io_long_memory         = io_long_memory
                                                          io_agent_info_provider = io_agent_info_provider
                                                IMPORTING eo_execution_plan      = lo_execution_plan
                                                          eo_first_tool_input    = lo_first_tool_input
                                                          eo_langu               = lo_langu
                                                          eo_decision_log        = lo_decision_log ).

    IF lo_execution_plan IS BOUND.
      et_execution_plan = lo_execution_plan->get_data( )->*.
    ENDIF.

    IF et_execution_plan IS INITIAL.
      RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDIF.

    IF lo_decision_log IS BOUND.
      ev_decision_log = lo_decision_log->get_data( )->*.
    ENDIF.

    IF lo_first_tool_input IS BOUND.
      ev_first_tool_input = lo_first_tool_input->get_data( )->*.
    ENDIF.

    IF lo_langu IS BOUND.
      ev_langu = lo_langu->get_data( )->*.
    ENDIF.

    GET TIME STAMP FIELD lv_now.

    DATA(lv_decision_log_message) = |\{ "USER": "{ sy-uname }", "TOPIC" : "DECISION_LOG", "TIMESTAMP" : "{ lv_now }",| &&
                                    | "CONTENT" : "{ ev_decision_log }" \}|.

    DATA(lv_first_tool_input_message) = |\{ "USER": "{ sy-uname }", "TOPIC" : "FIRST_TOOL_INPUT", "TIMESTAMP" : "{ lv_now }",| &&
                                   | "CONTENT" : "{ ev_first_tool_input }" \}|.

    lt_message_in = VALUE #( ( message_cid  = |{ lv_now }-{ sy-uname }-{ iv_stage }_{ 2 }|
                               stage        = iv_stage
                               sub_stage    = 'AFTER_DECISION'
                               namespace    = |{ sy-uname }.{ is_agent-agent_name }|
                               user_name    = sy-uname
                               agent_uuid   = is_agent-agent_uuid
                               message_time = lv_now
                               content      = |\{ "AGENT_NAME" : "{ is_agent-agent_name }", | &&
                                              | "DECISION_PROVIDER" : "{ is_agent-decision_provider }", | &&
                                              | "QUERY" : { iv_input_query }, | &&
                                              | "FIRST TOOL INPUT" : { lv_first_tool_input_message }, | &&
                                              | "LANGUAGE" : "{ ev_langu }", | &&
                                              | "DECISION LOG" : { lv_decision_log_message } \}|
                               message_type = zpru_if_short_memory_provider=>cs_msg_type-info ) ).

    io_short_memory->save_message( lt_message_in ).
  ENDMETHOD.

  METHOD construct_execution_steps.
    DATA lo_axc_service      TYPE REF TO zpru_if_axc_service.
    DATA lv_step_number_base TYPE zpru_de_step_number.
    DATA lt_message_in       TYPE zpru_if_short_memory_provider=>tt_message.

    TRY.
        lo_axc_service ?= zpru_cl_agent_service_mngr=>get_service(
                              iv_service = `ZPRU_IF_AXC_SERVICE`
                              iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDTRY.

    SORT ct_execution_plan BY sequence ASCENDING.
    DATA(lv_min_seq) = VALUE #( ct_execution_plan[ 1 ]-sequence OPTIONAL ).

    GET TIME STAMP FIELD DATA(lv_now).
    DATA(lv_count) = iv_count.

    LOOP AT ct_execution_plan ASSIGNING FIELD-SYMBOL(<ls_tool>).
      ASSIGN it_agent_tools[ agent_uuid = <ls_tool>-agent_uuid
                             tool_name  = <ls_tool>-tool_name ] TO FIELD-SYMBOL(<ls_tool_master_data>).
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

      APPEND INITIAL LINE TO et_execution_steps ASSIGNING FIELD-SYMBOL(<ls_execution_step>).
      TRY.
          <ls_execution_step>-step_uuid = cl_system_uuid=>create_uuid_x16_static( ).
        CATCH cx_uuid_error.
          RAISE EXCEPTION NEW zpru_cx_agent_core( ).
      ENDTRY.

      <ls_execution_step>-step_number     = lo_axc_service->generate_step_number(
                                                iv_query_uuid       = is_execution_query-query_uuid
                                                iv_step_number_base = lv_step_number_base ).
      <ls_execution_step>-query_uuid      = is_execution_query-query_uuid.
      <ls_execution_step>-run_uuid        = is_execution_header-run_uuid.
      <ls_execution_step>-tool_uuid       = <ls_tool_master_data>-tool_uuid.
      <ls_execution_step>-execution_seq   = <ls_tool>-sequence.
      <ls_execution_step>-step_status     = zpru_if_axc_type_and_constant=>sc_step_status-new.
      <ls_execution_step>-start_timestamp = lv_now.

      IF <ls_tool>-sequence = lv_min_seq.
        <ls_execution_step>-input_prompt = iv_first_tool_input.
      ENDIF.

      DATA(lv_tool_prompt_message) = ||.
      IF <ls_execution_step>-input_prompt IS NOT INITIAL.
        lv_tool_prompt_message = |\{ "USER": "{ sy-uname }", "TOPIC" : "TOOL_INPUT_PROMPT", "TIMESTAMP" : "{ lv_now }",| &&
                                       | "CONTENT" : "{ <ls_execution_step>-input_prompt }" \}|.
      ENDIF.

      DATA(lv_content_json) = |\{ "STEP_NUMBER" : "{ <ls_execution_step>-step_number }", | &&
                              | "QUERY_NUMBER" : "{ is_execution_query-query_number }", | &&
                              | "RUN_ID" : "{ is_execution_header-run_id }", | &&
                              | "EXECUTION_SEQUENCE" : "{ <ls_execution_step>-execution_seq }", | &&
                              | "INPUT_PROMPT" : { lv_tool_prompt_message } \}|.

      APPEND INITIAL LINE TO lt_message_in ASSIGNING FIELD-SYMBOL(<ls_message>).
      <ls_message> = VALUE #(
          message_cid  = |{ lv_now }-{ sy-uname }-{ iv_stage }_{ lv_count }|
          stage        = iv_stage
          sub_stage    = 'STEP_ANALYSIS'
          namespace    = |{ sy-uname }.{ is_agent-agent_name }.{ is_execution_header-run_id }.{ is_execution_query-query_number }|
          user_name    = sy-uname
          agent_uuid   = is_agent-agent_uuid
          run_uuid     = is_execution_header-run_uuid
          query_uuid   = is_execution_query-query_uuid
          step_uuid    = <ls_execution_step>-step_uuid
          message_time = lv_now
          content      = lv_content_json
          message_type = zpru_if_short_memory_provider=>cs_msg_type-step_input ).

      lv_count += 1.
      lv_step_number_base = <ls_execution_step>-step_number.
    ENDLOOP.

    io_short_memory->save_message( lt_message_in ).

    lo_axc_service->cba_step( EXPORTING it_axc_step_imp = VALUE #( FOR <ls_s> IN et_execution_steps
                                                                   ( step_uuid       = <ls_s>-step_uuid
                                                                     step_number     = <ls_s>-step_number
                                                                     query_uuid      = <ls_s>-query_uuid
                                                                     run_uuid        = <ls_s>-run_uuid
                                                                     tool_uuid       = <ls_s>-tool_uuid
                                                                     execution_seq   = <ls_s>-execution_seq
                                                                     step_status     = <ls_s>-step_status
                                                                     start_timestamp = <ls_s>-start_timestamp
                                                                     end_timestamp   = <ls_s>-end_timestamp
                                                                     input_prompt    = <ls_s>-input_prompt
                                                                     output_prompt   = <ls_s>-output_prompt
                                                                     control         = VALUE #(
                                                                         step_uuid       = abap_true
                                                                         step_number     = abap_true
                                                                         query_uuid      = abap_true
                                                                         run_uuid        = abap_true
                                                                         tool_uuid       = abap_true
                                                                         execution_seq   = abap_true
                                                                         step_status     = abap_true
                                                                         start_timestamp = abap_true
                                                                         end_timestamp   = abap_true
                                                                         input_prompt    = abap_true
                                                                         output_prompt   = abap_true ) ) )
                              CHANGING  cs_reported     = cs_axc_reported
                                        cs_failed       = cs_axc_failed
                                        cs_mapped       = cs_axc_mapped ).
  ENDMETHOD.

  METHOD clear_internal_state.
    CLEAR: mv_output_response,
           mv_output_response_prev,
           mv_input_query,
           mo_controller,
           mo_long_memory,
           mo_short_memory.
  ENDMETHOD.

  METHOD assign_controller_context.
    DATA(lo_controller) = get_controller( ).

    IF io_parent_controller IS BOUND.
      lo_controller->mo_parent_controller = io_parent_controller.
    ENDIF.
  ENDMETHOD.

  METHOD fetch_agent_configuration.
    TRY.
        eo_service ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_ADF_SERVICE`
                                                               iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDTRY.

    eo_service->query_agent( EXPORTING it_agent_name = VALUE #( ( sign   = zpru_if_agent_frw=>cs_sign-include
                                                                  option = zpru_if_agent_frw=>cs_option-equal
                                                                  low    = iv_agent_name ) )
                             IMPORTING et_agent_k    = et_agent_k ).

    eo_service->read_agent( EXPORTING it_agent_read_k = VALUE #( FOR <ls_k> IN et_agent_k
                                                                 ( agent_uuid                     = <ls_k>-agent_uuid
                                                                   control-agent_uuid             = abap_true
                                                                   control-agent_name             = abap_true
                                                                   control-agent_type             = abap_true
                                                                   control-decision_provider      = abap_true
                                                                   control-short_memory_provider  = abap_true
                                                                   control-long_memory_provider   = abap_true
                                                                   control-agent_info_provider    = abap_true
                                                                   control-system_prompt_provider = abap_true
                                                                   control-status                 = abap_true
                                                                   control-created_by             = abap_true
                                                                   control-created_at             = abap_true
                                                                   control-changed_by             = abap_true
                                                                   control-last_changed           = abap_true
                                                                   control-local_last_changed     = abap_true ) )
                            IMPORTING et_agent        = DATA(lt_agent)
                            CHANGING  cs_reported     = cs_reported
                                      cs_failed       = cs_failed ).

    es_agent = VALUE #( lt_agent[ 1 ] OPTIONAL ).
  ENDMETHOD.

  METHOD ensure_agent_is_active.
    IF is_agent IS INITIAL OR is_agent-status = zpru_if_adf_type_and_constant=>cs_agent_status-inactive.
      RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDIF.

    IF is_agent-status <> zpru_if_adf_type_and_constant=>cs_agent_status-new.
      RETURN.
    ENDIF.

    io_service->update_agent(
      EXPORTING it_agent_update_imp = VALUE #( ( agent_uuid = is_agent-agent_uuid
                                                 status     = zpru_if_adf_type_and_constant=>cs_agent_status-active
                                                 control    = VALUE #( status = abap_true ) ) )
      CHANGING  cs_reported         = cs_reported
                cs_failed           = cs_failed ).

    IF line_exists( cs_failed-agent[ agent_uuid = is_agent-agent_uuid ] ).
      RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDIF.
  ENDMETHOD.

  METHOD identify_available_tools.
    io_service->rba_tool( EXPORTING it_rba_tool_k = VALUE #( FOR <ls_k2> IN it_agent_k
                                                             ( agent_uuid                   = <ls_k2>-agent_uuid
                                                               control-tool_uuid            = abap_true
                                                               control-agent_uuid           = abap_true
                                                               control-tool_name            = abap_true
                                                               control-tool_provider        = abap_true
                                                               control-step_type            = abap_true
                                                               control-tool_schema_provider = abap_true
                                                               control-tool_info_provider   = abap_true
                                                               control-is_borrowed          = abap_true
                                                               control-is_transient         = abap_true    ) )
                          IMPORTING et_tool       = et_tools
                          CHANGING  cs_reported   = cs_reported
                                    cs_failed     = cs_failed ).
  ENDMETHOD.

  METHOD prepare_memory_provider.
    get_short_memory( EXPORTING iv_agent_uuid   = iv_agent_uuid
                      IMPORTING eo_short_memory = eo_short_memory
                      CHANGING  cs_reported     = cs_reported
                                cs_failed       = cs_failed ).
  ENDMETHOD.

  METHOD record_initialization_event.
    GET TIME STAMP FIELD DATA(lv_now).

    DATA(lv_count) = 1.

    DATA(lt_message) = VALUE zpru_if_short_memory_provider=>tt_message(
                                 ( message_cid  = |{ lv_now }-{ sy-uname }-INITIALIZE_{ lv_count }|
                                   stage        = 'INITIALIZE'
                                   sub_stage    = 'INITIALIZE_AGENT'
                                   namespace    = |{ sy-uname }.{ is_agent-agent_name }|
                                   user_name    = sy-uname
                                   agent_uuid   = is_agent-agent_uuid
                                   message_time = lv_now
                                   content      = |\{ "AGENT_NAME" : "{ is_agent-agent_name }", | &&
                                                  |"DECISION_PROVIDER" : "{ is_agent-decision_provider }",| &&
                                                  |"SYSTEM_PROMPT_PROVIDER" : "{ is_agent-system_prompt_provider }" \}|
                                   message_type = zpru_if_short_memory_provider=>cs_msg_type-info ) ).

    lv_count += 1.
    LOOP AT it_tools ASSIGNING FIELD-SYMBOL(<ls_tool>).
      APPEND INITIAL LINE TO lt_message ASSIGNING FIELD-SYMBOL(<ls_message>).
      <ls_message> = VALUE #( message_cid  = |{ lv_now }-{ sy-uname }-INITIALIZE_{ lv_count }|
                              stage        = 'INITIALIZE'
                              sub_stage    = 'INITIALIZE_TOOL'
                              namespace    = |{ sy-uname }.{ is_agent-agent_name }|
                              user_name    = sy-uname
                              agent_uuid   = is_agent-agent_uuid
                              message_time = lv_now
                              content      = |\{ "TOOL_NAME" : "{ <ls_tool>-tool_name }", | &&
                                             |"TOOL_PROVIDER" : "{ <ls_tool>-tool_provider }" \}|
                              message_type = zpru_if_short_memory_provider=>cs_msg_type-info ).

      lv_count += 1.

    ENDLOOP.

    io_short_memory->save_message( lt_message ).
  ENDMETHOD.

  METHOD fetch_agent_definition_by_uuid.
    TRY.
        eo_service ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_ADF_SERVICE`
                                                               iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDTRY.

    eo_service->read_agent( EXPORTING it_agent_read_k = VALUE #( ( agent_uuid                     = iv_agent_uuid
                                                                   control-agent_uuid             = abap_true
                                                                   control-agent_name             = abap_true
                                                                   control-agent_type             = abap_true
                                                                   control-decision_provider      = abap_true
                                                                   control-short_memory_provider  = abap_true
                                                                   control-long_memory_provider   = abap_true
                                                                   control-agent_info_provider    = abap_true
                                                                   control-system_prompt_provider = abap_true
                                                                   control-status                 = abap_true
                                                                   control-created_by             = abap_true
                                                                   control-created_at             = abap_true
                                                                   control-changed_by             = abap_true
                                                                   control-last_changed           = abap_true
                                                                   control-local_last_changed     = abap_true  ) )
                            IMPORTING et_agent        = DATA(lt_agent)
                            CHANGING  cs_reported     = cs_reported
                                      cs_failed       = cs_failed ).

    ASSIGN lt_agent[ 1 ] TO FIELD-SYMBOL(<ls_agent>).
    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDIF.

    es_agent = <ls_agent>.
  ENDMETHOD.

  METHOD update_query_internal_state.
    GET TIME STAMP FIELD DATA(lv_now).
    mv_input_query = |\{ "USER": "{ sy-uname }", "TOPIC" : "QUERY", "TIMESTAMP" : "{ lv_now }", "CONTENT" : "{ iv_input_query }"  \}|.
    CLEAR: mv_output_response,
           mv_output_response_prev.
  ENDMETHOD.

  METHOD append_query_to_controller.
    " TODO: parameter IV_INPUT_QUERY is never used (ABAP cleaner)

    DATA(lo_controller) = get_controller( ).
    DATA(lv_last_number) = lines( lo_controller->mt_input_output ).
    APPEND INITIAL LINE TO lo_controller->mt_input_output ASSIGNING FIELD-SYMBOL(<ls_input_output>).
    <ls_input_output>-number      = lv_last_number + 1.
    <ls_input_output>-input_query = mv_input_query.
  ENDMETHOD.

  METHOD record_query_event.
    DATA lt_message TYPE zpru_if_short_memory_provider=>tt_message.

    GET TIME STAMP FIELD DATA(lv_now).

    lt_message = VALUE #( ( message_cid  = |{ lv_now }-{ sy-uname }-SET_INPUT_QUERY_{ 1 }|
                            stage        = 'SET_INPUT_QUERY'
                            sub_stage    = 'SET_INPUT_QUERY'
                            namespace    = |{ sy-uname }.{ is_agent-agent_name }|
                            user_name    = sy-uname
                            agent_uuid   = is_agent-agent_uuid
                            message_time = lv_now
                            content      = |\{ "AGENT_NAME" : "{ is_agent-agent_name }", | &&
                                           |"DECISION_PROVIDER" : "{ is_agent-decision_provider }",| &&
                                           |"SYSTEM_PROMPT_PROVIDER" : "{ is_agent-system_prompt_provider }", | &&
                                           |"INPUT_QUERY" : { mv_input_query } \}|
                            message_type = zpru_if_short_memory_provider=>cs_msg_type-query ) ).

    io_short_memory->save_message( lt_message ).
  ENDMETHOD.
ENDCLASS.
