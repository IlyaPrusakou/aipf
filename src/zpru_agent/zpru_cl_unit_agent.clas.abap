CLASS zpru_cl_unit_agent DEFINITION
  PUBLIC
  INHERITING FROM zpru_cl_agent_base
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zpru_if_unit_agent.
ENDCLASS.


CLASS zpru_cl_unit_agent IMPLEMENTATION.
  METHOD zpru_if_unit_agent~execute_agent.
    DATA lo_api_agent TYPE REF TO zpru_if_api_agent.

    CLEAR: ev_built_query_uuid, ev_built_run_uuid, ev_final_response, eo_executed_controller.


    IF    iv_agent_name IS INITIAL
       OR is_prompt     IS INITIAL.
      RETURN.
    ENDIF.

    lo_api_agent ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_API_AGENT`
                                                             iv_context = zpru_if_agent_frw=>cs_context-standard ).

    lo_api_agent->set_rap_context_flag( iv_is_rap_context = iv_is_rap_context ).

    lo_api_agent->setup_agent( EXPORTING iv_agent_name        = iv_agent_name
                                         io_parent_controller = io_parent_controller
                               IMPORTING es_agent             = DATA(ls_agent) ).

    lo_api_agent->set_input_query( is_input_query = is_prompt
                                   iv_agent_uuid  = ls_agent-agentuuid ).

    lo_api_agent->build_execution( EXPORTING iv_agent_uuid       = ls_agent-agentuuid
                                   IMPORTING ev_built_run_uuid   = DATA(lv_built_run_uuid)
                                             ev_built_query_uuid = DATA(lv_built_query_uuid)  ).

    ev_built_run_uuid = lv_built_run_uuid.
    ev_built_query_uuid = lv_built_query_uuid.

    lo_api_agent->run( EXPORTING iv_run_uuid            = lv_built_run_uuid
                                 iv_query_uuid          = lv_built_query_uuid
                       IMPORTING eo_final_response      = DATA(lo_final_response)
                                 eo_executed_controller = eo_executed_controller ).

    ev_final_response = lo_final_response->get_data( )->*.

    IF iv_complete_run = abap_true.
      lo_api_agent->complete_run( iv_run_uuid = ev_built_run_uuid ).
    ENDIF.

  ENDMETHOD.

  METHOD zpru_if_unit_agent~plan_execution.
    DATA lo_api_agent TYPE REF TO zpru_if_api_agent.

    CLEAR: ev_built_query_uuid,
           ev_built_run_uuid,
           ev_environment_uuid.

    IF    iv_agent_name IS INITIAL
       OR is_prompt     IS INITIAL.
      RETURN.
    ENDIF.

    lo_api_agent ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_API_AGENT`
                                                             iv_context = zpru_if_agent_frw=>cs_context-standard ).

    lo_api_agent->set_rap_context_flag( iv_is_rap_context = iv_is_rap_context ).

    lo_api_agent->setup_agent( EXPORTING iv_agent_name        = iv_agent_name
                                         io_parent_controller = io_parent_controller
                               IMPORTING es_agent             = DATA(ls_agent) ).

    lo_api_agent->set_input_query( is_input_query = is_prompt
                                   iv_agent_uuid  = ls_agent-agentuuid ).

    lo_api_agent->build_execution( EXPORTING iv_agent_uuid       = ls_agent-agentuuid
                                   IMPORTING ev_built_run_uuid   = ev_built_run_uuid
                                             ev_built_query_uuid = ev_built_query_uuid  ).

    lo_api_agent->post_environment( EXPORTING iv_agent_uuid       = ls_agent-agentuuid
                                    iv_built_run_uuid   = ev_built_run_uuid
                                    iv_built_query_uuid = ev_built_query_uuid
                                    IMPORTING
                                    ev_environment_uuid = ev_environment_uuid ).
  ENDMETHOD.

  METHOD zpru_if_unit_agent~run_execution.
    DATA lo_api_agent_creator TYPE REF TO zpru_if_api_agent.
    DATA lo_api_agent         TYPE REF TO zpru_if_api_agent.

    IF    iv_built_query_uuid IS INITIAL
       OR iv_built_run_uuid   IS INITIAL.
      RETURN.
    ENDIF.

    lo_api_agent_creator ?= zpru_cl_agent_service_mngr=>get_service(
                                iv_service = `ZPRU_IF_API_AGENT`
                                iv_context = zpru_if_agent_frw=>cs_context-standard ).

    lo_api_agent = lo_api_agent_creator->restore_environment( iv_built_run_uuid   = iv_built_run_uuid
                                                              iv_built_query_uuid = iv_built_query_uuid
                                                              iv_environment_uuid = iv_environment_uuid ).

    lo_api_agent->set_rap_context_flag( iv_is_rap_context = iv_is_rap_context ).

    lo_api_agent->run( EXPORTING iv_run_uuid            = iv_built_run_uuid
                                 iv_query_uuid          = iv_built_query_uuid
                       IMPORTING eo_final_response      = DATA(lo_final_response)
                                 eo_executed_controller = eo_executed_controller ).

    ev_final_response = lo_final_response->get_data( )->*.

    IF iv_complete_run = abap_true.
      lo_api_agent->complete_run( iv_run_uuid = iv_built_run_uuid ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
