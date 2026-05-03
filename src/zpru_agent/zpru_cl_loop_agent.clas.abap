CLASS zpru_cl_loop_agent DEFINITION
  PUBLIC
  INHERITING FROM zpru_cl_agent_base FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zpru_if_loop_agent.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zpru_cl_loop_agent IMPLEMENTATION.
  METHOD zpru_if_loop_agent~execute_loop.
    DATA lo_api_agent      TYPE REF TO zpru_if_api_agent.
    DATA lv_final_response TYPE string.
    DATA ls_final_response TYPE zpru_s_final_response.
    DATA lv_is_finish_loop TYPE abap_boolean.
    DATA ls_prompt         TYPE zpru_s_prompt.

    CLEAR: ev_built_query_uuid,
           ev_built_run_uuid,
           ev_final_response,
           eo_executed_controller.

    IF    iv_agent_name IS INITIAL
       OR is_prompt     IS INITIAL.
      RETURN.
    ENDIF.

    lo_api_agent = zpru_cl_agent_service_mngr=>get_agent_api( ).

    lo_api_agent->set_rap_context_flag( iv_is_rap_context = iv_is_rap_context ).
    lo_api_agent->set_loop_execution( iv_is_loop_execution = abap_true ).

    lo_api_agent->setup_agent( EXPORTING iv_agent_name        = iv_agent_name
                                         io_parent_controller = io_parent_controller
                               IMPORTING es_agent             = DATA(ls_agent) ).

    ls_prompt = is_prompt.

    lo_api_agent->set_input_query( is_input_query = is_prompt
                                   iv_agent_uuid  = ls_agent-agentuuid ).

    lo_api_agent->build_execution( EXPORTING iv_agent_uuid       = ls_agent-agentuuid
                                   IMPORTING ev_built_run_uuid   = DATA(lv_built_run_uuid)
                                             ev_built_query_uuid = DATA(lv_built_query_uuid)  ).

    ev_built_run_uuid = lv_built_run_uuid.
*    ev_built_query_uuid = lv_built_query_uuid.

    lo_api_agent->run( EXPORTING iv_run_uuid            = lv_built_run_uuid
                                 iv_query_uuid          = lv_built_query_uuid
                       IMPORTING eo_final_response      = DATA(lo_final_response)
                                 eo_executed_controller = eo_executed_controller ).

    DATA(lo_util) = lo_api_agent->get_utility( ).

    lv_final_response = lo_final_response->get_data( )->*.

    lo_util->convert_to_abap( EXPORTING ir_string = REF #( lv_final_response )
                              CHANGING  cr_abap   = ls_final_response ).

    lv_is_finish_loop = ls_final_response-loopagentresponse-isfinishloop.

    DATA(lv_count) = 0.

    WHILE lv_is_finish_loop = abap_false.

      lv_count += 1.

      CLEAR ls_prompt.

      ls_prompt-string_content = ls_final_response-finalresponsebody-responsecontent.
      ls_prompt-type           = ls_final_response-finalresponsebody-type.

      lo_api_agent->add_query_2_run( EXPORTING iv_run_uuid    = lv_built_run_uuid
                                               is_input_query = ls_prompt
                                     IMPORTING ev_run_uuid    = DATA(lv_run_uuid)
                                               ev_query_uuid  = DATA(lv_query_uuid) ).

      lo_api_agent->run( EXPORTING iv_run_uuid            = lv_run_uuid
                                   iv_query_uuid          = lv_query_uuid
                         IMPORTING eo_final_response      = lo_final_response
                                   eo_executed_controller = eo_executed_controller ).

      lv_final_response = lo_final_response->get_data( )->*.

      lo_util->convert_to_abap( EXPORTING ir_string = REF #( lv_final_response )
                                CHANGING  cr_abap   = ls_final_response ).

      lv_is_finish_loop = ls_final_response-loopagentresponse-isfinishloop.

      IF lv_count = 10.
        lv_is_finish_loop = abap_true.
      ENDIF.

    ENDWHILE.

    ev_final_response = lo_final_response->get_data( )->*.

    IF iv_complete_run = abap_true.
      lo_api_agent->complete_run( iv_run_uuid = ev_built_run_uuid ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
