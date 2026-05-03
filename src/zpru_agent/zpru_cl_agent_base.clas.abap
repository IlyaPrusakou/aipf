CLASS zpru_cl_agent_base DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zpru_if_agent_base.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zpru_cl_agent_base IMPLEMENTATION.
  METHOD zpru_if_agent_base~get_response_content.
    DATA lo_util           TYPE REF TO zpru_if_agent_util.
    DATA ls_final_response TYPE zpru_s_final_response.
    DATA lo_api_agent      TYPE REF TO zpru_if_api_agent.

    lo_api_agent = zpru_cl_agent_service_mngr=>get_agent_api( ).

    lo_util = lo_api_agent->get_utility( ).

    lo_util->convert_to_abap( EXPORTING ir_string = REF #( iv_final_response )
                              CHANGING  cr_abap   = ls_final_response ).

    lo_util->convert_to_abap( EXPORTING ir_string = REF #( ls_final_response-finalresponsebody-responsecontent )
                              CHANGING  cr_abap   = ed_response_body ).
  ENDMETHOD.

  METHOD zpru_if_agent_base~discover_agent_definition.
    DATA lo_api_agent TYPE REF TO zpru_if_api_agent.

    CLEAR: et_agent_info,
           et_agent_k,
           et_agent_tool_info,
           et_tool_agent_link.

    IF it_agentname IS INITIAL.
      RETURN.
    ENDIF.

    lo_api_agent = zpru_cl_agent_service_mngr=>get_agent_api( ).

    lo_api_agent->read_agent_definition( EXPORTING it_agent_name      = VALUE #( FOR <lv_ag> IN it_agentname
                                                                                 ( sign   = `I`
                                                                                   option = `EQ`
                                                                                   low    = <lv_ag> ) )

                                         IMPORTING et_agent_k         = et_agent_k
                                                   et_tool_agent_link = et_tool_agent_link ).

    lo_api_agent->get_agent_metadata( EXPORTING it_agent_uuid = VALUE #( FOR <lv_ag_k> IN et_agent_k
                                                                         (  <lv_ag_k>-agentuuid ) )
                                      IMPORTING et_agent_info = et_agent_info ).

    lo_api_agent->get_agent_tools_metadata( EXPORTING it_agent_uuid      = VALUE #( FOR <lv_ag_k> IN et_agent_k
                                                                                    (  <lv_ag_k>-agentuuid ) )
                                            IMPORTING et_agent_tool_info = et_agent_tool_info ).
  ENDMETHOD.
ENDCLASS.
