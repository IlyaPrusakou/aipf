INTERFACE zpru_if_agent_base
  PUBLIC.

  INTERFACES zpru_if_agent_frw.

  METHODS get_response_content
    IMPORTING iv_final_response TYPE zpru_if_agent_frw=>ts_json
    EXPORTING ed_response_body  TYPE data
    RAISING   zpru_cx_agent_core.

  METHODS discover_agent_definition
    IMPORTING it_agentname       TYPE zpru_if_api_agent=>tt_agent_name
    EXPORTING et_agent_k         TYPE zpru_if_api_agent=>tt_agent_k
              et_tool_agent_link TYPE zpru_if_api_agent=>tt_tool_agent_link
              et_agent_info      TYPE zpru_tt_api_agent_info
              et_agent_tool_info TYPE zpru_tt_api_tool_info
    RAISING   zpru_cx_agent_core.

ENDINTERFACE.
