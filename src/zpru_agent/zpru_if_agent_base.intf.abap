INTERFACE zpru_if_agent_base
  PUBLIC.

  INTERFACES zpru_if_agent_frw.

  METHODS get_response_content
    IMPORTING iv_final_response TYPE zpru_if_agent_frw=>ts_json
    EXPORTING ed_response_body  TYPE data
    RAISING   zpru_cx_agent_core.

  METHODS discover_agent
    IMPORTING agentname TYPE zpru_if_api_agent=>tv_agent_name
    RAISING   zpru_cx_agent_core.

ENDINTERFACE.
