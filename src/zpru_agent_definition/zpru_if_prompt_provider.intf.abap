INTERFACE zpru_if_prompt_provider
  PUBLIC .

  METHODS get_system_prompt
    IMPORTING iv_agent_uuid           TYPE sysuuid_x16
    RETURNING VALUE(rv_system_prompt) TYPE zpru_if_agent_frw=>ts_json
    RAISING   zpru_cx_agent_core.

  METHODS get_abap_system_prompt
    IMPORTING iv_agent_uuid                TYPE sysuuid_x16
    RETURNING VALUE(rs_abap_system_prompt) TYPE zpru_s_system_prompt
    RAISING   zpru_cx_agent_core.

ENDINTERFACE.
