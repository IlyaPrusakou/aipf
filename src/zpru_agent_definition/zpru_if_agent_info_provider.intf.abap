INTERFACE zpru_if_agent_info_provider
  PUBLIC.

  METHODS get_agent_info
    IMPORTING iv_agent_uuid        TYPE sysuuid_x16
    RETURNING VALUE(rv_agent_info) TYPE zpru_if_agent_frw=>ts_json
    RAISING   zpru_cx_agent_core.

  METHODS get_abap_agent_info
    IMPORTING iv_agent_uuid        TYPE sysuuid_x16
    RETURNING VALUE(rs_agent_info) TYPE zpru_s_agent_info
    RAISING   zpru_cx_agent_core.

ENDINTERFACE.
