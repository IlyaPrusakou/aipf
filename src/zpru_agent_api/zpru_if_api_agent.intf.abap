INTERFACE zpru_if_api_agent
  PUBLIC.

  INTERFACES zpru_if_agent_frw.

  TYPES tv_agent_name TYPE char100.

  METHODS initialize
    IMPORTING iv_agent_name TYPE zpru_if_api_agent=>tv_agent_name
    RAISING   zpru_cx_agent_core.

  METHODS set_input_query
    IMPORTING iv_input_query TYPE zpru_if_agent_frw=>ts_json
    RAISING   zpru_cx_agent_core.

  METHODS build_execution
    RAISING zpru_cx_agent_core.

  METHODS save_execution
    IMPORTING iv_do_commit TYPE abap_boolean
    RAISING   zpru_cx_agent_core.

  METHODS run
    RAISING zpru_cx_agent_core.

  METHODS rerun_execution
    RAISING zpru_cx_agent_core.

  METHODS rerun_from_step
    RAISING zpru_cx_agent_core.

ENDINTERFACE.
