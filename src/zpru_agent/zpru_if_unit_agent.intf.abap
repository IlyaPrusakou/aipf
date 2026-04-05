INTERFACE zpru_if_unit_agent
  PUBLIC.
  METHODS execute_agent
    IMPORTING iv_agent_name          TYPE zpru_if_api_agent=>tv_agent_name
              is_prompt              TYPE zpru_s_prompt
              io_parent_controller   TYPE REF TO zpru_if_agent_controller OPTIONAL
              iv_is_rap_context    TYPE abap_boolean DEFAULT abap_false
    EXPORTING ev_final_response      TYPE zpru_if_agent_frw=>ts_json
              eo_executed_controller TYPE REF TO zpru_if_agent_controller
    RAISING   zpru_cx_agent_core.

  METHODS plan_execution
    IMPORTING iv_agent_name        TYPE zpru_if_api_agent=>tv_agent_name
              is_prompt            TYPE zpru_s_prompt
              io_parent_controller TYPE REF TO zpru_if_agent_controller OPTIONAL
              iv_is_rap_context    TYPE abap_boolean DEFAULT abap_false
    EXPORTING ev_built_run_uuid    TYPE sysuuid_x16
              ev_built_query_uuid  TYPE sysuuid_x16
    RAISING   zpru_cx_agent_core.

  METHODS run_execution
    IMPORTING iv_built_run_uuid      TYPE sysuuid_x16
              iv_built_query_uuid    TYPE sysuuid_x16
              iv_is_rap_context    TYPE abap_boolean DEFAULT abap_false
    EXPORTING ev_final_response      TYPE zpru_if_agent_frw=>ts_json
              eo_executed_controller TYPE REF TO zpru_if_agent_controller
    RAISING   zpru_cx_agent_core.

ENDINTERFACE.
