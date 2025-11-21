INTERFACE zpru_if_agent_controller
  PUBLIC .

  INTERFACES zpru_if_agent_frw .
  DATA mv_stop_agent TYPE abap_boolean.
  DATA mv_agent_uuid TYPE  sysuuid_x16.
ENDINTERFACE.
