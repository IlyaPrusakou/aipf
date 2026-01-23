INTERFACE zpru_if_unit_agent
  PUBLIC .
  METHODS execute_agent
    IMPORTING
      iv_agent_name     TYPE zpru_if_api_agent=>tv_agent_name
      iv_input_query    TYPE zpru_if_agent_frw=>ts_json
      io_parent_controller  type ref to zpru_if_agent_controller optional
    EXPORTING
      ev_final_response TYPE zpru_if_agent_frw=>ts_json.
ENDINTERFACE.
