INTERFACE zpru_if_agent_controller
  PUBLIC.

  TYPES: BEGIN OF ts_input_output,
           number          TYPE i,
           input_query     TYPE zpru_if_agent_frw=>ts_json,
           output_response TYPE zpru_if_agent_frw=>ts_json,
         END OF ts_input_output.

  TYPES tt_input_output TYPE STANDARD TABLE OF ts_input_output WITH EMPTY KEY.

  DATA mv_stop_agent        TYPE abap_boolean.
  DATA mv_agent_uuid        TYPE sysuuid_x16.
  DATA mo_context           TYPE REF TO zpru_if_payload.
  DATA mo_parent_controller TYPE REF TO zpru_if_agent_controller.
  DATA mo_short_memory      TYPE REF TO zpru_if_short_memory_provider.
  DATA mo_long_memory       TYPE REF TO zpru_if_long_memory_provider.
  DATA mo_api_agent               TYPE REF TO zpru_cl_api_agent.
  DATA mt_input_output      TYPE tt_input_output.
ENDINTERFACE.
