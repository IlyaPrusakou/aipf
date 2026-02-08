INTERFACE zpru_if_agent_controller
  PUBLIC.

  TYPES: BEGIN OF ts_run_history,
           count      TYPE i,
           run_uuid   TYPE sysuuid_x16,
           query_uuid TYPE sysuuid_x16,
         END OF ts_run_history.

  TYPES tt_run_history TYPE STANDARD TABLE OF ts_run_history WITH EMPTY KEY.

  TYPES: BEGIN OF ts_run_context,
           tool_master_data   TYPE zpru_if_adf_type_and_constant=>ts_agent_tool,
           execution_step     TYPE zpru_if_axc_type_and_constant=>ts_axc_step,
           abap_input_schema  TYPE REF TO cl_abap_structdescr,
           json_input_schema  TYPE zpru_if_agent_frw=>ts_json,
           abap_output_schema TYPE REF TO cl_abap_structdescr,
           json_output_schema TYPE zpru_if_agent_frw=>ts_json,
           abap_request       TYPE REF TO data,
           json_request       TYPE zpru_if_agent_frw=>ts_json,
           abap_response      TYPE REF TO data,
           json_response      TYPE zpru_if_agent_frw=>ts_json,
         END OF ts_run_context.

  TYPES: tt_run_context TYPE STANDARD TABLE OF ts_run_context WITH EMPTY KEY.

  TYPES: tt_controllers TYPE STANDARD TABLE OF REF TO zpru_if_agent_controller WITH EMPTY KEY.

  TYPES: BEGIN OF ts_input_output,
           number             TYPE i,
           input_prompt       TYPE zpru_s_prompt,
           input_query        TYPE zpru_if_agent_frw=>ts_json,
           execution_steps    TYPE zpru_if_axc_type_and_constant=>tt_axc_step,
           run_context        TYPE tt_run_context,
           output_response    TYPE zpru_if_agent_frw=>ts_json,
           parent_controller  TYPE REF TO zpru_if_agent_controller,
           current_controller TYPE REF TO zpru_if_agent_controller,
           direct_children    TYPE tt_controllers,
         END OF ts_input_output.

  TYPES tt_input_output TYPE STANDARD TABLE OF ts_input_output WITH EMPTY KEY.

  DATA mt_execution_steps TYPE zpru_if_axc_type_and_constant=>tt_axc_step.
  DATA mv_stop_agent        TYPE abap_boolean.
  DATA mv_agent_uuid        TYPE sysuuid_x16.
  DATA mv_run_uuid        TYPE sysuuid_x16.
  DATA mv_query_uuid      TYPE sysuuid_x16.
  DATA mt_run_context       TYPE tt_run_context.
  DATA mo_parent_controller TYPE REF TO zpru_if_agent_controller.
  DATA mo_short_memory      TYPE REF TO zpru_if_short_memory_provider.
  DATA mo_long_memory       TYPE REF TO zpru_if_long_memory_provider.
  DATA mo_api_agent               TYPE REF TO zpru_cl_api_agent.
  DATA mt_input_output      TYPE tt_input_output.
  DATA mv_max_number_of_loops TYPE i.
  DATA mv_real_number_of_loops TYPE i.
  DATA mt_run_history TYPE tt_run_history .
ENDINTERFACE.
