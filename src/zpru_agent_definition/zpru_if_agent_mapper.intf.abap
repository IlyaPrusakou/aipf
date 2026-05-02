INTERFACE zpru_if_agent_mapper
  PUBLIC .
  INTERFACES zpru_if_agent_frw .

    METHODS map_tools_parameter
      IMPORTING io_request                   TYPE REF TO zpru_if_payload
                iv_input_string              TYPE string
                is_curr_tool_master_data     TYPE zpru_if_adf_type_and_constant=>ts_agent_tool OPTIONAL
                is_curr_execution_step       TYPE zpru_if_axc_type_and_constant=>ts_axc_step   OPTIONAL
                is_prev_tool_master_data     TYPE zpru_if_adf_type_and_constant=>ts_agent_tool OPTIONAL
                is_prev_execution_step       TYPE zpru_if_axc_type_and_constant=>ts_axc_step   OPTIONAL
                io_controller                TYPE REF TO zpru_if_agent_controller
                io_util                      TYPE REF TO zpru_if_agent_util
                io_curr_tool_schema_provider TYPE REF TO zpru_if_tool_schema_provider
                it_key_value_pair            TYPE  zpru_tt_key_value
      EXPORTING ev_error_flag   type abap_boolean
      CHANGING  cr_input                     TYPE REF TO data
      RAISING   zpru_cx_agent_core.

ENDINTERFACE.
