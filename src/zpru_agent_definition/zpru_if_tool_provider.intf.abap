INTERFACE zpru_if_tool_provider
  PUBLIC.

  METHODS get_tool
    IMPORTING is_agent            TYPE zpru_if_adf_type_and_constant=>ts_agent
              io_controller       TYPE REF TO zpru_if_agent_controller
              io_input            TYPE REF TO zpru_if_payload
              is_tool_master_data TYPE zpru_if_adf_type_and_constant=>ts_agent_tool
              is_execution_step   TYPE zpru_if_axc_type_and_constant=>ts_axc_step
    RETURNING VALUE(ro_executor)  TYPE REF TO zpru_if_tool_executor
    raISING zpru_cx_agent_core.

ENDINTERFACE.
