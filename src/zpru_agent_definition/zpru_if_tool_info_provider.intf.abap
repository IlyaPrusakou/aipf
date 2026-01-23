INTERFACE zpru_if_tool_info_provider
  PUBLIC .

  METHODS get_tool_info
    IMPORTING is_tool_master_data TYPE zpru_if_adf_type_and_constant=>ts_agent_tool
              is_execution_step   TYPE zpru_if_axc_type_and_constant=>ts_axc_step
    RETURNING VALUE(rv_tool_info) TYPE zpru_if_agent_frw=>ts_json.
ENDINTERFACE.
