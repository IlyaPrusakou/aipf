INTERFACE zpru_if_tool_schema_provider
  PUBLIC.

  METHODS input_rtts_schema
    IMPORTING is_tool_master_data        TYPE zpru_if_adf_type_and_constant=>ts_agent_tool
              is_execution_step          TYPE zpru_if_axc_type_and_constant=>ts_axc_step
    RETURNING VALUE(ro_structure_schema) TYPE REF TO  cl_abap_structdescr.

  METHODS input_json_schema
    IMPORTING is_tool_master_data   TYPE zpru_if_adf_type_and_constant=>ts_agent_tool
              is_execution_step     TYPE zpru_if_axc_type_and_constant=>ts_axc_step
    RETURNING VALUE(ro_json_schema) TYPE REF TO zpru_if_agent_frw=>ts_json.

  METHODS output_rtts_schema
    IMPORTING is_tool_master_data        TYPE zpru_if_adf_type_and_constant=>ts_agent_tool
              is_execution_step          TYPE zpru_if_axc_type_and_constant=>ts_axc_step
    RETURNING VALUE(ro_structure_schema) TYPE REF TO  cl_abap_structdescr.

  METHODS output_json_schema
    IMPORTING is_tool_master_data   TYPE zpru_if_adf_type_and_constant=>ts_agent_tool
              is_execution_step     TYPE zpru_if_axc_type_and_constant=>ts_axc_step
    RETURNING VALUE(ro_json_schema) TYPE REF TO zpru_if_agent_frw=>ts_json.

ENDINTERFACE.
