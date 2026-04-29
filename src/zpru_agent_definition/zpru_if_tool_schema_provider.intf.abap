INTERFACE zpru_if_tool_schema_provider
  PUBLIC.

  METHODS input_rtts_schema
    IMPORTING is_tool_master_data        TYPE zpru_if_adf_type_and_constant=>ts_agent_tool
              is_execution_step          TYPE zpru_if_axc_type_and_constant=>ts_axc_step OPTIONAL
    RETURNING VALUE(ro_structure_schema) TYPE REF TO  cl_abap_structdescr
    RAISING   zpru_cx_agent_core.

  METHODS input_json_schema
    IMPORTING is_tool_master_data TYPE zpru_if_adf_type_and_constant=>ts_agent_tool
              is_execution_step   TYPE zpru_if_axc_type_and_constant=>ts_axc_step OPTIONAL
    EXPORTING eV_json_schema      TYPE zpru_if_agent_frw=>ts_json
              eS_json_structure   TYPE zpru_s_json_schema
    RAISING   zpru_cx_agent_core.

  METHODS output_rtts_schema
    IMPORTING is_tool_master_data        TYPE zpru_if_adf_type_and_constant=>ts_agent_tool
              is_execution_step          TYPE zpru_if_axc_type_and_constant=>ts_axc_step OPTIONAL
    RETURNING VALUE(ro_structure_schema) TYPE REF TO  cl_abap_structdescr
    RAISING   zpru_cx_agent_core.

  METHODS output_json_schema
    IMPORTING is_tool_master_data TYPE zpru_if_adf_type_and_constant=>ts_agent_tool
              is_execution_step   TYPE zpru_if_axc_type_and_constant=>ts_axc_step OPTIONAL
    EXPORTING eV_json_schema      TYPE zpru_if_agent_frw=>ts_json
              eS_json_structure   TYPE zpru_s_json_schema
    RAISING   zpru_cx_agent_core.

ENDINTERFACE.
