INTERFACE zpru_if_tool_info_provider
  PUBLIC .

  CONSTANTS: BEGIN OF cs_param_kind,
               exporting TYPE zpru_de_paramkind VALUE 'E',
               importing TYPE zpru_de_paramkind VALUE 'I',
               changing  TYPE zpru_de_paramkind VALUE 'C',
               receiving TYPE zpru_de_paramkind VALUE 'R',
             END OF cs_param_kind.

  METHODS get_tool_info
    IMPORTING is_tool_master_data TYPE zpru_if_adf_type_and_constant=>ts_agent_tool
              is_execution_step   TYPE zpru_if_axc_type_and_constant=>ts_axc_step OPTIONAL
    RETURNING VALUE(rv_tool_info) TYPE zpru_if_agent_frw=>ts_json
    RAISING   zpru_cx_agent_core.

  METHODS get_abap_tool_info
    IMPORTING is_tool_master_data      TYPE zpru_if_adf_type_and_constant=>ts_agent_tool
              is_execution_step        TYPE zpru_if_axc_type_and_constant=>ts_axc_step OPTIONAL
    RETURNING VALUE(rs_abap_tool_info) TYPE zpru_s_tool_info
    RAISING   zpru_cx_agent_core.


ENDINTERFACE.
