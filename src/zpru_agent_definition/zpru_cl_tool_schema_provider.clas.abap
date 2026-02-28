CLASS zpru_cl_tool_schema_provider DEFINITION
  PUBLIC ABSTRACT
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zpru_if_agent_frw.
    INTERFACES zpru_if_tool_schema_provider.

  PROTECTED SECTION.
    METHODS get_input_abap_type ABSTRACT
      IMPORTING is_tool_master_data        TYPE zpru_if_adf_type_and_constant=>ts_agent_tool
                is_execution_step          TYPE zpru_if_axc_type_and_constant=>ts_axc_step OPTIONAL
      RETURNING VALUE(ro_structure_schema) TYPE REF TO cl_abap_structdescr.

    METHODS get_input_json_schema ABSTRACT
      IMPORTING is_tool_master_data   TYPE zpru_if_adf_type_and_constant=>ts_agent_tool
                is_execution_step     TYPE zpru_if_axc_type_and_constant=>ts_axc_step OPTIONAL
      RETURNING VALUE(ro_json_schema) TYPE zpru_if_agent_frw=>ts_json.

    METHODS get_output_abap_type ABSTRACT
      IMPORTING is_tool_master_data        TYPE zpru_if_adf_type_and_constant=>ts_agent_tool
                is_execution_step          TYPE zpru_if_axc_type_and_constant=>ts_axc_step OPTIONAL
      RETURNING VALUE(ro_structure_schema) TYPE REF TO cl_abap_structdescr.

    METHODS get_output_json_schema ABSTRACT
      IMPORTING is_tool_master_data   TYPE zpru_if_adf_type_and_constant=>ts_agent_tool
                is_execution_step     TYPE zpru_if_axc_type_and_constant=>ts_axc_step OPTIONAL
      RETURNING VALUE(ro_json_schema) TYPE zpru_if_agent_frw=>ts_json.

  PRIVATE SECTION.
ENDCLASS.


CLASS zpru_cl_tool_schema_provider IMPLEMENTATION.
  METHOD zpru_if_tool_schema_provider~input_json_schema.
    ro_json_schema = get_input_json_schema( is_tool_master_data = is_tool_master_data
                                            is_execution_step   = is_execution_step ).
  ENDMETHOD.

  METHOD zpru_if_tool_schema_provider~input_rtts_schema.
    ro_structure_schema = get_input_abap_type( is_tool_master_data = is_tool_master_data
                                               is_execution_step   = is_execution_step ).
  ENDMETHOD.

  METHOD zpru_if_tool_schema_provider~output_json_schema.
    ro_json_schema = get_output_json_schema( is_tool_master_data = is_tool_master_data
                                             is_execution_step   = is_execution_step ).
  ENDMETHOD.

  METHOD zpru_if_tool_schema_provider~output_rtts_schema.
    ro_structure_schema = get_output_abap_type( is_tool_master_data = is_tool_master_data
                                                is_execution_step   = is_execution_step ).
  ENDMETHOD.
ENDCLASS.
