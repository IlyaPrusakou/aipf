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
      RETURNING VALUE(ro_structure_schema) TYPE REF TO cl_abap_structdescr
      RAISING   zpru_cx_agent_core.

    METHODS get_input_json_schema ABSTRACT
      IMPORTING is_tool_master_data TYPE zpru_if_adf_type_and_constant=>ts_agent_tool
                is_execution_step   TYPE zpru_if_axc_type_and_constant=>ts_axc_step OPTIONAL
      EXPORTING ev_json_schema      TYPE zpru_if_agent_frw=>ts_json
                es_json_structure   TYPE zpru_s_json_schema
      RAISING   zpru_cx_agent_core.

    METHODS get_output_abap_type
      IMPORTING is_tool_master_data        TYPE zpru_if_adf_type_and_constant=>ts_agent_tool
                is_execution_step          TYPE zpru_if_axc_type_and_constant=>ts_axc_step OPTIONAL
      RETURNING VALUE(ro_structure_schema) TYPE REF TO cl_abap_structdescr
      RAISING   zpru_cx_agent_core.

    METHODS get_output_json_schema
      IMPORTING is_tool_master_data TYPE zpru_if_adf_type_and_constant=>ts_agent_tool
                is_execution_step   TYPE zpru_if_axc_type_and_constant=>ts_axc_step OPTIONAL
      EXPORTING ev_json_schema      TYPE zpru_if_agent_frw=>ts_json
                es_json_structure   TYPE zpru_s_json_schema
      RAISING   zpru_cx_agent_core.

  PRIVATE SECTION.
ENDCLASS.


CLASS zpru_cl_tool_schema_provider IMPLEMENTATION.
  METHOD zpru_if_tool_schema_provider~input_json_schema.
    get_input_json_schema( EXPORTING is_tool_master_data = is_tool_master_data
                                     is_execution_step   = is_execution_step
                           IMPORTING ev_json_schema      = ev_json_schema
                                     es_json_structure   = es_json_structure ).
  ENDMETHOD.

  METHOD zpru_if_tool_schema_provider~input_rtts_schema.
    ro_structure_schema = get_input_abap_type( is_tool_master_data = is_tool_master_data
                                               is_execution_step   = is_execution_step ).
  ENDMETHOD.

  METHOD zpru_if_tool_schema_provider~output_json_schema.
    get_output_json_schema( EXPORTING is_tool_master_data = is_tool_master_data
                                      is_execution_step   = is_execution_step
                            IMPORTING ev_json_schema      = ev_json_schema
                                      es_json_structure   = es_json_structure ).
  ENDMETHOD.

  METHOD zpru_if_tool_schema_provider~output_rtts_schema.
    ro_structure_schema = get_output_abap_type( is_tool_master_data = is_tool_master_data
                                                is_execution_step   = is_execution_step ).
  ENDMETHOD.

  METHOD get_output_abap_type.
    " TODO: parameter IS_TOOL_MASTER_DATA is never used (ABAP cleaner)
    " TODO: parameter IS_EXECUTION_STEP is never used (ABAP cleaner)

    ro_structure_schema ?= cl_abap_structdescr=>describe_by_name( p_name = `ZPRU_TT_KEY_VALUE` ).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
  ENDMETHOD.

  METHOD get_output_json_schema.
    " TODO: parameter IS_TOOL_MASTER_DATA is never used (ABAP cleaner)
    " TODO: parameter IS_EXECUTION_STEP is never used (ABAP cleaner)
    " TODO: parameter EV_JSON_SCHEMA is never cleared or assigned (ABAP cleaner)
    " TODO: parameter ES_JSON_STRUCTURE is never cleared or assigned (ABAP cleaner)

    RETURN.
  ENDMETHOD.
ENDCLASS.
