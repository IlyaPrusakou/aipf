CLASS zpru_cl_transient_code DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zpru_if_tool_provider.
    INTERFACES zpru_if_tool_schema_provider.
    INTERFACES zpru_if_tool_info_provider.

  PRIVATE SECTION.

ENDCLASS.


CLASS zpru_cl_transient_code IMPLEMENTATION.
  METHOD zpru_if_tool_provider~get_tool.
    DATA lo_tool_provider TYPE REF TO zpru_if_tool_provider.

    lo_tool_provider = NEW lcl_adf_tool_provider( ).
    ro_executor = lo_tool_provider->get_tool( is_agent            = is_agent
                                              io_controller       = io_controller
                                              io_input            = io_input
                                              is_tool_master_data = is_tool_master_data
                                              is_execution_step   = is_execution_step ).
  ENDMETHOD.

  METHOD zpru_if_tool_info_provider~get_tool_info.
    DATA lo_tool_info_provider TYPE REF TO zpru_if_tool_info_provider.

    lo_tool_info_provider = NEW lcl_adf_tool_info_provider( ).
    rv_tool_info = lo_tool_info_provider->get_tool_info( is_tool_master_data = is_tool_master_data
                                                         is_execution_step   = is_execution_step ).
  ENDMETHOD.

  METHOD zpru_if_tool_info_provider~get_abap_tool_info.
    DATA lo_tool_info_provider TYPE REF TO zpru_if_tool_info_provider.

    lo_tool_info_provider = NEW lcl_adf_tool_info_provider( ).
    rs_abap_tool_info = lo_tool_info_provider->get_abap_tool_info( is_tool_master_data = is_tool_master_data
                                                                   is_execution_step   = is_execution_step ).
  ENDMETHOD.

  METHOD zpru_if_tool_schema_provider~input_json_schema.
    DATA lo_input_schema_provider TYPE REF TO zpru_if_tool_schema_provider.

    CLEAR: ev_json_schema,
           es_json_structure.

    lo_input_schema_provider = NEW lcl_adf_schema_provider( ).

    lo_input_schema_provider->input_json_schema( EXPORTING is_tool_master_data = is_tool_master_data
                                                           is_execution_step   = is_execution_step
                                                 IMPORTING ev_json_schema      = ev_json_schema
                                                           es_json_structure   = es_json_structure ).
  ENDMETHOD.

  METHOD zpru_if_tool_schema_provider~input_rtts_schema.
    DATA lo_input_schema_provider TYPE REF TO zpru_if_tool_schema_provider.

    lo_input_schema_provider = NEW lcl_adf_schema_provider( ).
    ro_structure_schema = lo_input_schema_provider->input_rtts_schema( is_tool_master_data = is_tool_master_data
                                                                       is_execution_step   = is_execution_step ).
  ENDMETHOD.

ENDCLASS.
