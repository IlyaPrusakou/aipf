CLASS zpru_cl_abap_executor DEFINITION
  PUBLIC
  INHERITING FROM zpru_cl_tool_executor ABSTRACT
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zpru_if_abap_executor.

  PROTECTED SECTION.
    METHODS execute_code_int
      ABSTRACT
      IMPORTING io_controller           TYPE REF TO zpru_if_agent_controller
                io_input                TYPE REF TO data
                io_tool_schema_provider TYPE REF TO zpru_if_tool_schema_provider OPTIONAL
                io_tool_info_provider   TYPE REF TO zpru_if_tool_info_provider OPTIONAL
      EXPORTING eo_output               TYPE REF TO data
                ev_error_flag           TYPE abap_boolean
                et_additional_step      TYPE zpru_tt_additional_step
      RAISING   zpru_cx_agent_core.

  PRIVATE SECTION.
ENDCLASS.


CLASS zpru_cl_abap_executor IMPLEMENTATION.
  METHOD zpru_if_abap_executor~execute_code.
    DATA lo_tool_schema_provider TYPE REF TO zpru_if_tool_schema_provider.
    DATA lo_tool_info_provider   TYPE REF TO zpru_if_tool_info_provider.
    DATA lr_input                TYPE REF TO data.
    DATA lr_output               TYPE REF TO data.
    DATA lo_util                 TYPE REF TO zpru_if_agent_util.

    CLEAR: et_additional_steps,
           et_additional_tools.
    ev_error_flag = abap_false.

    preprocess_tool_execution( EXPORTING io_request              = io_request
                                         is_tool_master_data     = is_tool_master_data
                                         is_execution_step       = is_execution_step
                               IMPORTING ev_error_flag           = ev_error_flag
                                         er_output               = lr_output
                                         er_input                = lr_input
                                         eo_tool_schema_provider = lo_tool_schema_provider
                                         eo_tool_info_provider   = lo_tool_info_provider
                                         eo_structure_output     = DATA(lo_structure_output)
                                         eo_structure_input      = DATA(lo_structure_input)
                                         eo_util                 = lo_util ).

    IF ev_error_flag = abap_true.
      RETURN.
    ENDIF.

    execute_code_int( EXPORTING io_controller           = io_controller
                                io_input                = lr_input
                                io_tool_schema_provider = lo_tool_schema_provider
                                io_tool_info_provider   = lo_tool_info_provider
                      IMPORTING eo_output               = lr_output
                                ev_error_flag           = ev_error_flag
                                et_additional_step      = DATA(lt_additional_step) ).

    IF ev_error_flag = abap_true.
      RETURN.
    ENDIF.

    IF lt_additional_step IS NOT INITIAL.
      prepare_additional_steps( EXPORTING is_current_step     = is_execution_step
                                          it_step_4_validate  = lt_additional_step
                                          io_controller       = io_controller
                                IMPORTING et_additional_steps = et_additional_steps
                                          et_additional_tools = et_additional_tools ).
    ENDIF.

    postprocess_tool_execution( EXPORTING io_util                 = lo_util
                                          ir_output               = lr_output
                                          ir_input                = lr_input
                                          io_controller           = io_controller
                                          is_tool_master_data     = is_tool_master_data
                                          is_execution_step       = is_execution_step
                                          io_tool_schema_provider = lo_tool_schema_provider
                                          io_structure_output     = lo_structure_output
                                          io_structure_input      = lo_structure_input
                                          io_request              = io_request
                                IMPORTING eo_response             = eo_response
                                          ev_error_flag           = ev_error_flag ).
  ENDMETHOD.
ENDCLASS.
