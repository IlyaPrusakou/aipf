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
                io_tool_info_provider   TYPE REF TO zpru_if_tool_info_provider   OPTIONAL
      EXPORTING eo_output               TYPE REF TO data
                ev_error_flag           TYPE abap_boolean
                et_additional_step      TYPE zpru_tt_additional_step.

  PRIVATE SECTION.
ENDCLASS.


CLASS zpru_cl_abap_executor IMPLEMENTATION.
  METHOD zpru_if_abap_executor~execute_code.
    DATA lo_tool_schema_provider TYPE REF TO zpru_if_tool_schema_provider.
    DATA lo_tool_info_provider   TYPE REF TO zpru_if_tool_info_provider.
    DATA lr_input                TYPE REF TO data.
    DATA lr_output               TYPE REF TO data.
    DATA lo_util                 TYPE REF TO zpru_if_agent_util.
    DATA lv_output_json          TYPE zpru_if_agent_frw=>ts_json.
    DATA lo_popping_agent TYPE REF TO zpru_if_unit_agent.

    CLEAR:       et_additional_steps,
      et_additional_tools.
    ev_error_flag = abap_false.

    CREATE OBJECT lo_tool_schema_provider TYPE (is_tool_master_data-tool_schema_provider).
    IF sy-subrc <> 0.
      " error
      RETURN.
    ENDIF.

    CREATE OBJECT lo_tool_info_provider TYPE (is_tool_master_data-tool_info_provider).
    IF sy-subrc <> 0.
      " error
      RETURN.
    ENDIF.

    TRY.
        lo_util ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_AGENT_UTIL`
                                                            iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        " error
        RETURN.
    ENDTRY.

    DATA(lo_structure_input) = lo_tool_schema_provider->input_rtts_schema( is_tool_master_data = is_tool_master_data
                                                                           is_execution_step   = is_execution_step  ).

    CREATE DATA lr_input TYPE HANDLE lo_structure_input.

    lo_util->convert_to_abap(
      EXPORTING
        ir_string = io_request->get_data( )->*
      CHANGING
        cr_abap   = lr_input->* ).

    DATA(lo_structure_output) = lo_tool_schema_provider->output_rtts_schema( is_tool_master_data = is_tool_master_data
                                                                             is_execution_step   = is_execution_step  ).

    CREATE DATA lr_output TYPE HANDLE lo_structure_output.

    execute_code_int( EXPORTING io_controller           = io_controller
                                io_input                = lr_input
                                io_tool_schema_provider = lo_tool_schema_provider
                                io_tool_info_provider   = lo_tool_info_provider
                      IMPORTING eo_output               = lr_output
                                ev_error_flag           = ev_error_flag
                                et_additional_step      = DATA(lt_additional_step) ).

    IF lt_additional_step IS NOT INITIAL.
      validate_additional_steps(
        EXPORTING
          it_step_4_validate = lt_additional_step
        IMPORTING
          et_additional_steps = et_additional_steps
          et_additional_tools = et_additional_tools ).
    ENDIF.

    lo_util->convert_to_string( EXPORTING ir_abap   = lr_output
                                CHANGING  cr_string = lv_output_json ).

    eo_response->set_data( NEW zpru_if_agent_frw=>ts_json( lv_output_json ) ).

    ASSIGN io_controller->mt_run_context[ execution_step-step_uuid = is_execution_step-step_uuid ] TO FIELD-SYMBOL(<ls_current_run_context>).
    IF sy-subrc <> 0.
      " error
      RETURN.
    ENDIF.
    <ls_current_run_context>-abap_input_schema  = lo_structure_input.
    <ls_current_run_context>-abap_output_schema = lo_structure_output.
    <ls_current_run_context>-abap_response      = lr_output.
    <ls_current_run_context>-json_response      = lv_output_json.
    <ls_current_run_context>-abap_request       = lr_input.
    <ls_current_run_context>-json_request       = io_request->get_data( )->*.
  ENDMETHOD.
ENDCLASS.
