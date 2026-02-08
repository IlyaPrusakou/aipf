CLASS zpru_cl_nested_agent_runner DEFINITION
  PUBLIC
INHERITING FROM zpru_cl_tool_executor ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zpru_if_nested_agent_runner .
  PROTECTED SECTION.

    METHODS run_nested_agent_int
      ABSTRACT
      IMPORTING io_controller           TYPE REF TO zpru_if_agent_controller
                io_input                TYPE REF TO data
                io_tool_schema_provider TYPE REF TO zpru_if_tool_schema_provider OPTIONAL
                io_tool_info_provider   TYPE REF TO zpru_if_tool_info_provider   OPTIONAL
      EXPORTING eo_output               TYPE REF TO data
                ev_error_flag           TYPE abap_boolean
                et_additional_step      TYPE zpru_tt_additional_step
                eo_nested_controller    TYPE REF TO zpru_if_agent_controller
      RAISING   zpru_cx_agent_core.

  PRIVATE SECTION.
ENDCLASS.



CLASS zpru_cl_nested_agent_runner IMPLEMENTATION.


  METHOD zpru_if_nested_agent_runner~run_nested_agent.
    DATA lo_tool_schema_provider TYPE REF TO zpru_if_tool_schema_provider.
    DATA lo_tool_info_provider   TYPE REF TO zpru_if_tool_info_provider.
    DATA lr_input                TYPE REF TO data.
    DATA lr_output               TYPE REF TO data.
    DATA lo_util                 TYPE REF TO zpru_if_agent_util.
    DATA lv_output_json          TYPE zpru_if_agent_frw=>ts_json.

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

    run_nested_agent_int( EXPORTING io_controller           = io_controller
                                    io_input                = lr_input
                                    io_tool_schema_provider = lo_tool_schema_provider
                                    io_tool_info_provider   = lo_tool_info_provider
                          IMPORTING eo_output               = lr_output
                                    ev_error_flag           = ev_error_flag
                                    et_additional_step      = DATA(lt_additional_step)
                                    eo_nested_controller    = DATA(lo_nested_controler) ).

    IF ev_error_flag = abap_true.
      RETURN.
    ENDIF.

    IF lo_nested_controler IS BOUND.

      ASSIGN   io_controller->mt_input_output[ current_controller->mv_query_uuid = is_execution_step-queryuuid ] TO FIELD-SYMBOL(<ls_current_input_output>).
      IF sy-subrc = 0.
        APPEND INITIAL LINE TO <ls_current_input_output>-direct_children ASSIGNING FIELD-SYMBOL(<ls_child_controller>).
        <ls_child_controller> = lo_nested_controler.
      ENDIF.
    ENDIF.


    IF lt_additional_step IS NOT INITIAL.
      prepare_additional_steps( EXPORTING is_current_step     = is_execution_step
                                          it_step_4_validate  = lt_additional_step
                                          io_controller       = io_controller
                                IMPORTING et_additional_steps = et_additional_steps
                                          et_additional_tools = et_additional_tools ).
    ENDIF.

    postprocess_tool_execution(
      EXPORTING
        io_util                 = lo_util
        ir_output               = lr_output
        ir_input                = lr_input
        io_controller           = io_controller
        is_tool_master_data     = is_tool_master_data
        is_execution_step       = is_execution_step
        io_tool_schema_provider = lo_tool_schema_provider
        io_structure_output     = lo_structure_output
        io_structure_input      = lo_structure_input
        io_request              = io_request
      IMPORTING
        eo_response             = eo_response
        ev_error_flag           = ev_error_flag ).

  ENDMETHOD.
ENDCLASS.
