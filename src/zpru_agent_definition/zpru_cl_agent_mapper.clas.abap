CLASS zpru_cl_agent_mapper DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zpru_if_agent_frw.
    INTERFACES zpru_if_agent_mapper.

  PROTECTED SECTION.
    METHODS map_prev_out_2_next_in
      IMPORTING io_request                   TYPE REF TO zpru_if_payload
                iv_input_string              TYPE string
                is_curr_tool_master_data     TYPE zpru_if_adf_type_and_constant=>ts_agent_tool OPTIONAL
                is_curr_execution_step       TYPE zpru_if_axc_type_and_constant=>ts_axc_step   OPTIONAL
                is_prev_tool_master_data     TYPE zpru_if_adf_type_and_constant=>ts_agent_tool OPTIONAL
                is_prev_execution_step       TYPE zpru_if_axc_type_and_constant=>ts_axc_step   OPTIONAL
                io_controller                TYPE REF TO zpru_if_agent_controller
                io_util                      TYPE REF TO zpru_if_agent_util
                io_curr_tool_schema_provider TYPE REF TO zpru_if_tool_schema_provider
                it_key_value_pair            TYPE  zpru_tt_key_value
      CHANGING  cr_input                     TYPE REF TO data
      RAISING   zpru_cx_agent_core.

    METHODS traverse_tree_abap
      IMPORTING io_request                   TYPE REF TO zpru_if_payload
                iv_input_string              TYPE string
                is_curr_tool_master_data     TYPE zpru_if_adf_type_and_constant=>ts_agent_tool OPTIONAL
                is_curr_execution_step       TYPE zpru_if_axc_type_and_constant=>ts_axc_step   OPTIONAL
                is_prev_tool_master_data     TYPE zpru_if_adf_type_and_constant=>ts_agent_tool OPTIONAL
                is_prev_execution_step       TYPE zpru_if_axc_type_and_constant=>ts_axc_step   OPTIONAL
                io_controller                TYPE REF TO zpru_if_agent_controller
                io_util                      TYPE REF TO zpru_if_agent_util
                io_curr_tool_schema_provider TYPE REF TO zpru_if_tool_schema_provider
                it_key_value_pair            TYPE  zpru_tt_key_value
                io_abap_struct               TYPE REF TO cl_abap_datadescr
      CHANGING  cr_input                     TYPE data.

    METHODS get_component_mapping
      IMPORTING iv_struct_name           TYPE string
                is_curr_tool_master_data TYPE zpru_if_adf_type_and_constant=>ts_agent_tool OPTIONAL
                is_curr_execution_step   TYPE zpru_if_axc_type_and_constant=>ts_axc_step   OPTIONAL
                is_prev_tool_master_data TYPE zpru_if_adf_type_and_constant=>ts_agent_tool OPTIONAL
                is_prev_execution_step   TYPE zpru_if_axc_type_and_constant=>ts_axc_step   OPTIONAL
      EXPORTING ev_context_name          TYPE string.

  PRIVATE SECTION.
ENDCLASS.


CLASS zpru_cl_agent_mapper IMPLEMENTATION.
  METHOD zpru_if_agent_mapper~map_tools_parameter.
    DATA lv_input_string  TYPE string.
    DATA lv_prev_sequence TYPE zpru_de_step_sequence.

    lv_input_string = io_request->get_data( )->*.

    LOOP AT io_controller->mt_execution_steps ASSIGNING FIELD-SYMBOL(<ls_search_min_seq>) USING KEY sequence.
      DATA(lv_min_seq) = <ls_search_min_seq>-stepsequence.
      EXIT.
    ENDLOOP.

    IF is_curr_execution_step-stepsequence > lv_min_seq.

      " NOT FIRST TOOL INPUT --- ALWAYS ZPRU_TT_KEY_VALUE
      lv_prev_sequence = is_curr_execution_step-stepsequence - 1.
      ASSIGN io_controller->mt_execution_steps[ KEY sequence
                                                COMPONENTS stepsequence = lv_prev_sequence ] TO FIELD-SYMBOL(<ls_prev_step>).
      IF sy-subrc <> 0.
        ev_error_flag = abap_true.
        RETURN.
      ENDIF.

      ASSIGN io_controller->mt_run_context[ execution_step-stepuuid = <ls_prev_step>-stepuuid ] TO FIELD-SYMBOL(<ls_prev_tool>).
      IF sy-subrc <> 0.
        ev_error_flag = abap_true.
        RETURN.
      ENDIF.

      SORT io_controller->mt_input_output BY number DESCENDING.

      map_prev_out_2_next_in(
        EXPORTING
          io_request                   = io_request
          iv_input_string              = lv_input_string
          is_curr_tool_master_data     = is_curr_tool_master_data
          is_curr_execution_step       = is_curr_execution_step
          is_prev_tool_master_data     = <ls_prev_tool>-tool_master_data
          is_prev_execution_step       = <ls_prev_step>
          io_controller                = io_controller
          io_util                      = io_util
          io_curr_tool_schema_provider = io_curr_tool_schema_provider
          it_key_value_pair            = VALUE #( io_controller->mt_input_output[
                                                      lines( io_controller->mt_input_output ) ]-key_value_pairs OPTIONAL )
        CHANGING
          cr_input                     = cr_input  ).

    ELSE.

      " FIRST TOOL INPUT --- UNIQUE TOOL PROVIDER STRUCTURE
      io_util->convert_to_abap( EXPORTING ir_string = REF #( lv_input_string )
                                CHANGING  cr_abap   = cr_input ).

    ENDIF.
  ENDMETHOD.

  METHOD map_prev_out_2_next_in.
    DATA(lo_input_structure) = io_curr_tool_schema_provider->input_rtts_schema(
                                   is_tool_master_data = is_curr_tool_master_data
                                   is_execution_step   = is_curr_execution_step ).

    ASSIGN cr_input->* TO FIELD-SYMBOL(<ls_structure>).
    IF sy-subrc <> 0.
      RETURN. " error
    ENDIF.

    traverse_tree_abap( EXPORTING io_request                   = io_request
                                  iv_input_string              = iv_input_string
                                  is_curr_tool_master_data     = is_curr_tool_master_data
                                  is_curr_execution_step       = is_curr_execution_step
                                  is_prev_tool_master_data     = is_prev_tool_master_data
                                  is_prev_execution_step       = is_prev_execution_step
                                  io_controller                = io_controller
                                  io_util                      = io_util
                                  io_curr_tool_schema_provider = io_curr_tool_schema_provider
                                  it_key_value_pair            = it_key_value_pair
                                  io_abap_struct               = lo_input_structure
                        CHANGING  cr_input                     = <ls_structure> ).
  ENDMETHOD.

  METHOD traverse_tree_abap.
    " TODO: parameter IO_REQUEST is never used (ABAP cleaner)
    " TODO: parameter IO_CONTROLLER is never used (ABAP cleaner)
    " TODO: parameter IO_CURR_TOOL_SCHEMA_PROVIDER is never used (ABAP cleaner)

    " WORKS ONLY WITH FLAT STRUCTURES

    DATA lo_structure           TYPE REF TO cl_abap_structdescr.
    DATA lt_key_value_prev_step TYPE zpru_tt_key_value.

    IF io_abap_struct IS NOT INSTANCE OF cl_abap_structdescr.
      RETURN.
    ENDIF.

    lo_structure ?= io_abap_struct.

    DATA(lt_components) = lo_structure->get_components( ).

    io_util->convert_to_abap( EXPORTING ir_string = REF #( iv_input_string )
                              CHANGING  cr_abap   = lt_key_value_prev_step ).

    DATA(lt_key_value_all_fields) = it_key_value_pair.

    LOOP AT lt_components ASSIGNING FIELD-SYMBOL(<ls_component>).
      get_component_mapping( EXPORTING iv_struct_name           = <ls_component>-name
                                       is_curr_tool_master_data = is_curr_tool_master_data
                                       is_curr_execution_step   = is_curr_execution_step
                                       is_prev_tool_master_data = is_prev_tool_master_data
                                       is_prev_execution_step   = is_prev_execution_step
                             IMPORTING ev_context_name          = DATA(lv_name) ).

      IF lv_name IS INITIAL.
        CONTINUE.
      ENDIF.

      DATA(lv_max_prev) = 0.
      LOOP AT lt_key_value_prev_step ASSIGNING FIELD-SYMBOL(<ls_group_prev>)
           GROUP BY ( name = <ls_group_prev>-name
                      type = <ls_group_prev>-type ) ASSIGNING FIELD-SYMBOL(<ls_prev_k>).

        IF <ls_prev_k>-name <> lv_name.
          CONTINUE.
        ENDIF.

        LOOP AT GROUP <ls_prev_k> ASSIGNING FIELD-SYMBOL(<ls_member_prev>).
          IF lv_max_prev < <ls_member_prev>-counter.
            lv_max_prev = <ls_member_prev>-counter. " usually stay as zero
          ENDIF.
        ENDLOOP.
      ENDLOOP.

      ASSIGN lt_key_value_prev_step[ KEY counter
                                     COMPONENTS name    = lv_name
                                                type    = <ls_component>-type->absolute_name
                                                counter = lv_max_prev ] TO FIELD-SYMBOL(<ls_source>).
      IF sy-subrc <> 0.

        DATA(lv_max_all) = 0.
        LOOP AT lt_key_value_all_fields ASSIGNING FIELD-SYMBOL(<ls_group_all>)
             GROUP BY ( name = <ls_group_all>-name
                        type = <ls_group_all>-type ) ASSIGNING FIELD-SYMBOL(<ls_all_k>).

          IF <ls_all_k>-name <> lv_name.
            CONTINUE.
          ENDIF.

          LOOP AT GROUP <ls_all_k> ASSIGNING FIELD-SYMBOL(<ls_member_all>).
            IF lv_max_all < <ls_member_all>-counter.
              lv_max_all = <ls_member_all>-counter.
            ENDIF.
          ENDLOOP.
        ENDLOOP.

        ASSIGN lt_key_value_all_fields[ KEY counter
                                        COMPONENTS name    = lv_name
                                                   type    = <ls_component>-type->absolute_name
                                                   counter = lv_max_all ] TO <ls_source>.
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.
      ENDIF.

      ASSIGN COMPONENT <ls_component>-name OF STRUCTURE cr_input TO FIELD-SYMBOL(<lv_target>).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      IF    <ls_component>-type IS INSTANCE OF cl_abap_structdescr
         OR <ls_component>-type IS INSTANCE OF cl_abap_tabledescr.
        io_util->convert_to_abap( EXPORTING ir_string = REF #( <ls_source>-value )
                                  CHANGING  cr_abap   = <lv_target> ).
      ELSE.
        <lv_target> = <ls_source>-value.

      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_component_mapping.
    " TODO: parameter IS_CURR_TOOL_MASTER_DATA is never used (ABAP cleaner)
    " TODO: parameter IS_CURR_EXECUTION_STEP is never used (ABAP cleaner)
    " TODO: parameter IS_PREV_TOOL_MASTER_DATA is never used (ABAP cleaner)
    " TODO: parameter IS_PREV_EXECUTION_STEP is never used (ABAP cleaner)

    CLEAR ev_context_name.
    ev_context_name = iv_struct_name.
  ENDMETHOD.
ENDCLASS.
