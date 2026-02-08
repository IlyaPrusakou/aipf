CLASS zpru_cl_tool_executor DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zpru_if_agent_frw.
    INTERFACES zpru_if_tool_executor.

  PROTECTED SECTION.
    METHODS preprocess_tool_execution
      IMPORTING io_request              TYPE REF TO zpru_if_payload
                is_tool_master_data     TYPE zpru_if_adf_type_and_constant=>ts_agent_tool OPTIONAL
                is_execution_step       TYPE zpru_if_axc_type_and_constant=>ts_axc_step OPTIONAL
      EXPORTING ev_error_flag           TYPE abap_boolean
                er_output               TYPE REF TO data
                er_input                TYPE REF TO data
                eo_tool_schema_provider TYPE REF TO zpru_if_tool_schema_provider
                eo_tool_info_provider   TYPE REF TO zpru_if_tool_info_provider
                eo_structure_output     TYPE REF TO cl_abap_structdescr
                eo_structure_input      TYPE REF TO cl_abap_structdescr
                eo_util                 TYPE REF TO zpru_if_agent_util.

    METHODS prepare_additional_steps
      IMPORTING is_current_step     TYPE zpru_if_axc_type_and_constant=>ts_axc_step
                it_step_4_validate  TYPE zpru_tt_additional_step
                io_controller       TYPE REF TO zpru_if_agent_controller
      EXPORTING et_additional_steps TYPE zpru_if_axc_type_and_constant=>tt_axc_step
                et_additional_tools TYPE zpru_if_adf_type_and_constant=>tt_agent_tool
      RAISING   zpru_cx_agent_core.

    METHODS postprocess_tool_execution
      IMPORTING io_util                 TYPE REF TO zpru_if_agent_util
                ir_output               TYPE REF TO data
                ir_input                TYPE REF TO data
                io_controller           TYPE REF TO zpru_if_agent_controller
                is_tool_master_data     TYPE zpru_if_adf_type_and_constant=>ts_agent_tool OPTIONAL
                is_execution_step       TYPE zpru_if_axc_type_and_constant=>ts_axc_step OPTIONAL
                io_tool_schema_provider TYPE REF TO zpru_if_tool_schema_provider
                io_structure_output     TYPE REF TO cl_abap_structdescr
                io_structure_input      TYPE REF TO cl_abap_structdescr
                io_request              TYPE REF TO zpru_if_payload
      EXPORTING eo_response             TYPE REF TO zpru_if_payload
                ev_error_flag           TYPE abap_boolean.

  PRIVATE SECTION.
ENDCLASS.


CLASS zpru_cl_tool_executor IMPLEMENTATION.
  METHOD prepare_additional_steps.
    DATA lo_axc_service           TYPE REF TO zpru_if_axc_service.
    DATA lo_adf_service           TYPE REF TO zpru_if_adf_service.
    DATA lo_adf_validator         TYPE REF TO zpru_if_adf_validator.
    DATA lt_message_in            TYPE zpru_if_short_memory_provider=>tt_message.
    DATA lv_wrong_step_type       TYPE abap_boolean.
    DATA lv_wrong_tool_provider   TYPE abap_boolean.
    DATA lv_wrong_schema_provider TYPE abap_boolean.
    DATA lv_wrong_info_provider   TYPE abap_boolean.
    DATA lv_wrong_agent_tool_comb TYPE abap_boolean.
    DATA ls_last_step             TYPE zpru_s_additional_step.

    CLEAR: et_additional_steps,
           et_additional_tools.

    IF    is_current_step    IS INITIAL
       OR it_step_4_validate IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        lo_axc_service ?= zpru_cl_agent_service_mngr=>get_service(
                              iv_service = `ZPRU_IF_AXC_SERVICE`
                              iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    TRY.
        lo_adf_service ?= zpru_cl_agent_service_mngr=>get_service(
                              iv_service = `ZPRU_IF_ADF_SERVICE`
                              iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    TRY.
        lo_adf_validator ?= zpru_cl_agent_service_mngr=>get_service(
                                iv_service = `ZPRU_IF_ADF_VALIDATOR`
                                iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    lo_adf_service->read_agent( EXPORTING it_agent_read_k = VALUE #( FOR <ls_a> IN it_step_4_validate
                                                                     ( agentuuid         = <ls_a>-agentuuid
                                                                       control-agentuuid = abap_true ) )
                                IMPORTING et_agent        = DATA(lt_existing_agent) ).

    lo_adf_service->read_agent( EXPORTING it_agent_read_k = VALUE #( ( agentuuid         = io_controller->mv_agent_uuid
                                                                       control-agentname = abap_true  ) )
                                IMPORTING et_agent        = DATA(lt_current_agent) ).

    DATA(ls_current_agent) = VALUE #( lt_current_agent[ 1 ] OPTIONAL ).

    lo_adf_service->read_tool( EXPORTING it_tool_read_k = VALUE #( FOR <ls_a2> IN it_step_4_validate
                                                                   ( agentuuid                  = <ls_a2>-agentuuid
                                                                     tooluuid                   = <ls_a2>-tooluuid
                                                                     control-agentuuid          = abap_true
                                                                     control-tooluuid           = abap_true
                                                                     control-toolname           = abap_true
                                                                     control-toolprovider       = abap_true
                                                                     control-steptype           = abap_true
                                                                     control-toolschemaprovider = abap_true
                                                                     control-toolinfoprovider   = abap_true
                                                                     control-isborrowed         = abap_true
                                                                     control-istransient        = abap_true  ) )
                               IMPORTING et_tool        = DATA(lt_existing_tool) ).

    lo_axc_service->read_header( EXPORTING it_head_read_k = VALUE #( ( runuuid       = is_current_step-runuuid
                                                                       control-runid = abap_true ) )
                                 IMPORTING et_axc_head    = DATA(lt_current_run) ).

    DATA(ls_current_run) = VALUE #( lt_current_run[ 1 ] OPTIONAL ).

    lo_axc_service->read_query( EXPORTING it_query_read_k = VALUE #( ( queryuuid           = is_current_step-queryuuid
                                                                       control-querynumber = abap_true ) )
                                IMPORTING et_axc_query    = DATA(lt_current_query) ).

    DATA(ls_current_query) = VALUE #( lt_current_query[ 1 ] OPTIONAL ).

    DATA(lo_elem) = CAST cl_abap_elemdescr( cl_abap_typedescr=>describe_by_name( 'ZPRU_DE_ADF_STEP_TYPE' ) ).
    DATA(lt_fixed_values) = lo_elem->get_ddic_fixed_values( ).

    lv_wrong_step_type = abap_false.
    lv_wrong_tool_provider = abap_false.
    lv_wrong_schema_provider = abap_false.
    lv_wrong_info_provider = abap_false.
    lv_wrong_agent_tool_comb = abap_false.

    LOOP AT it_step_4_validate ASSIGNING FIELD-SYMBOL(<ls_step_4_validate>).

      IF line_exists( lt_fixed_values[ low = <ls_step_4_validate>-steptype ] ).
        lv_wrong_step_type = abap_true.
        ls_last_step = <ls_step_4_validate>.
        EXIT.
      ENDIF.

      lo_adf_validator->validate_provider_cls( EXPORTING iv_class            = <ls_step_4_validate>-toolprovider
                                                         iv_intf_2_be_impl_1 = 'ZPRU_IF_TOOL_PROVIDER'
                                               IMPORTING ev_type_not_exist   = DATA(lv_type_not_exist)
                                                         ev_type_not_class   = DATA(lv_type_not_class)
                                                         ev_intf_not_impl_1  = DATA(lv_intf_not_impl_1) ).

      IF    lv_type_not_exist  = abap_true
         OR lv_type_not_class  = abap_true
         OR lv_intf_not_impl_1 = abap_true.
        ls_last_step = <ls_step_4_validate>.
        lv_wrong_tool_provider = abap_true.
        EXIT.
      ENDIF.

      lo_adf_validator->validate_provider_cls( EXPORTING iv_class            = <ls_step_4_validate>-toolschemaprovider
                                                         iv_intf_2_be_impl_1 = 'ZPRU_IF_TOOL_SCHEMA_PROVIDER'
                                               IMPORTING ev_type_not_exist   = lv_type_not_exist
                                                         ev_type_not_class   = lv_type_not_class
                                                         ev_intf_not_impl_1  = lv_intf_not_impl_1 ).

      IF    lv_type_not_exist  = abap_true
         OR lv_type_not_class  = abap_true
         OR lv_intf_not_impl_1 = abap_true.
        ls_last_step = <ls_step_4_validate>.
        lv_wrong_schema_provider = abap_true.
        EXIT.
      ENDIF.

      lo_adf_validator->validate_provider_cls( EXPORTING iv_class            = <ls_step_4_validate>-toolinfoprovider
                                                         iv_intf_2_be_impl_1 = 'ZPRU_IF_TOOL_INFO_PROVIDER'
                                               IMPORTING ev_type_not_exist   = lv_type_not_exist
                                                         ev_type_not_class   = lv_type_not_class
                                                         ev_intf_not_impl_1  = lv_intf_not_impl_1 ).

      IF    lv_type_not_exist  = abap_true
         OR lv_type_not_class  = abap_true
         OR lv_intf_not_impl_1 = abap_true.
        ls_last_step = <ls_step_4_validate>.
        lv_wrong_info_provider = abap_true.
        EXIT.
      ENDIF.

      TRY.
          ASSIGN lt_existing_agent[ agentuuid = <ls_step_4_validate>-agentuuid ] TO FIELD-SYMBOL(<ls_existing_agent>).
          ASSIGN lt_existing_tool[ tooluuid = <ls_step_4_validate>-tooluuid ] TO FIELD-SYMBOL(<ls_existing_tool>).

          IF    (     <ls_existing_agent> IS ASSIGNED
                  AND <ls_existing_tool>  IS NOT ASSIGNED )
             OR (     <ls_existing_agent> IS NOT ASSIGNED
                  AND <ls_existing_tool>  IS ASSIGNED ).
            ls_last_step = <ls_step_4_validate>.
            lv_wrong_agent_tool_comb = abap_true.
            EXIT.
          ENDIF.

          APPEND INITIAL LINE TO et_additional_tools ASSIGNING FIELD-SYMBOL(<ls_additional_tool>).

          IF <ls_existing_tool> IS ASSIGNED.
            <ls_additional_tool>-tooluuid           = <ls_existing_tool>-tooluuid.
            <ls_additional_tool>-toolname           = <ls_existing_tool>-toolname.
            <ls_additional_tool>-toolprovider       = <ls_existing_tool>-toolprovider.
            <ls_additional_tool>-steptype           = <ls_existing_tool>-steptype.
            <ls_additional_tool>-toolschemaprovider = <ls_existing_tool>-toolschemaprovider.
            <ls_additional_tool>-toolinfoprovider   = <ls_existing_tool>-toolinfoprovider.
          ELSE.
            DATA(lv_temp_tool_uuid) = cl_system_uuid=>create_uuid_x16_static( ).
            <ls_additional_tool>-tooluuid           = lv_temp_tool_uuid.
            <ls_additional_tool>-toolname           = <ls_step_4_validate>-toolname.
            <ls_additional_tool>-toolprovider       = <ls_step_4_validate>-toolprovider.
            <ls_additional_tool>-steptype           = <ls_step_4_validate>-steptype.
            <ls_additional_tool>-toolschemaprovider = <ls_step_4_validate>-toolschemaprovider.
            <ls_additional_tool>-toolinfoprovider   = <ls_step_4_validate>-toolinfoprovider.
          ENDIF.

          IF <ls_existing_agent> IS ASSIGNED.
            <ls_additional_tool>-agentuuid = <ls_existing_agent>-agentuuid.
          ELSE.
            DATA(lv_temp_agent_uuid) = cl_system_uuid=>create_uuid_x16_static( ).
            <ls_additional_tool>-agentuuid = lv_temp_agent_uuid.
          ENDIF.

          IF     <ls_existing_agent> IS NOT ASSIGNED
             AND <ls_existing_tool>  IS NOT ASSIGNED.
            <ls_additional_tool>-istransient = abap_true.
          ENDIF.

          IF     <ls_existing_agent> IS ASSIGNED
             AND <ls_existing_tool>  IS ASSIGNED.
            <ls_additional_tool>-isborrowed = abap_true.
          ENDIF.

          APPEND INITIAL LINE TO et_additional_steps ASSIGNING FIELD-SYMBOL(<ls_additional_steps>).

          <ls_additional_steps>-stepuuid   = cl_system_uuid=>create_uuid_x16_static( ).

          <ls_additional_steps>-stepnumber = lo_axc_service->generate_step_number(
                                                 iv_query_uuid = is_current_step-queryuuid ).
          <ls_additional_steps>-queryuuid  = is_current_step-queryuuid.
          <ls_additional_steps>-runuuid    = is_current_step-runuuid.
          <ls_additional_steps>-tooluuid   = <ls_additional_tool>-tooluuid.

          CLEAR: lv_temp_tool_uuid,
                 lv_temp_agent_uuid.

        CATCH cx_uuid_error.
          RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
      ENDTRY.
    ENDLOOP.

    GET TIME STAMP FIELD DATA(lv_now).

    IF    lv_wrong_step_type       = abap_true
       OR lv_wrong_tool_provider   = abap_true
       OR lv_wrong_schema_provider = abap_true
       OR lv_wrong_info_provider   = abap_true
       OR lv_wrong_agent_tool_comb = abap_true.

      DATA(lv_additional_error) = COND #( WHEN lv_wrong_step_type = abap_true THEN
                                            |{ ls_last_step-toolname } has step type { ls_last_step-steptype } |
                                          WHEN lv_wrong_tool_provider = abap_true THEN
                                            |{ ls_last_step-toolname } has tool provider { ls_last_step-toolprovider }|
                                          WHEN lv_wrong_schema_provider = abap_true THEN
                                            |{ ls_last_step-toolname } has schema provider { ls_last_step-toolschemaprovider }|
                                          WHEN lv_wrong_info_provider = abap_true THEN
                                            |{ ls_last_step-toolname } has info provider { ls_last_step-toolinfoprovider }|
                                          WHEN lv_wrong_agent_tool_comb = abap_true THEN
                                            |Wrong combination of agent { ls_last_step-agentuuid } and tool { ls_last_step-tooluuid }| ).

      lt_message_in = VALUE #(
          ( messagecid  = |{ lv_now }-{ sy-uname }-VALIDATE_ADDITIONAL_STEPS_{ is_current_step-stepuuid }|
            stage        = 'VALIDATE_ADDITIONAL_STEPS'
            substage    = 'AFTER VALIDATION'
            namespace    = |{ sy-uname }.{ ls_current_agent-agentname }.{ ls_current_run-runid }.{ ls_current_query-querynumber }|
            username    = sy-uname
            agentuuid   = ls_current_agent-agentuuid
            messagetime = lv_now
            content      = |\{ "AGENT_NAME" : "{ ls_current_agent-agentname }", | &&
                           | "ADDITIONAL_STEP_ERROR" : "{ lv_additional_error }" \}|
            messagetype = zpru_if_short_memory_provider=>cs_msg_type-info ) ).
      TRY.
          io_controller->mo_short_memory->save_message( lt_message_in ).
        CATCH zpru_cx_agent_core.
          RETURN.
      ENDTRY.
    ENDIF.
  ENDMETHOD.

  METHOD preprocess_tool_execution.
    DATA lo_tool_schema_provider TYPE REF TO zpru_if_tool_schema_provider.
    DATA lo_tool_info_provider   TYPE REF TO zpru_if_tool_info_provider.
    DATA lr_input                TYPE REF TO data.
    DATA lr_output               TYPE REF TO data.
    DATA lo_util                 TYPE REF TO zpru_if_agent_util.

    ev_error_flag = abap_false.

    CREATE OBJECT lo_tool_schema_provider TYPE (is_tool_master_data-toolschemaprovider).
    IF sy-subrc <> 0.
      ev_error_flag = abap_true.
      RETURN.
    ENDIF.

    eo_tool_schema_provider = lo_tool_schema_provider.

    CREATE OBJECT lo_tool_info_provider TYPE (is_tool_master_data-toolinfoprovider).
    IF sy-subrc <> 0.
      ev_error_flag = abap_true.
      RETURN.
    ENDIF.

    eo_tool_info_provider = lo_tool_info_provider.

    TRY.
        lo_util ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_AGENT_UTIL`
                                                            iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    eo_util = lo_util.

    DATA(lo_structure_input) = lo_tool_schema_provider->input_rtts_schema( is_tool_master_data = is_tool_master_data
                                                                           is_execution_step   = is_execution_step  ).

    eo_structure_input = lo_structure_input.

    CREATE DATA lr_input TYPE HANDLE lo_structure_input.

    lo_util->convert_to_abap(
      EXPORTING
        ir_string = io_request->get_data( )->*
      CHANGING
        cr_abap   = lr_input->* ).

    er_input = er_input.

    DATA(lo_structure_output) = lo_tool_schema_provider->output_rtts_schema( is_tool_master_data = is_tool_master_data
                                                                             is_execution_step   = is_execution_step  ).

    eo_structure_output = lo_structure_output.

    CREATE DATA lr_output TYPE HANDLE lo_structure_output.

    er_output = lr_output.
  ENDMETHOD.

  METHOD postprocess_tool_execution.
    " TODO: parameter EO_RESPONSE is never cleared or assigned (ABAP cleaner)

    DATA lv_output_json TYPE zpru_if_agent_frw=>ts_json.

    io_util->convert_to_string( EXPORTING ir_abap   = ir_output
                                CHANGING  cr_string = lv_output_json ).

    eo_response->set_data( NEW zpru_if_agent_frw=>ts_json( lv_output_json ) ).

    ASSIGN io_controller->mt_run_context[ execution_step-stepuuid = is_execution_step-stepuuid ] TO FIELD-SYMBOL(<ls_current_run_context>).
    IF sy-subrc <> 0.
      ev_error_flag = abap_true.
      RETURN.
    ENDIF.

    DATA(ls_input_json_schema) = io_tool_schema_provider->input_json_schema( is_tool_master_data = is_tool_master_data
                                                                             is_execution_step   = is_execution_step ).

    DATA(ls_output_json_schema) = io_tool_schema_provider->output_json_schema(
                                      is_tool_master_data = is_tool_master_data
                                      is_execution_step   = is_execution_step ).

    <ls_current_run_context>-abap_input_schema  = io_structure_input.
    <ls_current_run_context>-json_input_schema  = ls_input_json_schema.
    <ls_current_run_context>-abap_output_schema = io_structure_output.
    <ls_current_run_context>-json_output_schema = ls_output_json_schema.
    <ls_current_run_context>-abap_response      = ir_output.
    <ls_current_run_context>-json_response      = lv_output_json.
    <ls_current_run_context>-abap_request       = ir_input.
    <ls_current_run_context>-json_request       = io_request->get_data( )->*.
  ENDMETHOD.
ENDCLASS.
