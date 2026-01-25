CLASS zpru_cl_tool_executor DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zpru_if_agent_frw.
    INTERFACES zpru_if_tool_executor.

  PROTECTED SECTION.
    METHODS validate_additional_steps
      IMPORTING is_execution_step   TYPE zpru_if_axc_type_and_constant=>ts_axc_step
                it_step_4_validate  TYPE zpru_tt_additional_step
      EXPORTING et_additional_steps TYPE zpru_if_axc_type_and_constant=>tt_axc_step
                et_additional_tools TYPE zpru_if_adf_type_and_constant=>tt_agent_tool.

  PRIVATE SECTION.
ENDCLASS.


CLASS zpru_cl_tool_executor IMPLEMENTATION.
  METHOD validate_additional_steps.
    DATA lo_axc_service TYPE REF TO zpru_if_axc_service.
    DATA lo_adf_service TYPE REF TO zpru_if_adf_service.
    DATA lo_adf_validator TYPE REF TO zpru_if_adf_validator.

    CLEAR: et_additional_steps,
           et_additional_tools.

    TRY.
        lo_axc_service ?= zpru_cl_agent_service_mngr=>get_service(
                              iv_service = `ZPRU_IF_AXC_SERVICE`
                              iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.

    ENDTRY.

    TRY.
        lo_adf_service ?= zpru_cl_agent_service_mngr=>get_service(
                              iv_service = `ZPRU_IF_ADF_SERVICE`
                              iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.

    ENDTRY.

    TRY.
        lo_adf_validator ?= zpru_cl_agent_service_mngr=>get_service(
                                iv_service = `ZPRU_IF_ADF_VALIDATOR`
                                iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.


    lo_adf_service->read_agent( EXPORTING it_agent_read_k = VALUE #( FOR <ls_a> IN it_step_4_validate
                                                                     ( agent_uuid         = <ls_a>-agentuuid
                                                                       control-agent_uuid = abap_true ) )
                                IMPORTING et_agent        = DATA(lt_existing_agent) ).

    lo_adf_service->read_tool( EXPORTING it_tool_read_k = VALUE #( FOR <ls_a2> IN it_step_4_validate
                                                                   ( agent_uuid         = <ls_a2>-agentuuid
                                                                     tool_uuid          = <ls_a2>-tooluuid
                                                                     control-agent_uuid = abap_true
                                                                     control-tool_uuid  = abap_true ) )
                               IMPORTING et_tool        = DATA(lt_existing_tool) ).

    DATA(lo_elem) = CAST cl_abap_elemdescr( cl_abap_typedescr=>describe_by_name( 'ZPRU_DE_ADF_STEP_TYPE' ) ).
    DATA(lt_fixed_values) = lo_elem->get_ddic_fixed_values( ).

    LOOP AT it_step_4_validate ASSIGNING FIELD-SYMBOL(<ls_step_4_validate>).

      IF line_exists( lt_fixed_values[ low = <ls_step_4_validate>-steptype ] ).
        CONTINUE.
      ENDIF.

      lo_adf_validator->validate_provider_cls(
        EXPORTING
          iv_class            = <ls_step_4_validate>-toolprovider
          iv_intf_2_be_impl_1 = 'ZPRU_IF_TOOL_PROVIDER'
        IMPORTING
          ev_type_not_exist   = DATA(lv_type_not_exist)
          ev_type_not_class   = DATA(lv_type_not_class)
          ev_intf_not_impl_1  = DATA(lv_intf_not_impl_1) ).

      IF lv_type_not_exist = abap_true OR
         lv_type_not_class = abap_true OR
         lv_intf_not_impl_1 = abap_true.
        CONTINUE.
      ENDIF.

      lo_adf_validator->validate_provider_cls(
        EXPORTING
          iv_class            = <ls_step_4_validate>-toolprovider
          iv_intf_2_be_impl_1 = 'ZPRU_IF_TOOL_SCHEMA_PROVIDER'
        IMPORTING
          ev_type_not_exist   = lv_type_not_exist
          ev_type_not_class   = lv_type_not_class
          ev_intf_not_impl_1  = lv_intf_not_impl_1 ).

      IF lv_type_not_exist = abap_true OR
         lv_type_not_class = abap_true OR
         lv_intf_not_impl_1 = abap_true.
        CONTINUE.
      ENDIF.

      lo_adf_validator->validate_provider_cls(
        EXPORTING
          iv_class            = <ls_step_4_validate>-toolprovider
          iv_intf_2_be_impl_1 = 'ZPRU_IF_TOOL_INFO_PROVIDER'
        IMPORTING
          ev_type_not_exist   = lv_type_not_exist
          ev_type_not_class   = lv_type_not_class
          ev_intf_not_impl_1  = lv_intf_not_impl_1 ).

      IF lv_type_not_exist = abap_true OR
         lv_type_not_class = abap_true OR
         lv_intf_not_impl_1 = abap_true.
        CONTINUE.
      ENDIF.

      TRY.
          ASSIGN lt_existing_agent[ agent_uuid = <ls_step_4_validate>-agentuuid ] TO FIELD-SYMBOL(<ls_existing_agent>).
          ASSIGN lt_existing_tool[ tool_uuid = <ls_step_4_validate>-tooluuid ] TO FIELD-SYMBOL(<ls_existing_tool>).

          APPEND INITIAL LINE TO et_additional_tools ASSIGNING FIELD-SYMBOL(<ls_additional_tool>).

          IF <ls_existing_tool> IS ASSIGNED.
            <ls_additional_tool>-tool_uuid            = <ls_existing_tool>-tool_uuid.
            <ls_additional_tool>-tool_name            = <ls_existing_tool>-tool_name.
            <ls_additional_tool>-tool_provider        = <ls_existing_tool>-tool_provider.
            <ls_additional_tool>-step_type            = <ls_existing_tool>-step_type.
            <ls_additional_tool>-tool_schema_provider = <ls_existing_tool>-tool_schema_provider.
            <ls_additional_tool>-tool_info_provider   = <ls_existing_tool>-tool_info_provider.
          ELSE.
            DATA(lv_temp_tool_uuid)        = cl_system_uuid=>create_uuid_x16_static( ).
            <ls_additional_tool>-tool_uuid            = lv_temp_tool_uuid.
            <ls_additional_tool>-tool_name            = <ls_step_4_validate>-toolname.
            <ls_additional_tool>-tool_provider        = <ls_step_4_validate>-toolprovider.
            <ls_additional_tool>-step_type            = <ls_step_4_validate>-steptype.
            <ls_additional_tool>-tool_schema_provider = <ls_step_4_validate>-toolschemaprovider.
            <ls_additional_tool>-tool_info_provider   = <ls_step_4_validate>-toolinfoprovider.
          ENDIF.

          IF <ls_existing_agent> IS ASSIGNED.
            <ls_additional_tool>-agent_uuid = <ls_step_4_validate>-agentuuid.
          ELSE.
            DATA(lv_temp_agent_uuid) = cl_system_uuid=>create_uuid_x16_static( ).
            <ls_additional_tool>-agent_uuid = lv_temp_agent_uuid.
          ENDIF.

          APPEND INITIAL LINE TO et_additional_steps ASSIGNING FIELD-SYMBOL(<ls_additional_steps>).

          <ls_additional_steps>-step_uuid   = cl_system_uuid=>create_uuid_x16_static( ).

          <ls_additional_steps>-step_number = lo_axc_service->generate_step_number(
                                                  iv_query_uuid = is_execution_step-query_uuid ).
          <ls_additional_steps>-query_uuid  = is_execution_step-query_uuid.
          <ls_additional_steps>-run_uuid    = is_execution_step-run_uuid.
          <ls_additional_steps>-tool_uuid   = <ls_additional_tool>-tool_uuid.

          CLEAR: lv_temp_tool_uuid,
                 lv_temp_agent_uuid.

        CATCH cx_uuid_error.
          CONTINUE.
      ENDTRY.

    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
