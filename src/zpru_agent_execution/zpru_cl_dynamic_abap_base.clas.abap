CLASS zpru_cl_dynamic_abap_base DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zpru_if_agent_frw.
    INTERFACES zpru_if_dynamic_abap_processor.
    INTERFACES zpru_if_tool_executor.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zpru_cl_dynamic_abap_base IMPLEMENTATION.
  METHOD zpru_if_dynamic_abap_processor~process_dynamic_abap.
    DATA lt_params                 TYPE abap_parmbind_tab.
    DATA ls_param                  TYPE abap_parmbind.
    DATA lo_instance               TYPE REF TO object.
    DATA lo_util                   TYPE REF TO zpru_if_agent_util.
    DATA lv_input                  TYPE string.
    DATA lt_invocation_payload     TYPE STANDARD TABLE OF zpru_if_axc_type_and_constant=>ts_tool_invocation WITH EMPTY KEY.
    DATA lt_invocation_result      TYPE STANDARD TABLE OF zpru_if_axc_type_and_constant=>ts_tool_invocation WITH EMPTY KEY.
    DATA lv_invokation_result_json TYPE zpru_if_agent_frw=>ts_json.

    TRY.
        lo_util ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_AGENT_UTIL`
                                                            iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    lv_input = io_request->get_data( )->*.

    DATA(lv_invokation_metadata) = lo_util->search_node_in_json( iv_json           = lv_input
                                                                 iv_field_2_search = 'dynamic_tool' ).

    lo_util->convert_to_abap( EXPORTING ir_string = REF #( lv_invokation_metadata )
                              CHANGING  cr_abap   = lt_invocation_payload ).

    IF lt_invocation_payload IS INITIAL.
      RETURN.
    ENDIF.

    SELECT log_area, class_name, method_name, is_static
      FROM zpru_dyn_list
      FOR ALL ENTRIES IN @lt_invocation_payload
      WHERE log_area    = @lt_invocation_payload-log_area
        AND class_name  = @lt_invocation_payload-class_name
        AND method_name = @lt_invocation_payload-method_name
      INTO TABLE @DATA(lt_dynamic_methods).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    SELECT * FROM zpru_dyn_list_pr
      FOR ALL ENTRIES IN @lt_dynamic_methods
      WHERE log_area    = @lt_dynamic_methods-log_area
        AND class_name  = @lt_dynamic_methods-class_name
        AND method_name = @lt_dynamic_methods-method_name
      INTO TABLE @DATA(lt_dynamic_params).

    LOOP AT lt_invocation_payload ASSIGNING FIELD-SYMBOL(<ls_invocation_payload>).

      ASSIGN lt_dynamic_methods[ log_area    = <ls_invocation_payload>-log_area
                                 class_name  = <ls_invocation_payload>-class_name
                                 method_name = <ls_invocation_payload>-method_name ] TO FIELD-SYMBOL(<ls_dynamic_method>).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      CLEAR lt_params.
      LOOP AT lt_dynamic_params ASSIGNING FIELD-SYMBOL(<ls_dynamic_param>) WHERE     log_area    = <ls_dynamic_method>-log_area
                                                                                 AND class_name  = <ls_dynamic_method>-class_name
                                                                                 AND method_name = <ls_dynamic_method>-method_name.
        CLEAR ls_param.
        ls_param-name = <ls_dynamic_param>-param_name.
        ls_param-kind = SWITCH #( <ls_dynamic_param>-param_type
                                  WHEN 'I' THEN cl_abap_objectdescr=>exporting
                                  WHEN 'E' THEN cl_abap_objectdescr=>importing
                                  WHEN 'C' THEN cl_abap_objectdescr=>changing
                                  WHEN 'R' THEN cl_abap_objectdescr=>receiving ).

        CREATE DATA ls_param-value TYPE (<ls_dynamic_param>-rollname).

        ASSIGN <ls_invocation_payload>-parameters[ name = <ls_dynamic_param>-param_name ] TO FIELD-SYMBOL(<ls_parameter_metadata>).
        IF sy-subrc = 0.
          ASSIGN ls_param-value->* TO FIELD-SYMBOL(<ls_passing_value>).
          IF sy-subrc = 0.
            <ls_passing_value> = <ls_parameter_metadata>-value.
          ENDIF.
        ELSE.
          IF <ls_dynamic_param>-default_value IS NOT INITIAL.
            ASSIGN ls_param-value->* TO <ls_passing_value>.
            IF sy-subrc = 0.
              <ls_passing_value> = <ls_dynamic_param>-default_value.
            ENDIF.
          ENDIF.
        ENDIF.

        INSERT ls_param INTO TABLE lt_params.
      ENDLOOP.

      TRY.
          IF <ls_dynamic_method>-is_static = abap_true.
            CALL METHOD (<ls_dynamic_method>-class_name)=>(<ls_dynamic_method>-method_name)
              PARAMETER-TABLE lt_params.
          ELSE.
            CREATE OBJECT lo_instance TYPE (<ls_dynamic_method>-class_name).

            CALL METHOD lo_instance->(<ls_dynamic_method>-method_name)
              PARAMETER-TABLE lt_params.
          ENDIF.

          APPEND INITIAL LINE TO lt_invocation_result ASSIGNING FIELD-SYMBOL(<ls_result>).
          <ls_result>-log_area    = <ls_dynamic_method>-log_area.
          <ls_result>-class_name  = <ls_dynamic_method>-class_name.
          <ls_result>-method_name = <ls_dynamic_method>-method_name.

          LOOP AT lt_params ASSIGNING FIELD-SYMBOL(<ls_par_result>).
            APPEND INITIAL LINE TO <ls_result>-parameters ASSIGNING FIELD-SYMBOL(<ls_par_target>).
            <ls_par_target>-name  = <ls_par_result>-name.
            <ls_par_target>-type  = <ls_par_result>-kind.
            <ls_par_target>-value = <ls_par_result>-value->*.

          ENDLOOP.
        CATCH cx_sy_dyn_call_error
              cx_sy_no_handler
              cx_sy_ref_is_initial
              cx_sy_create_object_error.
          RETURN.
      ENDTRY.

    ENDLOOP.

    lo_util->convert_to_string( EXPORTING ir_abap   = REF #( lt_invocation_result )
                                CHANGING  cr_string = lv_invokation_result_json ).

    IF lv_invokation_result_json IS NOT INITIAL.

      DATA(lv_new_json) = lo_util->append_json_to_json( iv_field_4_append = 'dynamic_tool_result'
                                                        iv_json_4_append  = lv_invokation_result_json
                                                        iv_json_target    = lv_input  ).

      eo_response->set_data( ir_data = NEW string( lv_new_json ) ).
    ELSE.
      eo_response->set_data( ir_data = NEW string( lv_input ) ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
