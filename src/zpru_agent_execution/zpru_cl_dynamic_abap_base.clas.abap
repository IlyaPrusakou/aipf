CLASS zpru_cl_dynamic_abap_base DEFINITION
  PUBLIC
  INHERITING FROM zpru_cl_dynamic_abap_processor
  CREATE PUBLIC.

  PUBLIC SECTION.

  PROTECTED SECTION.
    METHODS process_dynamic_abap_int REDEFINITION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zpru_cl_dynamic_abap_base IMPLEMENTATION.
  METHOD process_dynamic_abap_int.
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

    lv_input = io_input->*.

    DATA(lv_invokation_metadata) = lo_util->search_node_in_json( iv_json           = lv_input
                                                                 iv_field_2_search = 'dynamic_tool' ).

    lo_util->convert_to_abap( EXPORTING ir_string = REF #( lv_invokation_metadata )
                              CHANGING  cr_abap   = lt_invocation_payload ).

    IF lt_invocation_payload IS INITIAL.
      RETURN.
    ENDIF.

    SELECT logarea    AS logarea,
           classname  AS classname,
           methodname AS methodname,
           isstatic   AS isstatic
      FROM zpru_dyn_list
      FOR ALL ENTRIES IN @lt_invocation_payload
      WHERE logarea    = @lt_invocation_payload-logarea
        AND classname  = @lt_invocation_payload-classname
        AND methodname = @lt_invocation_payload-methodname
      INTO TABLE @DATA(lt_dynamic_methods).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    SELECT logarea      AS logarea,
           classname    AS classname,
           methodname   AS methodname,
           paramname    AS paramname,
           paramorder   AS paramorder,
           paramtype    AS paramtype,
           rollname      AS rollname,
           default_value AS defaultvalue
      FROM zpru_dyn_list_pr
      FOR ALL ENTRIES IN @lt_dynamic_methods
      WHERE logarea    = @lt_dynamic_methods-logarea
        AND classname  = @lt_dynamic_methods-classname
        AND methodname = @lt_dynamic_methods-methodname
      INTO TABLE @DATA(lt_dynamic_params).

    LOOP AT lt_invocation_payload ASSIGNING FIELD-SYMBOL(<ls_invocation_payload>).

      ASSIGN lt_dynamic_methods[ logarea    = <ls_invocation_payload>-logarea
                                 classname  = <ls_invocation_payload>-classname
                                 methodname = <ls_invocation_payload>-methodname ] TO FIELD-SYMBOL(<ls_dynamic_method>).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      CLEAR lt_params.
      LOOP AT lt_dynamic_params ASSIGNING FIELD-SYMBOL(<ls_dynamic_param>) WHERE     logarea    = <ls_dynamic_method>-logarea
                                                                                 AND classname  = <ls_dynamic_method>-classname
                                                                                 AND methodname = <ls_dynamic_method>-methodname.
        CLEAR ls_param.
        ls_param-name = <ls_dynamic_param>-paramname.
        ls_param-kind = SWITCH #( <ls_dynamic_param>-paramtype
                                  WHEN 'I' THEN cl_abap_objectdescr=>exporting
                                  WHEN 'E' THEN cl_abap_objectdescr=>importing
                                  WHEN 'C' THEN cl_abap_objectdescr=>changing
                                  WHEN 'R' THEN cl_abap_objectdescr=>receiving ).

        CREATE DATA ls_param-value TYPE (<ls_dynamic_param>-rollname).

        ASSIGN <ls_invocation_payload>-parameters[ name = <ls_dynamic_param>-paramname ] TO FIELD-SYMBOL(<ls_parameter_metadata>).
        IF sy-subrc = 0.
          ASSIGN ls_param-value->* TO FIELD-SYMBOL(<ls_passing_value>).
          IF sy-subrc = 0.
            <ls_passing_value> = <ls_parameter_metadata>-value.
          ENDIF.
        ELSE.
          IF <ls_dynamic_param>-defaultvalue IS NOT INITIAL.
            ASSIGN ls_param-value->* TO <ls_passing_value>.
            IF sy-subrc = 0.
              <ls_passing_value> = <ls_dynamic_param>-defaultvalue.
            ENDIF.
          ENDIF.
        ENDIF.

        INSERT ls_param INTO TABLE lt_params.
      ENDLOOP.

      TRY.
          IF <ls_dynamic_method>-isstatic = abap_true.
            CALL METHOD (<ls_dynamic_method>-classname)=>(<ls_dynamic_method>-methodname)
              PARAMETER-TABLE lt_params.
          ELSE.
            CREATE OBJECT lo_instance TYPE (<ls_dynamic_method>-classname).

            CALL METHOD lo_instance->(<ls_dynamic_method>-methodname)
              PARAMETER-TABLE lt_params.
          ENDIF.

          APPEND INITIAL LINE TO lt_invocation_result ASSIGNING FIELD-SYMBOL(<ls_result>).
          <ls_result>-logarea    = <ls_dynamic_method>-logarea.
          <ls_result>-classname  = <ls_dynamic_method>-classname.
          <ls_result>-methodname = <ls_dynamic_method>-methodname.

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

      er_output = NEW string( lv_new_json ).
    ELSE.
      er_output = NEW string( lv_input ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
