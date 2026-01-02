CLASS zpru_cl_dynamic_abap_base DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zpru_if_dynamic_abap_processor .
    INTERFACES zpru_if_tool_executor .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zpru_cl_dynamic_abap_base IMPLEMENTATION.


  METHOD zpru_if_dynamic_abap_processor~process_dynamic_abap.

    TYPES: BEGIN OF ts_json_parameter,
             name  TYPE string,
             type  TYPE string,
             value TYPE string,
           END OF ts_json_parameter.

    TYPES: tt_json_parameters TYPE STANDARD TABLE OF ts_json_parameter WITH EMPTY KEY.

    TYPES: BEGIN OF ts_tool_invocation,
             log_area    TYPE char20,
             class_name  TYPE char30,
             method_name TYPE char30,
             parameters  TYPE tt_json_parameters,
           END OF ts_tool_invocation.

    DATA: lt_params   TYPE abap_parmbind_tab,
          ls_param    TYPE abap_parmbind,
          lo_instance TYPE REF TO object.
    DATA lo_util TYPE REF TO zpru_if_agent_util.
    DATA lv_input TYPE string.
    DATA lt_invocation_metadata TYPE STANDARD TABLE OF ts_tool_invocation WITH EMPTY KEY.
    DATA lt_invocation_result TYPE STANDARD TABLE OF ts_tool_invocation WITH EMPTY KEY.

    lv_input = io_request->get_data( )->*.
    lo_util = NEW zpru_cl_agent_util( ).

    DATA(lv_invokation_metadata) = lo_util->search_node_in_json(
      EXPORTING
        iv_json           = lv_input
        iv_field_2_search = 'dynamic_tool' ).

    lo_util->convert_to_abap(
      EXPORTING
        ir_string = REF #( lv_invokation_metadata )
      CHANGING
        cr_abap   = lt_invocation_metadata ).

    SELECT log_area, class_name, method_name, is_static
      FROM zpru_dyn_list
      FOR ALL ENTRIES IN @lt_invocation_metadata
      WHERE log_area = @lt_invocation_metadata-log_area AND
            class_name = @lt_invocation_metadata-class_name AND
            method_name = @lt_invocation_metadata-method_name INTO TABLE @DATA(lt_header).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    SELECT * FROM zpru_dyn_list_pr
    FOR ALL ENTRIES IN @lt_header
      WHERE log_area    = @lt_header-log_area
        AND class_name  = @lt_header-class_name
        AND method_name = @lt_header-method_name
      INTO TABLE @DATA(lt_config_params).

    LOOP AT lt_header ASSIGNING FIELD-SYMBOL(<ls_header>).

      ASSIGN lt_invocation_metadata[ log_area =  <ls_header>-log_area
      class_name = <ls_header>-class_name
      method_name = <ls_header>-method_name ] TO FIELD-SYMBOL(<ls_invocation_metadata>).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.


      CLEAR lt_params.
      LOOP AT lt_config_params ASSIGNING FIELD-SYMBOL(<ls_config_param>) WHERE log_area = <ls_header>-log_area AND
      class_name = <ls_header>-class_name AND
      method_name = <ls_header>-method_name.
        CLEAR ls_param.
        ls_param-name = <ls_config_param>-param_name.
        ls_param-kind = SWITCH #( <ls_config_param>-param_type
                                  WHEN 'I' THEN cl_abap_objectdescr=>exporting
                                  WHEN 'E' THEN cl_abap_objectdescr=>importing
                                  WHEN 'C' THEN cl_abap_objectdescr=>changing
                                  WHEN 'R' THEN cl_abap_objectdescr=>receiving ).

        CREATE DATA ls_param-value TYPE (<ls_config_param>-rollname).

        ASSIGN <ls_invocation_metadata>-parameters[ name = <ls_config_param>-param_name ] TO FIELD-SYMBOL(<ls_parameter_metadata>).
        IF  sy-subrc = 0.
          ASSIGN ls_param-value->* TO FIELD-SYMBOL(<ls_val>).
          IF sy-subrc = 0.
            <ls_val> = <ls_parameter_metadata>-value.
          ENDIF.
        ELSE.
          IF <ls_config_param>-default_value IS NOT INITIAL.
            ASSIGN ls_param-value->* TO <ls_val>.
            IF sy-subrc = 0.
              <ls_val> = <ls_config_param>-default_value.
            ENDIF.
          ENDIF.
        ENDIF.

        INSERT ls_param INTO TABLE lt_params.
      ENDLOOP.

      TRY.
          IF <ls_header>-is_static = abap_true.
            CALL METHOD (<ls_header>-class_name)=>(<ls_header>-method_name)
              PARAMETER-TABLE lt_params.
          ELSE.
            CREATE OBJECT lo_instance TYPE (<ls_header>-class_name).

            CALL METHOD lo_instance->(<ls_header>-method_name)
              PARAMETER-TABLE lt_params.
          ENDIF.

          APPEND INITIAL LINE TO lt_invocation_result ASSIGNING FIELD-SYMBOL(<ls_result>).
          <ls_result>-log_area = <ls_header>-log_area.
          <ls_result>-class_name = <ls_header>-class_name.
          <ls_result>-method_name = <ls_header>-method_name.

          LOOP AT lt_params ASSIGNING FIELD-SYMBOL(<ls_par_result>).
            APPEND INITIAL LINE TO <ls_result>-parameters ASSIGNING FIELD-SYMBOL(<ls_par_target>).
            <ls_par_target>-name = <ls_par_result>-name.
            <ls_par_target>-type = <ls_par_result>-kind.
            <ls_par_target>-value = <ls_par_result>-value->*.

          ENDLOOP.
        CATCH cx_sy_dyn_call_error INTO DATA(lo_err).
          RETURN.
      ENDTRY.

    ENDLOOP.

*SERIALIZE AND ADD TO RESPOND + CONTEXT
*ALSO ADD IN SSYTEM PROMPT EXAMPLE ABOUT PAREMETER TYPE
*ALSO RENAME PARAMETER TYPE INTO KIND IN DB TABLE AND SYSTEM PROMPT

  ENDMETHOD.
ENDCLASS.
