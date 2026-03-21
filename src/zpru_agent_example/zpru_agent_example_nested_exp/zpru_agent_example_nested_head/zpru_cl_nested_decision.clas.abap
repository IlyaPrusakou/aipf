CLASS zpru_cl_nested_decision DEFINITION
  PUBLIC
  INHERITING FROM zpru_cl_decision_provider
  CREATE PUBLIC.

  PUBLIC SECTION.

  PROTECTED SECTION.
    METHODS check_authorizations        REDEFINITION.
    METHODS recall_memory               REDEFINITION.
    METHODS read_data_4_thinking        REDEFINITION.
    METHODS process_thinking            REDEFINITION.
    METHODS prepare_first_tool_input    REDEFINITION.
    METHODS set_model_id                REDEFINITION.
    METHODS set_result_comment          REDEFINITION.
    METHODS set_final_response_content  REDEFINITION.
    METHODS set_final_response_metadata REDEFINITION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zpru_cl_nested_decision IMPLEMENTATION.
  METHOD check_authorizations.
    ev_allowed = abap_true.
  ENDMETHOD.

  METHOD prepare_first_tool_input.

    DATA ls_nested_abap TYPE zpru_s_nested_abap_input.
    DATA ls_nested_http TYPE zpru_s_nested_http_input.
    DATA ls_nested_llm TYPE zpru_s_nested_llm_input.
    DATA lo_util                 TYPE REF TO zpru_if_agent_util.
    DATA lr_nested_prompt    TYPE REF TO data.
    DATA lv_input TYPE string.

    FIELD-SYMBOLS <ls_nested_abap_input> TYPE any.
    FIELD-SYMBOLS <ls_nested_abap> TYPE zpru_s_nested_abap_input.
    FIELD-SYMBOLS <ls_nested_http> TYPE zpru_s_nested_http_input.
    FIELD-SYMBOLS <ls_nested_llm> TYPE zpru_s_nested_llm_input.

    ASSIGN er_first_tool_input->* TO <ls_nested_abap_input>.
    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDIF.

    lo_util ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_AGENT_UTIL`
                                                        iv_context = zpru_if_agent_frw=>cs_context-standard ).

    lv_input = io_input->get_data( )->*.

    IF lo_util->is_wrapped_in_json_markdown( iv_content = lv_input ) = abap_true.
      lv_input = lo_util->unwrap_from_json_markdown( iv_markdown = lv_input ).
    ENDIF.

    IF lo_util->is_wrapped_in_text_markdown( iv_content = lv_input ) = abap_true.
      lv_input = lo_util->unwrap_from_text_markdown( iv_markdown = lv_input ).
    ENDIF.

    CREATE DATA lr_nested_prompt TYPE (is_input_prompt-type).

    ASSIGN lr_nested_prompt->* TO FIELD-SYMBOL(<ls_input>).
    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDIF.

    lo_util->convert_to_abap(
      EXPORTING
        ir_string = REF #( lv_input )
      CHANGING
        cr_abap   = <ls_input> ).


    CASE is_first_tool-toolname.

      WHEN 'NESTED_ABAP'.
        ls_nested_abap-nestedabapinput = `nested first abap input`.

        ASSIGN COMPONENT 'WAREHOUSE' OF STRUCTURE <ls_input> TO FIELD-SYMBOL(<lv_warehouse>).
        IF sy-subrc = 0.
          ls_nested_abap-warehouse = <lv_warehouse>.
        ELSE.
          ls_nested_abap-warehouse = `BS01`.
        ENDIF.

        ASSIGN COMPONENT 'STORAGEBIN' OF STRUCTURE <ls_input> TO FIELD-SYMBOL(<lv_storagebin>).
        IF sy-subrc = 0.
          ls_nested_abap-storagebin = <lv_storagebin>.
        ELSE.
          ls_nested_abap-storagebin = `MY_BIN3`.
        ENDIF.

        ASSIGN COMPONENT 'OUTBOUNDDELIVERYHEADER' OF STRUCTURE <ls_input> TO FIELD-SYMBOL(<ls_outbounddeliveryheader>).
        IF sy-subrc = 0.
          ls_nested_abap-outbounddeliveryheader = <ls_outbounddeliveryheader>.
        ENDIF.

        ASSIGN COMPONENT 'OUTBOUNDDELIVERYITEMS' OF STRUCTURE <ls_input> TO FIELD-SYMBOL(<ls_outbounddeliveryitems>).
        IF sy-subrc = 0.
          ls_nested_abap-outbounddeliveryitems = <ls_outbounddeliveryitems>.
        ENDIF.

        ASSIGN er_first_tool_input->* TO <ls_nested_abap>.
        IF sy-subrc <> 0.
          RAISE EXCEPTION NEW zpru_cx_agent_core( ).
        ENDIF.
        <ls_nested_abap> = ls_nested_abap.

      WHEN 'NESTED_LLM'.
        ls_nested_llm-nestedllminput = `nested first llm input`.

        ASSIGN COMPONENT 'WAREHOUSE' OF STRUCTURE <ls_input> TO <lv_warehouse>.
        IF sy-subrc = 0.
          ls_nested_llm-warehouse = <lv_warehouse>.
        ELSE.
          ls_nested_llm-warehouse = `BS01`.
        ENDIF.

        ASSIGN COMPONENT 'STORAGEBIN' OF STRUCTURE <ls_input> TO <lv_storagebin>.
        IF sy-subrc = 0.
          ls_nested_llm-storagebin = <lv_storagebin>.
        ELSE.
          ls_nested_llm-storagebin = `MY_BIN3`.
        ENDIF.

        ASSIGN COMPONENT 'OUTBOUNDDELIVERYHEADER' OF STRUCTURE <ls_input> TO <ls_outbounddeliveryheader>.
        IF sy-subrc = 0.
          ls_nested_llm-outbounddeliveryheader = <ls_outbounddeliveryheader>.
        ENDIF.

        ASSIGN COMPONENT 'OUTBOUNDDELIVERYITEMS' OF STRUCTURE <ls_input> TO <ls_outbounddeliveryitems>.
        IF sy-subrc = 0.
          ls_nested_llm-outbounddeliveryitems = <ls_outbounddeliveryitems>.
        ENDIF.

        ASSIGN er_first_tool_input->* TO <ls_nested_llm>.
        IF sy-subrc <> 0.
          RAISE EXCEPTION NEW zpru_cx_agent_core( ).
        ENDIF.
        <ls_nested_llm> = ls_nested_llm.

      WHEN 'NESTED_HTTP'.
        ls_nested_http-nestedhttpinput = `nested first http input`.

        ASSIGN COMPONENT 'WAREHOUSE' OF STRUCTURE <ls_input> TO <lv_warehouse>.
        IF sy-subrc = 0.
          ls_nested_http-warehouse = <lv_warehouse>.
        ELSE.
          ls_nested_http-warehouse = `BS01`.
        ENDIF.

        ASSIGN COMPONENT 'RESOURCE' OF STRUCTURE <ls_input> TO FIELD-SYMBOL(<lv_resource>).
        IF sy-subrc = 0.
          ls_nested_http-resource  = <lv_resource>.
        ELSE.
          ls_nested_http-resource  = `MY_RES3`.
        ENDIF.

        ASSIGN COMPONENT 'INBOUNDDELIVERYHEADER' OF STRUCTURE <ls_input> TO FIELD-SYMBOL(<ls_inbounddeliveryheader>).
        IF sy-subrc = 0.
          ls_nested_http-inbounddeliveryheader = <ls_inbounddeliveryheader>.
        ENDIF.

        ASSIGN COMPONENT 'INBOUNDDELIVERYITEMS' OF STRUCTURE <ls_input> TO FIELD-SYMBOL(<ls_inbounddeliveryitems>).
        IF sy-subrc = 0.
          ls_nested_http-inbounddeliveryitems = <ls_inbounddeliveryitems>.
        ENDIF.

        ASSIGN er_first_tool_input->* TO <ls_nested_http>.
        IF sy-subrc <> 0.
          RAISE EXCEPTION NEW zpru_cx_agent_core( ).
        ENDIF.
        <ls_nested_http> = ls_nested_http.

      WHEN OTHERS.
        RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDCASE.

  ENDMETHOD.

  METHOD process_thinking.
    ev_langu = sy-langu.
    et_execution_plan = VALUE #( agentuuid = is_agent-agentuuid
                                 ( toolname = 'NESTED_ABAP'
                                   sequence = 1 )
                                 ( toolname = 'NESTED_LLM'
                                   sequence = 2 )
                                 ( toolname = 'NESTED_HTTP'
                                   sequence = 3 ) ).
  ENDMETHOD.

  METHOD read_data_4_thinking.
  ENDMETHOD.

  METHOD recall_memory.
  ENDMETHOD.

  METHOD set_final_response_content.

    DATA lt_key_value TYPE zpru_tt_key_value.
    DATA lo_util                 TYPE REF TO zpru_if_agent_util.
    DATA ls_final_output TYPE zpru_s_nested_agent_output.
    DATA lo_abap_datadescr TYPE REF TO cl_abap_datadescr.
    DATA lt_absolute_name TYPE string_table.
    DATA lt_key_value_condensed TYPE zpru_tt_key_value.
    DATA ls_key_value_source TYPE zpru_s_key_value.

    lo_util ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_AGENT_UTIL`
                                                        iv_context = zpru_if_agent_frw=>cs_context-standard ).


    lo_util->convert_to_abap(
      EXPORTING
        ir_string =    REF #( iv_last_output )
      CHANGING
        cr_abap   = lt_key_value ).


    LOOP AT lt_key_value ASSIGNING FIELD-SYMBOL(<lv_group>)
                         GROUP BY ( name = <lv_group>-name )
                         ASSIGNING FIELD-SYMBOL(<ls_group_key>).

      CLEAR: lt_absolute_name, lt_key_value_condensed, ls_key_value_source.
      LOOP AT GROUP <ls_group_key> ASSIGNING FIELD-SYMBOL(<ls_member>).
        APPEND INITIAL LINE TO lt_absolute_name ASSIGNING FIELD-SYMBOL(<lv_absolute_name>).
        <lv_absolute_name> = <ls_member>-type.
      ENDLOOP.

      LOOP AT lt_absolute_name ASSIGNING <lv_absolute_name>.
        lt_key_value_condensed = VALUE #( FOR <ls_in> IN lt_key_value
                                          WHERE ( name = <ls_group_key>-name AND
                                                  type = <lv_absolute_name> )
                                                  ( <ls_in> ) ).
        IF lt_key_value_condensed IS INITIAL.
          CONTINUE.
        ENDIF.

        SORT lt_key_value_condensed BY counter DESCENDING.
        ls_key_value_source = VALUE #( lt_key_value_condensed[ 1 ] OPTIONAL ).


        IF ls_key_value_source IS INITIAL.
          CONTINUE.
        ENDIF.

        ASSIGN COMPONENT ls_key_value_source-name OF STRUCTURE ls_final_output TO FIELD-SYMBOL(<lv_target_field>).
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.

        lo_abap_datadescr ?= cl_abap_datadescr=>describe_by_data( p_data = <lv_target_field> ).

        IF lo_abap_datadescr->absolute_name <> ls_key_value_source-type.
          CONTINUE.
        ENDIF.

        IF lo_abap_datadescr IS INSTANCE OF cl_abap_structdescr OR
           lo_abap_datadescr IS INSTANCE OF cl_abap_tabledescr.
          lo_util->convert_to_abap( EXPORTING ir_string = REF #( ls_key_value_source-value )
                                    CHANGING  cr_abap   = <lv_target_field> ).
        ELSE.
          <lv_target_field> = ls_key_value_source-value.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    lo_util->convert_to_string(
      EXPORTING
        ir_abap          = REF #( ls_final_output )
      CHANGING
        cr_string        = cs_final_response_body-responsecontent ).

  ENDMETHOD.

  METHOD set_final_response_metadata.
  ENDMETHOD.

  METHOD set_model_id.
    rv_model_id = `ST-GEMINI-3.0`.
  ENDMETHOD.

  METHOD set_result_comment.
    rv_result_comment = `Decision Engine processing is finished`.
  ENDMETHOD.
ENDCLASS.
