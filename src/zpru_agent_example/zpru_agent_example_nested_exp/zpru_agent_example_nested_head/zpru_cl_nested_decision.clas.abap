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
    DATA ls_nested_prompt    TYPE zpru_s_nested_agent_input.
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


    lo_util->convert_to_abap(
      EXPORTING
        ir_string = REF #( lv_input )
      CHANGING
        cr_abap   = ls_nested_prompt ).


    CASE is_first_tool-toolname.

      WHEN 'NESTED_ABAP'.
        ls_nested_abap-warehouse = ls_nested_prompt-warehouse.
        ls_nested_abap-storagebin = ls_nested_prompt-storagebin.
        ASSIGN er_first_tool_input->* TO <ls_nested_abap>.
        IF sy-subrc <> 0.
          RAISE EXCEPTION NEW zpru_cx_agent_core( ).
        ENDIF.
        <ls_nested_abap> = ls_nested_abap.

      WHEN 'NESTED_LLM'.
        ls_nested_llm-warehouse = ls_nested_prompt-warehouse.
        ls_nested_llm-storagebin = ls_nested_prompt-storagebin.
        ASSIGN er_first_tool_input->* TO <ls_nested_llm>.
        IF sy-subrc <> 0.
          RAISE EXCEPTION NEW zpru_cx_agent_core( ).
        ENDIF.
        <ls_nested_llm> = ls_nested_llm.

      WHEN 'NESTED_HTTP'.
        ls_nested_http-warehouse = ls_nested_prompt-warehouse.
        ls_nested_http-resource = `MY_RES3`.
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
    IF iv_last_output <> `Nested http has played`.
      RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDIF.

    cs_final_response_body-responsecontent = `Nested final response`.
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
