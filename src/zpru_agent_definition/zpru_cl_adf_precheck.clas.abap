CLASS zpru_cl_adf_precheck DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zpru_if_agent_frw.
    INTERFACES zpru_if_adf_precheck.
ENDCLASS.


CLASS zpru_cl_adf_precheck IMPLEMENTATION.
  METHOD zpru_if_adf_precheck~precheck_create_agent.
    DATA ls_line TYPE zpru_if_adf_type_and_constant=>ts_agent_create_imp.
    DATA lo_util TYPE REF TO zpru_if_agent_util.

    CLEAR et_entities.

    TRY.
        lo_util ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_AGENT_UTIL`
                                                            iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    LOOP AT it_agent_create_imp ASSIGNING FIELD-SYMBOL(<ls_create>).
      ls_line = <ls_create>.
      IF ls_line-agent_uuid IS INITIAL.
        TRY.
            ls_line-agent_uuid = cl_system_uuid=>create_uuid_x16_static( ).
          CATCH cx_uuid_error.
            RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
        ENDTRY.
      ENDIF.

      IF ls_line-agent_name IS INITIAL.
        APPEND INITIAL LINE TO cs_failed-agent ASSIGNING FIELD-SYMBOL(<ls_agent_failed>).
        <ls_agent_failed>-agent_uuid = ls_line-agent_uuid.
        <ls_agent_failed>-fail       = zpru_if_agent_frw=>cs_fail_cause-unspecific.
        <ls_agent_failed>-create     = abap_true.

        APPEND INITIAL LINE TO cs_reported-agent ASSIGNING FIELD-SYMBOL(<ls_agent_reported>).
        <ls_agent_reported>-agent_uuid = ls_line-agent_uuid.
        <ls_agent_reported>-create     = abap_true.
        <ls_agent_reported>-msg        = lo_util->new_message(
                                             iv_id       = zpru_if_agent_frw=>cs_message_class-zpru_msg_definition
                                             iv_number   = `001`
                                             iv_severity = zpru_if_agent_message=>sc_severity-error ).
        CONTINUE.
      ENDIF.

      lo_util->fill_flags( EXPORTING iv_name    = `ZPRU_IF_ADF_TYPE_AND_CONSTANT=>TS_AGENT_CONTROL`
                           CHANGING  cs_data    = ls_line
                                     cs_control = ls_line-control ).

      APPEND INITIAL LINE TO et_entities ASSIGNING FIELD-SYMBOL(<ls_entity>).
      <ls_entity> = ls_line.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_adf_precheck~precheck_update_agent.
    DATA ls_line TYPE zpru_if_adf_type_and_constant=>ts_agent_update_imp.
    DATA lo_util TYPE REF TO zpru_if_agent_util.

    CLEAR et_entities.

    TRY.
        lo_util ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_AGENT_UTIL`
                                                            iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    LOOP AT it_agent_update_imp ASSIGNING FIELD-SYMBOL(<ls_update>).
      ls_line = <ls_update>.
      lo_util->fill_flags( EXPORTING iv_name    = `ZPRU_IF_ADF_TYPE_AND_CONSTANT=>TS_AGENT_CONTROL`
                           CHANGING  cs_data    = ls_line
                                     cs_control = ls_line-control ).

      APPEND INITIAL LINE TO et_entities ASSIGNING FIELD-SYMBOL(<ls_entity>).
      <ls_entity> = ls_line.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_adf_precheck~precheck_delete_agent.
    DATA ls_line TYPE zpru_if_adf_type_and_constant=>ts_agent_delete_imp.

    CLEAR et_entities.

    LOOP AT it_agent_delete_imp ASSIGNING FIELD-SYMBOL(<ls_delete>).
      ls_line = <ls_delete>.
      APPEND INITIAL LINE TO et_entities ASSIGNING FIELD-SYMBOL(<ls_entity>).
      <ls_entity> = ls_line.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_adf_precheck~precheck_read_agent.
    DATA ls_line TYPE zpru_if_adf_type_and_constant=>ts_agent_read_k.
    DATA lo_util TYPE REF TO zpru_if_agent_util.

    CLEAR et_entities.

    TRY.
        lo_util ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_AGENT_UTIL`
                                                            iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    LOOP AT it_agent_read_k ASSIGNING FIELD-SYMBOL(<ls_read>).
      ls_line = <ls_read>.
      lo_util->fill_flags( EXPORTING iv_name    = `ZPRU_IF_ADF_TYPE_AND_CONSTANT=>TS_AGENT_CONTROL`
                           CHANGING  cs_data    = ls_line
                                     cs_control = ls_line-control ).

      APPEND INITIAL LINE TO et_entities ASSIGNING FIELD-SYMBOL(<ls_entity>).
      <ls_entity> = ls_line.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_adf_precheck~precheck_cba_tool.
    DATA ls_line TYPE zpru_if_adf_type_and_constant=>ts_tool_create_imp.
    DATA lo_util TYPE REF TO zpru_if_agent_util.

    CLEAR et_entities.

    TRY.
        lo_util ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_AGENT_UTIL`
                                                            iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    LOOP AT it_tool_create_imp ASSIGNING FIELD-SYMBOL(<ls_create>).
      ls_line = <ls_create>.
      IF ls_line-tool_uuid IS INITIAL.
        TRY.
            ls_line-tool_uuid = cl_system_uuid=>create_uuid_x16_static( ).
          CATCH cx_uuid_error.
            RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
        ENDTRY.
      ENDIF.

      lo_util->fill_flags( EXPORTING iv_name    = `ZPRU_IF_ADF_TYPE_AND_CONSTANT=>TS_TOOL_CONTROL`
                           CHANGING  cs_data    = ls_line
                                     cs_control = ls_line-control ).

      IF ls_line-agent_uuid IS INITIAL.
        APPEND VALUE #( agent_uuid = ls_line-agent_uuid
                        tool_uuid  = ls_line-tool_uuid
                        create     = abap_true
                        fail       = zpru_if_agent_frw=>cs_fail_cause-dependency )
               TO cs_failed-tool.

        APPEND VALUE #( agent_uuid = ls_line-agent_uuid
                        tool_uuid  = ls_line-tool_uuid
                        create     = abap_true
                        msg        = lo_util->new_message(
                                         iv_id       = zpru_if_agent_frw=>cs_message_class-zpru_msg_execution
                                         iv_number   = `011` " Assumed message for missing parent key
                                         iv_severity = zpru_if_agent_message=>sc_severity-error ) )
               TO cs_reported-tool.
        CONTINUE.
      ENDIF.

      APPEND INITIAL LINE TO et_entities ASSIGNING FIELD-SYMBOL(<ls_entity>).
      <ls_entity> = ls_line.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_adf_precheck~precheck_update_tool.
    DATA ls_line TYPE zpru_if_adf_type_and_constant=>ts_tool_update_imp.
    DATA lo_util TYPE REF TO zpru_if_agent_util.

    CLEAR et_entities.

    TRY.
        lo_util ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_AGENT_UTIL`
                                                            iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    LOOP AT it_tool_update_imp ASSIGNING FIELD-SYMBOL(<ls_update>).
      ls_line = <ls_update>.
      lo_util->fill_flags( EXPORTING iv_name    = `ZPRU_IF_ADF_TYPE_AND_CONSTANT=>TS_TOOL_CONTROL`
                           CHANGING  cs_data    = ls_line
                                     cs_control = ls_line-control ).

      APPEND INITIAL LINE TO et_entities ASSIGNING FIELD-SYMBOL(<ls_entity>).
      <ls_entity> = ls_line.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_adf_precheck~precheck_delete_tool.
    DATA ls_line TYPE zpru_if_adf_type_and_constant=>ts_tool_delete_imp.

    CLEAR et_entities.

    LOOP AT it_tool_delete_imp ASSIGNING FIELD-SYMBOL(<ls_delete>).
      ls_line = <ls_delete>.
      APPEND INITIAL LINE TO et_entities ASSIGNING FIELD-SYMBOL(<ls_entity>).
      <ls_entity> = ls_line.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_adf_precheck~precheck_read_tool.
    DATA ls_line TYPE zpru_if_adf_type_and_constant=>ts_tool_read_k.
    DATA lo_util TYPE REF TO zpru_if_agent_util.

    CLEAR et_entities.

    TRY.
        lo_util ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_AGENT_UTIL`
                                                            iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    LOOP AT it_tool_read_k ASSIGNING FIELD-SYMBOL(<ls_read>).
      ls_line = <ls_read>.
      lo_util->fill_flags( EXPORTING iv_name    = `ZPRU_IF_ADF_TYPE_AND_CONSTANT=>TS_TOOL_CONTROL`
                           CHANGING  cs_data    = ls_line
                                     cs_control = ls_line-control ).

      APPEND INITIAL LINE TO et_entities ASSIGNING FIELD-SYMBOL(<ls_entity>).
      <ls_entity> = ls_line.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_adf_precheck~precheck_rba_tool.
    DATA ls_line TYPE zpru_if_adf_type_and_constant=>ts_rba_tool_k.
    DATA lo_util TYPE REF TO zpru_if_agent_util.

    CLEAR et_entities.

    TRY.
        lo_util ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_AGENT_UTIL`
                                                            iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    LOOP AT it_rba_tool_k ASSIGNING FIELD-SYMBOL(<ls_rba>).
      ls_line = <ls_rba>.
      lo_util->fill_flags( EXPORTING iv_name    = `ZPRU_IF_ADF_TYPE_AND_CONSTANT=>TS_TOOL_CONTROL`
                           CHANGING  cs_data    = ls_line
                                     cs_control = ls_line-control ).

      APPEND INITIAL LINE TO et_entities ASSIGNING FIELD-SYMBOL(<ls_entity>).
      <ls_entity> = ls_line.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
