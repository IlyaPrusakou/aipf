CLASS zpru_cl_agty_precheck DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zpru_if_agent_frw .
    INTERFACES zpru_if_agty_precheck .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zpru_cl_agty_precheck IMPLEMENTATION.
  METHOD zpru_if_agty_precheck~precheck_create_agent_type.
    DATA ls_line TYPE zpru_if_agty_crud=>ts_agty_create_imp.
    DATA lo_util TYPE REF TO zpru_if_agent_util.

    CLEAR et_entities.

    TRY.
        lo_util ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_AGENT_UTIL`
                                                            iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    LOOP AT it_agty_create_imp ASSIGNING FIELD-SYMBOL(<ls_create>).
      ls_line = <ls_create>.

      IF ls_line-agenttype IS INITIAL.
        APPEND INITIAL LINE TO cs_failed-agent_type ASSIGNING FIELD-SYMBOL(<ls_agent_failed>).
        <ls_agent_failed>-agenttype = ls_line-agenttype.
        <ls_agent_failed>-fail       = zpru_if_agent_frw=>cs_fail_cause-unspecific.
        <ls_agent_failed>-create     = abap_true.

        APPEND INITIAL LINE TO cs_reported-agent_type ASSIGNING FIELD-SYMBOL(<ls_agent_reported>).
        <ls_agent_reported>-agenttype = ls_line-agenttype.
        <ls_agent_reported>-create     = abap_true.
        <ls_agent_reported>-msg        = lo_util->new_message(
                                             iv_id       = zpru_if_agent_frw=>cs_message_class-zpru_msg_definition
                                             iv_number   = `001`
                                             iv_severity = zpru_if_agent_message=>sc_severity-error ).
        CONTINUE.
      ENDIF.

      lo_util->fill_flags( EXPORTING iv_name    = `ZPRU_IF_AGTY_CRUD=>TS_AGTY_CONTROL`
                           CHANGING  cs_data    = ls_line
                                     cs_control = ls_line-control ).

      APPEND INITIAL LINE TO et_entities ASSIGNING FIELD-SYMBOL(<ls_entity>).
      <ls_entity> = ls_line.
    ENDLOOP.
  ENDMETHOD.


  METHOD zpru_if_agty_precheck~precheck_delete_agent_type.
    DATA ls_line TYPE zpru_if_agty_crud=>ts_agty_delete_imp.

    CLEAR et_entities.

    LOOP AT it_agty_delete_imp ASSIGNING FIELD-SYMBOL(<ls_delete>).
      ls_line = <ls_delete>.
      APPEND INITIAL LINE TO et_entities ASSIGNING FIELD-SYMBOL(<ls_entity>).
      <ls_entity> = ls_line.
    ENDLOOP.
  ENDMETHOD.


  METHOD zpru_if_agty_precheck~precheck_read_agent_type.
    DATA ls_line TYPE zpru_if_agty_crud=>ts_agty_read_k.
    DATA lo_util TYPE REF TO zpru_if_agent_util.

    CLEAR et_entities.

    TRY.
        lo_util ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_AGENT_UTIL`
                                                            iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    LOOP AT it_agty_read_k ASSIGNING FIELD-SYMBOL(<ls_read>).
      ls_line = <ls_read>.
      lo_util->fill_flags( EXPORTING iv_name    = `ZPRU_IF_AGTY_CRUD=>TS_AGTY_CONTROL`
                           CHANGING  cs_data    = ls_line
                                     cs_control = ls_line-control ).

      APPEND INITIAL LINE TO et_entities ASSIGNING FIELD-SYMBOL(<ls_entity>).
      <ls_entity> = ls_line.
    ENDLOOP.
  ENDMETHOD.


  METHOD zpru_if_agty_precheck~precheck_update_agent_type.
    DATA ls_line TYPE zpru_if_agty_crud=>ts_agty_update_imp.
    DATA lo_util TYPE REF TO zpru_if_agent_util.

    CLEAR et_entities.

    TRY.
        lo_util ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_AGENT_UTIL`
                                                            iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    LOOP AT it_agty_update_imp ASSIGNING FIELD-SYMBOL(<ls_update>).
      ls_line = <ls_update>.
      lo_util->fill_flags( EXPORTING iv_name    = `ZPRU_IF_AGTY_CRUD=>TS_AGTY_CONTROL`
                           CHANGING  cs_data    = ls_line
                                     cs_control = ls_line-control ).

      APPEND INITIAL LINE TO et_entities ASSIGNING FIELD-SYMBOL(<ls_entity>).
      <ls_entity> = ls_line.
    ENDLOOP.
  ENDMETHOD.

ENDCLASS.
