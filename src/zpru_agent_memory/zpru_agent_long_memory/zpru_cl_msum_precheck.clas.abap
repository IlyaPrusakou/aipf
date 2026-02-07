CLASS zpru_cl_msum_precheck DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zpru_if_agent_frw.
    INTERFACES zpru_if_msum_precheck.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zpru_cl_msum_precheck IMPLEMENTATION.
  METHOD zpru_if_msum_precheck~precheck_create_msum.
    DATA ls_line TYPE zpru_if_msum_crud=>ts_msum_create_imp.
    DATA lo_util TYPE REF TO zpru_if_agent_util.

    CLEAR et_entities.

    TRY.
        lo_util ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_AGENT_UTIL`
                                                            iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    LOOP AT it_msum_create_imp ASSIGNING FIELD-SYMBOL(<ls_create>).
      ls_line = <ls_create>.

      IF ls_line-summary_uuid IS INITIAL.
        APPEND INITIAL LINE TO cs_failed-msum ASSIGNING FIELD-SYMBOL(<ls_msum_failed>).
        <ls_msum_failed>-summary_uuid = ls_line-summary_uuid.
        <ls_msum_failed>-fail         = zpru_if_agent_frw=>cs_fail_cause-unspecific.
        <ls_msum_failed>-create       = abap_true.

        APPEND INITIAL LINE TO cs_reported-msum ASSIGNING FIELD-SYMBOL(<ls_msum_reported>).
        <ls_msum_reported>-summary_uuid = ls_line-summary_uuid.
        <ls_msum_reported>-create       = abap_true.
        <ls_msum_reported>-msg          = lo_util->new_message(
                                              iv_id       = zpru_if_agent_frw=>cs_message_class-zpru_msg_definition
                                              iv_number   = `001`
                                              iv_severity = zpru_if_agent_message=>sc_severity-error ).
        CONTINUE.
      ENDIF.

      lo_util->fill_flags( EXPORTING iv_name    = `ZPRU_IF_MSUM_CRUD=>TS_MSUM_CONTROL`
                           CHANGING  cs_data    = ls_line
                                     cs_control = ls_line-control ).

      APPEND INITIAL LINE TO et_entities ASSIGNING FIELD-SYMBOL(<ls_entity>).
      <ls_entity> = ls_line.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_msum_precheck~precheck_delete_msum.
    DATA ls_line TYPE zpru_if_msum_crud=>ts_msum_delete_imp.

    CLEAR et_entities.

    LOOP AT it_msum_delete_imp ASSIGNING FIELD-SYMBOL(<ls_delete>).
      ls_line = <ls_delete>.
      APPEND INITIAL LINE TO et_entities ASSIGNING FIELD-SYMBOL(<ls_entity>).
      <ls_entity> = ls_line.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_msum_precheck~precheck_read_msum.
    DATA ls_line TYPE zpru_if_msum_crud=>ts_msum_read_k.
    DATA lo_util TYPE REF TO zpru_if_agent_util.

    CLEAR et_entities.

    TRY.
        lo_util ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_AGENT_UTIL`
                                                            iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    LOOP AT it_msum_read_k ASSIGNING FIELD-SYMBOL(<ls_read>).
      ls_line = <ls_read>.
      lo_util->fill_flags( EXPORTING iv_name    = `ZPRU_IF_MSUM_CRUD=>TS_MSUM_CONTROL`
                           CHANGING  cs_data    = ls_line
                                     cs_control = ls_line-control ).

      APPEND INITIAL LINE TO et_entities ASSIGNING FIELD-SYMBOL(<ls_entity>).
      <ls_entity> = ls_line.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_msum_precheck~precheck_update_msum.
    DATA ls_line TYPE zpru_if_msum_crud=>ts_msum_update_imp.
    DATA lo_util TYPE REF TO zpru_if_agent_util.

    CLEAR et_entities.

    TRY.
        lo_util ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_AGENT_UTIL`
                                                            iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    LOOP AT it_msum_update_imp ASSIGNING FIELD-SYMBOL(<ls_update>).
      ls_line = <ls_update>.
      lo_util->fill_flags( EXPORTING iv_name    = `ZPRU_IF_MSUM_CRUD=>TS_MSUM_CONTROL`
                           CHANGING  cs_data    = ls_line
                                     cs_control = ls_line-control ).

      APPEND INITIAL LINE TO et_entities ASSIGNING FIELD-SYMBOL(<ls_entity>).
      <ls_entity> = ls_line.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
