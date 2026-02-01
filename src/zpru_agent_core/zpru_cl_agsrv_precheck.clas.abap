CLASS zpru_cl_agsrv_precheck DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zpru_if_agent_frw.
    INTERFACES zpru_if_agsrv_precheck.
ENDCLASS.


CLASS zpru_cl_agsrv_precheck IMPLEMENTATION.
  METHOD zpru_if_agsrv_precheck~precheck_create_agent_service.
    DATA ls_line TYPE zpru_if_agsrv_crud=>ts_agsrv_create_imp.
    DATA lo_util TYPE REF TO zpru_if_agent_util.

    CLEAR et_entities.

    TRY.
        lo_util ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_AGENT_UTIL`
                                                            iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    LOOP AT it_agsrv_create_imp ASSIGNING FIELD-SYMBOL(<ls_create>).
      ls_line = <ls_create>.

      IF ls_line-service IS INITIAL OR ls_line-context IS INITIAL.
        APPEND INITIAL LINE TO cs_failed-agsrv ASSIGNING FIELD-SYMBOL(<ls_failed>).
        <ls_failed>-service = ls_line-service.
        <ls_failed>-context = ls_line-context.
        <ls_failed>-fail    = zpru_if_agent_frw=>cs_fail_cause-unspecific.
        <ls_failed>-create  = abap_true.

        APPEND INITIAL LINE TO cs_reported-agsrv ASSIGNING FIELD-SYMBOL(<ls_reported>).
        <ls_reported>-service = ls_line-service.
        <ls_reported>-context = ls_line-context.
        <ls_reported>-create  = abap_true.
        <ls_reported>-msg     = lo_util->new_message(
                                    iv_id       = zpru_if_agent_frw=>cs_message_class-zpru_msg_definition
                                    iv_number   = `001`
                                    iv_severity = zpru_if_agent_message=>sc_severity-error ).
        CONTINUE.
      ENDIF.

      lo_util->fill_flags( EXPORTING iv_name    = `ZPRU_IF_AGSRV_CRUD=>TS_AGSRV_CONTROL`
                           CHANGING  cs_data    = ls_line
                                     cs_control = ls_line-control ).

      APPEND ls_line TO et_entities.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_agsrv_precheck~precheck_update_agent_service.
    DATA ls_line TYPE zpru_if_agsrv_crud=>ts_agsrv_update_imp.
    DATA lo_util TYPE REF TO zpru_if_agent_util.

    CLEAR et_entities.

    TRY.
        lo_util ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_AGENT_UTIL`
                                                            iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    LOOP AT it_agsrv_update_imp ASSIGNING FIELD-SYMBOL(<ls_update>).
      ls_line = <ls_update>.

      lo_util->fill_flags( EXPORTING iv_name    = `ZPRU_IF_AGSRV_CRUD=>TS_AGSRV_CONTROL`
                           CHANGING  cs_data    = ls_line
                                     cs_control = ls_line-control ).

      APPEND ls_line TO et_entities.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_agsrv_precheck~precheck_delete_agent_service.
    CLEAR et_entities.
    et_entities = it_agsrv_delete_imp.
  ENDMETHOD.

  METHOD zpru_if_agsrv_precheck~precheck_read_agent_service.
    DATA ls_line TYPE zpru_if_agsrv_crud=>ts_agsrv_read_k.
    DATA lo_util TYPE REF TO zpru_if_agent_util.

    CLEAR et_entities.

    TRY.
        lo_util ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_AGENT_UTIL`
                                                            iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    LOOP AT it_agsrv_read_k ASSIGNING FIELD-SYMBOL(<ls_read>).
      ls_line = <ls_read>.

      lo_util->fill_flags( EXPORTING iv_name    = `ZPRU_IF_AGSRV_CRUD=>TS_AGSRV_CONTROL`
                           CHANGING  cs_data    = ls_line
                                     cs_control = ls_line-control ).

      APPEND ls_line TO et_entities.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
