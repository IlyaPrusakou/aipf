CLASS zpru_cl_adf_validator DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zpru_if_adf_validator.
ENDCLASS.


CLASS zpru_cl_adf_validator IMPLEMENTATION.
  METHOD zpru_if_adf_validator~check_agent_info.
    DATA lo_util        TYPE REF TO zpru_if_agent_util.
    DATA lo_adf_service TYPE REF TO zpru_if_adf_service.

    IF it_keys IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        lo_util ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_AGENT_UTIL`
                                                            iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    TRY.
        lo_adf_service ?= zpru_cl_agent_service_mngr=>get_service(
                              iv_service = `ZPRU_IF_ADF_SERVICE`
                              iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.

    ENDTRY.

    lo_adf_service->read_agent( EXPORTING it_agent_read_k = VALUE #( FOR <ls_k> IN it_keys
                                                                     ( agentuuid                = <ls_k>-agentuuid
                                                                       control-agentinfoprovider = abap_true ) )
                                IMPORTING et_agent        = DATA(lt_agent)
                                CHANGING  cs_reported     = cs_reported
                                          cs_failed       = cs_failed ).

    LOOP AT lt_agent ASSIGNING FIELD-SYMBOL(<ls_agent>) WHERE agentinfoprovider IS NOT INITIAL.

      zpru_if_adf_validator~validate_provider_cls( EXPORTING iv_class            = <ls_agent>-agentinfoprovider
                                                             iv_intf_2_be_impl_1 = 'ZPRU_IF_AGENT_INFO_PROVIDER'
                                                   IMPORTING ev_type_not_exist   = DATA(lv_type_not_exist)
                                                             ev_type_not_class   = DATA(lv_type_not_class)
                                                             ev_intf_not_impl_1  = DATA(lv_intf_not_impl_1)  ).

      IF lv_type_not_exist = abap_true.
        APPEND INITIAL LINE TO cs_failed-agent ASSIGNING FIELD-SYMBOL(<ls_agent_failed>).
        <ls_agent_failed>-agentuuid = <ls_agent>-agentuuid.
        <ls_agent_failed>-fail      = zpru_if_agent_frw=>cs_fail_cause-unspecific.

        APPEND VALUE #( agentuuid = <ls_agent>-agentuuid
                        msg       = lo_util->new_message(
                                        iv_id       = zpru_if_agent_frw=>cs_message_class-zpru_msg_definition
                                        iv_number   = `003`
                                        iv_severity = zpru_if_agent_message=>sc_severity-error
                                        iv_v1       = <ls_agent>-agentinfoprovider
                                        iv_v2       = <ls_agent>-agentname ) )
               TO cs_reported-agent.
        CONTINUE.
      ENDIF.

      IF lv_type_not_class = abap_true.
        APPEND INITIAL LINE TO cs_failed-agent ASSIGNING <ls_agent_failed>.
        <ls_agent_failed>-agentuuid = <ls_agent>-agentuuid.
        <ls_agent_failed>-fail      = zpru_if_agent_frw=>cs_fail_cause-unspecific.

        APPEND VALUE #( agentuuid = <ls_agent>-agentuuid
                        msg       = lo_util->new_message(
                                        iv_id       = zpru_if_agent_frw=>cs_message_class-zpru_msg_definition
                                        iv_number   = `005`
                                        iv_severity = zpru_if_agent_message=>sc_severity-error
                                        iv_v1       = <ls_agent>-agentinfoprovider ) )
               TO cs_reported-agent.
        CONTINUE.
      ENDIF.

      IF lv_intf_not_impl_1 = abap_true.
        APPEND INITIAL LINE TO cs_failed-agent ASSIGNING <ls_agent_failed>.
        <ls_agent_failed>-agentuuid = <ls_agent>-agentuuid.
        <ls_agent_failed>-fail      = zpru_if_agent_frw=>cs_fail_cause-unspecific.

        APPEND VALUE #( agentuuid = <ls_agent>-agentuuid
                        msg       = lo_util->new_message(
                                        iv_id       = zpru_if_agent_frw=>cs_message_class-zpru_msg_definition
                                        iv_number   = `004`
                                        iv_severity = zpru_if_agent_message=>sc_severity-error
                                        iv_v1       = <ls_agent>-agentinfoprovider
                                        iv_v2       = 'ZPRU_IF_AGENT_INFO_PROVIDER'
                                        iv_v3       = <ls_agent>-agentname ) )
               TO cs_reported-agent.

      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_adf_validator~check_decision_provider.
    DATA lo_util        TYPE REF TO zpru_if_agent_util.
    DATA lo_adf_service TYPE REF TO zpru_if_adf_service.

    IF it_keys IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        lo_util ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_AGENT_UTIL`
                                                            iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    TRY.
        lo_adf_service ?= zpru_cl_agent_service_mngr=>get_service(
                              iv_service = `ZPRU_IF_ADF_SERVICE`
                              iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.

    ENDTRY.

    lo_adf_service->read_agent( EXPORTING it_agent_read_k = VALUE #( FOR <ls_k> IN it_keys
                                                                     ( agentuuid               = <ls_k>-agentuuid
                                                                       control-decisionprovider = abap_true ) )
                                IMPORTING et_agent        = DATA(lt_agent)
                                CHANGING  cs_reported     = cs_reported
                                          cs_failed       = cs_failed ).

    LOOP AT lt_agent ASSIGNING FIELD-SYMBOL(<ls_agent>).

      IF <ls_agent>-decisionprovider IS INITIAL.
        APPEND INITIAL LINE TO cs_failed-agent ASSIGNING FIELD-SYMBOL(<ls_agent_failed>).
        <ls_agent_failed>-agentuuid = <ls_agent>-agentuuid.
        <ls_agent_failed>-fail      = zpru_if_agent_frw=>cs_fail_cause-unspecific.

        APPEND VALUE #( agentuuid = <ls_agent>-agentuuid
                        msg       = lo_util->new_message(
                                        iv_id       = zpru_if_agent_frw=>cs_message_class-zpru_msg_definition
                                        iv_number   = `002`
                                        iv_severity = zpru_if_agent_message=>sc_severity-error
                                        iv_v1       = <ls_agent>-agentname ) )
               TO cs_reported-agent.
        CONTINUE.
      ENDIF.

      zpru_if_adf_validator~validate_provider_cls( EXPORTING iv_class            = <ls_agent>-decisionprovider
                                                             iv_intf_2_be_impl_1 = 'ZPRU_IF_DECISION_PROVIDER'
                                                   IMPORTING ev_type_not_exist   = DATA(lv_type_not_exist)
                                                             ev_type_not_class   = DATA(lv_type_not_class)
                                                             ev_intf_not_impl_1  = DATA(lv_intf_not_impl_1)  ).

      IF lv_type_not_exist = abap_true.
        APPEND INITIAL LINE TO cs_failed-agent ASSIGNING <ls_agent_failed>.
        <ls_agent_failed>-agentuuid = <ls_agent>-agentuuid.
        <ls_agent_failed>-fail      = zpru_if_agent_frw=>cs_fail_cause-unspecific.

        APPEND VALUE #( agentuuid = <ls_agent>-agentuuid
                        msg       = lo_util->new_message(
                                        iv_id       = zpru_if_agent_frw=>cs_message_class-zpru_msg_definition
                                        iv_number   = `003`
                                        iv_severity = zpru_if_agent_message=>sc_severity-error
                                        iv_v1       = <ls_agent>-decisionprovider
                                        iv_v2       = <ls_agent>-agentname ) )
               TO cs_reported-agent.
        CONTINUE.
      ENDIF.

      IF lv_type_not_class = abap_true.
        APPEND INITIAL LINE TO cs_failed-agent ASSIGNING <ls_agent_failed>.
        <ls_agent_failed>-agentuuid = <ls_agent>-agentuuid.
        <ls_agent_failed>-fail      = zpru_if_agent_frw=>cs_fail_cause-unspecific.

        APPEND VALUE #( agentuuid = <ls_agent>-agentuuid
                        msg       = lo_util->new_message(
                                        iv_id       = zpru_if_agent_frw=>cs_message_class-zpru_msg_definition
                                        iv_number   = `005`
                                        iv_severity = zpru_if_agent_message=>sc_severity-error
                                        iv_v1       = <ls_agent>-decisionprovider ) )
               TO cs_reported-agent.
        CONTINUE.
      ENDIF.

      IF lv_intf_not_impl_1 = abap_true.
        APPEND INITIAL LINE TO cs_failed-agent ASSIGNING <ls_agent_failed>.
        <ls_agent_failed>-agentuuid = <ls_agent>-agentuuid.
        <ls_agent_failed>-fail      = zpru_if_agent_frw=>cs_fail_cause-unspecific.

        APPEND VALUE #( agentuuid = <ls_agent>-agentuuid
                        msg       = lo_util->new_message(
                                        iv_id       = zpru_if_agent_frw=>cs_message_class-zpru_msg_definition
                                        iv_number   = `004`
                                        iv_severity = zpru_if_agent_message=>sc_severity-error
                                        iv_v1       = <ls_agent>-decisionprovider
                                        iv_v2       = 'ZPRU_IF_DECISION_PROVIDER'
                                        iv_v3       = <ls_agent>-agentname ) )
               TO cs_reported-agent.

      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_adf_validator~check_long_memory.
    DATA lo_util        TYPE REF TO zpru_if_agent_util.
    DATA lo_adf_service TYPE REF TO zpru_if_adf_service.

    IF it_keys IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        lo_util ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_AGENT_UTIL`
                                                            iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    TRY.
        lo_adf_service ?= zpru_cl_agent_service_mngr=>get_service(
                              iv_service = `ZPRU_IF_ADF_SERVICE`
                              iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.

    ENDTRY.

    lo_adf_service->read_agent( EXPORTING it_agent_read_k = VALUE #( FOR <ls_k> IN it_keys
                                                                     ( agentuuid                 = <ls_k>-agentuuid
                                                                       control-longmemoryprovider = abap_true ) )
                                IMPORTING et_agent        = DATA(lt_agent)
                                CHANGING  cs_reported     = cs_reported
                                          cs_failed       = cs_failed ).

    LOOP AT lt_agent ASSIGNING FIELD-SYMBOL(<ls_agent>).

      IF <ls_agent>-longmemoryprovider IS INITIAL.
        APPEND INITIAL LINE TO cs_failed-agent ASSIGNING FIELD-SYMBOL(<ls_agent_failed>).
        <ls_agent_failed>-agentuuid = <ls_agent>-agentuuid.
        <ls_agent_failed>-fail      = zpru_if_agent_frw=>cs_fail_cause-unspecific.

        APPEND VALUE #( agentuuid = <ls_agent>-agentuuid
                        msg       = lo_util->new_message(
                                        iv_id       = zpru_if_agent_frw=>cs_message_class-zpru_msg_definition
                                        iv_number   = `007`
                                        iv_severity = zpru_if_agent_message=>sc_severity-error
                                        iv_v1       = <ls_agent>-agentname ) )
               TO cs_reported-agent.
        CONTINUE.
      ENDIF.

      zpru_if_adf_validator~validate_provider_cls( EXPORTING iv_class            = <ls_agent>-longmemoryprovider
                                                             iv_intf_2_be_impl_1 = 'ZPRU_IF_LONG_MEMORY_PROVIDER'
                                                   IMPORTING ev_type_not_exist   = DATA(lv_type_not_exist)
                                                             ev_type_not_class   = DATA(lv_type_not_class)
                                                             ev_intf_not_impl_1  = DATA(lv_intf_not_impl_1)  ).

      IF lv_type_not_exist = abap_true.
        APPEND INITIAL LINE TO cs_failed-agent ASSIGNING <ls_agent_failed>.
        <ls_agent_failed>-agentuuid = <ls_agent>-agentuuid.
        <ls_agent_failed>-fail      = zpru_if_agent_frw=>cs_fail_cause-unspecific.

        APPEND VALUE #( agentuuid = <ls_agent>-agentuuid
                        msg       = lo_util->new_message(
                                        iv_id       = zpru_if_agent_frw=>cs_message_class-zpru_msg_definition
                                        iv_number   = `003`
                                        iv_severity = zpru_if_agent_message=>sc_severity-error
                                        iv_v1       = <ls_agent>-longmemoryprovider
                                        iv_v2       = <ls_agent>-agentname ) )
               TO cs_reported-agent.
        CONTINUE.
      ENDIF.

      IF lv_type_not_class = abap_true.
        APPEND INITIAL LINE TO cs_failed-agent ASSIGNING <ls_agent_failed>.
        <ls_agent_failed>-agentuuid = <ls_agent>-agentuuid.
        <ls_agent_failed>-fail      = zpru_if_agent_frw=>cs_fail_cause-unspecific.

        APPEND VALUE #( agentuuid = <ls_agent>-agentuuid
                        msg       = lo_util->new_message(
                                        iv_id       = zpru_if_agent_frw=>cs_message_class-zpru_msg_definition
                                        iv_number   = `005`
                                        iv_severity = zpru_if_agent_message=>sc_severity-error
                                        iv_v1       = <ls_agent>-longmemoryprovider ) )
               TO cs_reported-agent.
        CONTINUE.
      ENDIF.

      IF lv_intf_not_impl_1 = abap_true.
        APPEND INITIAL LINE TO cs_failed-agent ASSIGNING <ls_agent_failed>.
        <ls_agent_failed>-agentuuid = <ls_agent>-agentuuid.
        <ls_agent_failed>-fail      = zpru_if_agent_frw=>cs_fail_cause-unspecific.

        APPEND VALUE #( agentuuid = <ls_agent>-agentuuid
                        msg       = lo_util->new_message(
                                        iv_id       = zpru_if_agent_frw=>cs_message_class-zpru_msg_definition
                                        iv_number   = `004`
                                        iv_severity = zpru_if_agent_message=>sc_severity-error
                                        iv_v1       = <ls_agent>-longmemoryprovider
                                        iv_v2       = 'ZPRU_IF_LONG_MEMORY_PROVIDER'
                                        iv_v3       = <ls_agent>-agentname ) )
               TO cs_reported-agent.

      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_adf_validator~check_short_memory.
    DATA lo_util        TYPE REF TO zpru_if_agent_util.
    DATA lo_adf_service TYPE REF TO zpru_if_adf_service.

    IF it_keys IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        lo_util ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_AGENT_UTIL`
                                                            iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    TRY.
        lo_adf_service ?= zpru_cl_agent_service_mngr=>get_service(
                              iv_service = `ZPRU_IF_ADF_SERVICE`
                              iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.

    ENDTRY.

    lo_adf_service->read_agent( EXPORTING it_agent_read_k = VALUE #( FOR <ls_k> IN it_keys
                                                                     ( agentuuid                  = <ls_k>-agentuuid
                                                                       control-shortmemoryprovider = abap_true ) )
                                IMPORTING et_agent        = DATA(lt_agent)
                                CHANGING  cs_reported     = cs_reported
                                          cs_failed       = cs_failed ).

    LOOP AT lt_agent ASSIGNING FIELD-SYMBOL(<ls_agent>).

      IF <ls_agent>-shortmemoryprovider IS INITIAL.
        APPEND INITIAL LINE TO cs_failed-agent ASSIGNING FIELD-SYMBOL(<ls_agent_failed>).
        <ls_agent_failed>-agentuuid = <ls_agent>-agentuuid.
        <ls_agent_failed>-fail      = zpru_if_agent_frw=>cs_fail_cause-unspecific.

        APPEND VALUE #( agentuuid = <ls_agent>-agentuuid
                        msg       = lo_util->new_message(
                                        iv_id       = zpru_if_agent_frw=>cs_message_class-zpru_msg_definition
                                        iv_number   = `006`
                                        iv_severity = zpru_if_agent_message=>sc_severity-error
                                        iv_v1       = <ls_agent>-agentname ) )
               TO cs_reported-agent.
        CONTINUE.
      ENDIF.

      zpru_if_adf_validator~validate_provider_cls( EXPORTING iv_class            = <ls_agent>-shortmemoryprovider
                                                             iv_intf_2_be_impl_1 = 'ZPRU_IF_SHORT_MEMORY_PROVIDER'
                                                   IMPORTING ev_type_not_exist   = DATA(lv_type_not_exist)
                                                             ev_type_not_class   = DATA(lv_type_not_class)
                                                             ev_intf_not_impl_1  = DATA(lv_intf_not_impl_1)  ).

      IF lv_type_not_exist = abap_true.
        APPEND INITIAL LINE TO cs_failed-agent ASSIGNING <ls_agent_failed>.
        <ls_agent_failed>-agentuuid = <ls_agent>-agentuuid.
        <ls_agent_failed>-fail      = zpru_if_agent_frw=>cs_fail_cause-unspecific.

        APPEND VALUE #( agentuuid = <ls_agent>-agentuuid
                        msg       = lo_util->new_message(
                                        iv_id       = zpru_if_agent_frw=>cs_message_class-zpru_msg_definition
                                        iv_number   = `003`
                                        iv_severity = zpru_if_agent_message=>sc_severity-error
                                        iv_v1       = <ls_agent>-shortmemoryprovider
                                        iv_v2       = <ls_agent>-agentname ) )
               TO cs_reported-agent.
        CONTINUE.
      ENDIF.

      IF lv_type_not_class = abap_true.
        APPEND INITIAL LINE TO cs_failed-agent ASSIGNING <ls_agent_failed>.
        <ls_agent_failed>-agentuuid = <ls_agent>-agentuuid.
        <ls_agent_failed>-fail      = zpru_if_agent_frw=>cs_fail_cause-unspecific.

        APPEND VALUE #( agentuuid = <ls_agent>-agentuuid
                        msg       = lo_util->new_message(
                                        iv_id       = zpru_if_agent_frw=>cs_message_class-zpru_msg_definition
                                        iv_number   = `005`
                                        iv_severity = zpru_if_agent_message=>sc_severity-error
                                        iv_v1       = <ls_agent>-shortmemoryprovider ) )
               TO cs_reported-agent.
        CONTINUE.
      ENDIF.

      IF lv_intf_not_impl_1 = abap_true.
        APPEND INITIAL LINE TO cs_failed-agent ASSIGNING <ls_agent_failed>.
        <ls_agent_failed>-agentuuid = <ls_agent>-agentuuid.
        <ls_agent_failed>-fail      = zpru_if_agent_frw=>cs_fail_cause-unspecific.

        APPEND VALUE #( agentuuid = <ls_agent>-agentuuid
                        msg       = lo_util->new_message(
                                        iv_id       = zpru_if_agent_frw=>cs_message_class-zpru_msg_definition
                                        iv_number   = `004`
                                        iv_severity = zpru_if_agent_message=>sc_severity-error
                                        iv_v1       = <ls_agent>-shortmemoryprovider
                                        iv_v2       = 'ZPRU_IF_SHORT_MEMORY_PROVIDER'
                                        iv_v3       = <ls_agent>-agentname ) )
               TO cs_reported-agent.

      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_adf_validator~check_tool_provider.
  ENDMETHOD.

  METHOD zpru_if_adf_validator~validate_provider_cls.
    DATA lo_type_descr       TYPE REF TO cl_abap_typedescr.
    DATA lo_abap_objectdescr TYPE REF TO cl_abap_objectdescr.

    ev_type_not_exist = abap_false.
    ev_type_not_class = abap_false.
    ev_intf_not_impl_1 = abap_false.
    ev_intf_not_impl_2 = abap_false.

    cl_abap_typedescr=>describe_by_name( EXPORTING  p_name         = iv_class
                                         RECEIVING  p_descr_ref    = lo_type_descr
                                         EXCEPTIONS type_not_found = 1
                                                    OTHERS         = 99  ).
    IF sy-subrc <> 0.
      ev_type_not_exist = abap_true.
      RETURN.
    ENDIF.

    TRY.
        lo_abap_objectdescr = CAST cl_abap_objectdescr( lo_type_descr ).
      CATCH cx_sy_move_cast_error.
        ev_type_not_class = abap_true.
        RETURN.
    ENDTRY.

    IF iv_intf_2_be_impl_1 IS NOT INITIAL.
      lo_abap_objectdescr->get_interface_type( EXPORTING  p_name              = iv_intf_2_be_impl_1
                                               EXCEPTIONS interface_not_found = 1
                                                          OTHERS              = 2 ).
      IF sy-subrc <> 0.
        ev_intf_not_impl_1 = abap_true.
      ENDIF.
    ENDIF.

    IF ev_intf_not_impl_2 IS NOT INITIAL.
      lo_abap_objectdescr->get_interface_type( EXPORTING  p_name              = ev_intf_not_impl_2
                                               EXCEPTIONS interface_not_found = 1
                                                          OTHERS              = 2 ).
      IF sy-subrc <> 0.
        ev_intf_not_impl_2 = abap_true.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD zpru_if_adf_validator~check_info_provider.
  ENDMETHOD.

  METHOD zpru_if_adf_validator~check_tool_schema_provider.
  ENDMETHOD.
ENDCLASS.
