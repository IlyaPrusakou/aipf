CLASS zpru_cl_adf_service DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zpru_if_agent_frw.
    INTERFACES zpru_if_adf_service.

  PROTECTED SECTION.
    TYPES tt_agent      TYPE STANDARD TABLE OF zpru_s_agent WITH EMPTY KEY.
    TYPES tt_agent_tool TYPE STANDARD TABLE OF zpru_s_agent_tool WITH EMPTY KEY.

    METHODS calculate_triggers
      EXPORTING et_check_decision_provider_v TYPE zpru_if_adf_type_and_constant=>tt_agent_read_k
                et_check_short_memory_v      TYPE zpru_if_adf_type_and_constant=>tt_agent_read_k
                et_check_long_memory_v       TYPE zpru_if_adf_type_and_constant=>tt_agent_read_k
                et_check_agent_info_v        TYPE zpru_if_adf_type_and_constant=>tt_agent_read_k
      CHANGING  cs_reported                  TYPE zpru_if_agent_frw=>ts_adf_reported OPTIONAL
                cs_failed                    TYPE zpru_if_agent_frw=>ts_adf_failed   OPTIONAL.

    METHODS db_modify
      IMPORTING iv_do_commit    TYPE abap_boolean
      CHANGING  cs_reported     TYPE zpru_if_agent_frw=>ts_adf_reported OPTIONAL
                cs_failed       TYPE zpru_if_agent_frw=>ts_adf_failed   OPTIONAL
                cs_mapped       TYPE zpru_if_agent_frw=>ts_adf_mapped   OPTIONAL
      RETURNING VALUE(rv_error) TYPE abap_bool.

    METHODS precheck_create_agent
      IMPORTING it_agent_create_imp TYPE zpru_if_adf_type_and_constant=>tt_agent_create_imp
      EXPORTING et_entities         TYPE zpru_if_adf_type_and_constant=>tt_agent_create_imp
      CHANGING  cs_reported         TYPE zpru_if_agent_frw=>ts_adf_reported OPTIONAL
                cs_failed           TYPE zpru_if_agent_frw=>ts_adf_failed   OPTIONAL.

    METHODS precheck_update_agent
      IMPORTING it_agent_update_imp TYPE zpru_if_adf_type_and_constant=>tt_agent_update_imp
      EXPORTING et_entities         TYPE zpru_if_adf_type_and_constant=>tt_agent_update_imp
      CHANGING  cs_reported         TYPE zpru_if_agent_frw=>ts_adf_reported OPTIONAL
                cs_failed           TYPE zpru_if_agent_frw=>ts_adf_failed   OPTIONAL.

    METHODS precheck_delete_agent
      IMPORTING it_agent_delete_imp TYPE zpru_if_adf_type_and_constant=>tt_agent_delete_imp
      EXPORTING et_entities         TYPE zpru_if_adf_type_and_constant=>tt_agent_delete_imp
      CHANGING  cs_reported         TYPE zpru_if_agent_frw=>ts_adf_reported OPTIONAL
                cs_failed           TYPE zpru_if_agent_frw=>ts_adf_failed   OPTIONAL.

    METHODS precheck_read_agent
      IMPORTING it_agent_read_k TYPE zpru_if_adf_type_and_constant=>tt_agent_read_k
      EXPORTING et_entities     TYPE zpru_if_adf_type_and_constant=>tt_agent_read_k
      CHANGING  cs_reported     TYPE zpru_if_agent_frw=>ts_adf_reported OPTIONAL
                cs_failed       TYPE zpru_if_agent_frw=>ts_adf_failed   OPTIONAL.

    METHODS precheck_cba_tool
      IMPORTING it_tool_create_imp TYPE zpru_if_adf_type_and_constant=>tt_tool_create_imp
      EXPORTING et_entities        TYPE zpru_if_adf_type_and_constant=>tt_tool_create_imp
      CHANGING  cs_reported        TYPE zpru_if_agent_frw=>ts_adf_reported OPTIONAL
                cs_failed          TYPE zpru_if_agent_frw=>ts_adf_failed   OPTIONAL.

    METHODS precheck_update_tool
      IMPORTING it_tool_update_imp TYPE zpru_if_adf_type_and_constant=>tt_tool_update_imp
      EXPORTING et_entities        TYPE zpru_if_adf_type_and_constant=>tt_tool_update_imp
      CHANGING  cs_reported        TYPE zpru_if_agent_frw=>ts_adf_reported OPTIONAL
                cs_failed          TYPE zpru_if_agent_frw=>ts_adf_failed   OPTIONAL.

    METHODS precheck_delete_tool
      IMPORTING it_tool_delete_imp TYPE zpru_if_adf_type_and_constant=>tt_tool_delete_imp
      EXPORTING et_entities        TYPE zpru_if_adf_type_and_constant=>tt_tool_delete_imp
      CHANGING  cs_reported        TYPE zpru_if_agent_frw=>ts_adf_reported OPTIONAL
                cs_failed          TYPE zpru_if_agent_frw=>ts_adf_failed   OPTIONAL.

    METHODS precheck_read_tool
      IMPORTING it_tool_read_k TYPE zpru_if_adf_type_and_constant=>tt_tool_read_k
      EXPORTING et_entities    TYPE zpru_if_adf_type_and_constant=>tt_tool_read_k
      CHANGING  cs_reported    TYPE zpru_if_agent_frw=>ts_adf_reported OPTIONAL
                cs_failed      TYPE zpru_if_agent_frw=>ts_adf_failed   OPTIONAL.

    METHODS precheck_rba_tool
      IMPORTING it_rba_tool_k TYPE zpru_if_adf_type_and_constant=>tt_rba_tool_k
      EXPORTING et_entities   TYPE zpru_if_adf_type_and_constant=>tt_rba_tool_k
      CHANGING  cs_reported   TYPE zpru_if_agent_frw=>ts_adf_reported OPTIONAL
                cs_failed     TYPE zpru_if_agent_frw=>ts_adf_failed   OPTIONAL.

    METHODS collect_changes
      EXPORTING et_modify_agent TYPE tt_agent
                et_modify_tool  TYPE tt_agent_tool
                et_delete_agent TYPE tt_agent
                et_delete_tool  TYPE tt_agent_tool.

    METHODS cascade_deletes
      CHANGING ct_delete_agent TYPE tt_agent
               ct_delete_tool  TYPE tt_agent_tool.

    METHODS apply_db_changes
      IMPORTING it_modify_agent TYPE tt_agent
                it_modify_tool  TYPE tt_agent_tool
                it_delete_agent TYPE tt_agent
                it_delete_tool  TYPE tt_agent_tool
      RETURNING VALUE(rv_error) TYPE abap_bool.

    METHODS fill_agent_admin_fields
      IMPORTING iv_during_create TYPE abap_boolean DEFAULT abap_false
      CHANGING  cs_agent         TYPE zpru_cl_adf_buffer=>ts_agent.
ENDCLASS.


CLASS zpru_cl_adf_service IMPLEMENTATION.
  METHOD precheck_create_agent.
    DATA lo_pre TYPE REF TO zpru_if_adf_precheck.

    TRY.
        lo_pre ?= zpru_cl_agent_service_mngr=>get_service(
                      iv_service = `ZPRU_IF_ADF_PRECHECK`
                      iv_context = zpru_if_agent_frw=>cs_context-st_agent_definition ).
      CATCH zpru_cx_agent_core.
        RETURN.
    ENDTRY.

    lo_pre->precheck_create_agent( EXPORTING it_agent_create_imp = it_agent_create_imp
                                   IMPORTING et_entities         = et_entities
                                   CHANGING  cs_reported         = cs_reported
                                             cs_failed           = cs_failed ).
  ENDMETHOD.

  METHOD precheck_update_agent.
    DATA lo_pre TYPE REF TO zpru_if_adf_precheck.

    TRY.
        lo_pre ?= zpru_cl_agent_service_mngr=>get_service(
                      iv_service = `ZPRU_IF_ADF_PRECHECK`
                      iv_context = zpru_if_agent_frw=>cs_context-st_agent_definition ).
      CATCH zpru_cx_agent_core.
        RETURN.
    ENDTRY.

    lo_pre->precheck_update_agent( EXPORTING it_agent_update_imp = it_agent_update_imp
                                   IMPORTING et_entities         = et_entities
                                   CHANGING  cs_reported         = cs_reported
                                             cs_failed           = cs_failed ).
  ENDMETHOD.

  METHOD precheck_delete_agent.
    DATA lo_pre TYPE REF TO zpru_if_adf_precheck.

    TRY.
        lo_pre ?= zpru_cl_agent_service_mngr=>get_service(
                      iv_service = `ZPRU_IF_ADF_PRECHECK`
                      iv_context = zpru_if_agent_frw=>cs_context-st_agent_definition ).
      CATCH zpru_cx_agent_core.
        RETURN.
    ENDTRY.

    lo_pre->precheck_delete_agent( EXPORTING it_agent_delete_imp = it_agent_delete_imp
                                   IMPORTING et_entities         = et_entities
                                   CHANGING  cs_reported         = cs_reported
                                             cs_failed           = cs_failed ).
  ENDMETHOD.

  METHOD precheck_read_agent.
    DATA lo_pre TYPE REF TO zpru_if_adf_precheck.

    TRY.
        lo_pre ?= zpru_cl_agent_service_mngr=>get_service(
                      iv_service = `ZPRU_IF_ADF_PRECHECK`
                      iv_context = zpru_if_agent_frw=>cs_context-st_agent_definition ).
      CATCH zpru_cx_agent_core.
        RETURN.
    ENDTRY.

    lo_pre->precheck_read_agent( EXPORTING it_agent_read_k = it_agent_read_k
                                 IMPORTING et_entities     = et_entities
                                 CHANGING  cs_reported     = cs_reported
                                           cs_failed       = cs_failed ).
  ENDMETHOD.

  METHOD precheck_cba_tool.
    DATA lo_pre TYPE REF TO zpru_if_adf_precheck.

    TRY.
        lo_pre ?= zpru_cl_agent_service_mngr=>get_service(
                      iv_service = `ZPRU_IF_ADF_PRECHECK`
                      iv_context = zpru_if_agent_frw=>cs_context-st_agent_definition ).
      CATCH zpru_cx_agent_core.
        RETURN.
    ENDTRY.

    lo_pre->precheck_cba_tool( EXPORTING it_tool_create_imp = it_tool_create_imp
                               IMPORTING et_entities        = et_entities
                               CHANGING  cs_reported        = cs_reported
                                         cs_failed          = cs_failed ).
  ENDMETHOD.

  METHOD precheck_update_tool.
    DATA lo_pre TYPE REF TO zpru_if_adf_precheck.

    TRY.
        lo_pre ?= zpru_cl_agent_service_mngr=>get_service(
                      iv_service = `ZPRU_IF_ADF_PRECHECK`
                      iv_context = zpru_if_agent_frw=>cs_context-st_agent_definition ).
      CATCH zpru_cx_agent_core.
        RETURN.
    ENDTRY.

    lo_pre->precheck_update_tool( EXPORTING it_tool_update_imp = it_tool_update_imp
                                  IMPORTING et_entities        = et_entities
                                  CHANGING  cs_reported        = cs_reported
                                            cs_failed          = cs_failed ).
  ENDMETHOD.

  METHOD precheck_delete_tool.
    DATA lo_pre TYPE REF TO zpru_if_adf_precheck.

    TRY.
        lo_pre ?= zpru_cl_agent_service_mngr=>get_service(
                      iv_service = `ZPRU_IF_ADF_PRECHECK`
                      iv_context = zpru_if_agent_frw=>cs_context-st_agent_definition ).
      CATCH zpru_cx_agent_core.
        RETURN.
    ENDTRY.

    lo_pre->precheck_delete_tool( EXPORTING it_tool_delete_imp = it_tool_delete_imp
                                  IMPORTING et_entities        = et_entities
                                  CHANGING  cs_reported        = cs_reported
                                            cs_failed          = cs_failed ).
  ENDMETHOD.

  METHOD precheck_read_tool.
    DATA lo_pre TYPE REF TO zpru_if_adf_precheck.

    TRY.
        lo_pre ?= zpru_cl_agent_service_mngr=>get_service(
                      iv_service = `ZPRU_IF_ADF_PRECHECK`
                      iv_context = zpru_if_agent_frw=>cs_context-st_agent_definition ).
      CATCH zpru_cx_agent_core.
        RETURN.
    ENDTRY.

    lo_pre->precheck_read_tool( EXPORTING it_tool_read_k = it_tool_read_k
                                IMPORTING et_entities    = et_entities
                                CHANGING  cs_reported    = cs_reported
                                          cs_failed      = cs_failed ).
  ENDMETHOD.

  METHOD precheck_rba_tool.
    DATA lo_pre TYPE REF TO zpru_if_adf_precheck.

    TRY.
        lo_pre ?= zpru_cl_agent_service_mngr=>get_service(
                      iv_service = `ZPRU_IF_ADF_PRECHECK`
                      iv_context = zpru_if_agent_frw=>cs_context-st_agent_definition ).
      CATCH zpru_cx_agent_core.
        RETURN.
    ENDTRY.

    lo_pre->precheck_rba_tool( EXPORTING it_rba_tool_k = it_rba_tool_k
                               IMPORTING et_entities   = et_entities
                               CHANGING  cs_reported   = cs_reported
                                         cs_failed     = cs_failed ).
  ENDMETHOD.

  METHOD zpru_if_adf_service~query_agent.
    CLEAR: et_agent_k,
           et_tool_agent_link.

    SELECT aipf7agentuuid as agentuuid FROM zi_pru_agent
      WHERE aipf7agentname            IN @it_agent_name
        AND aipf7agenttype            IN @it_agent_type
        AND aipf7decisionprovider     IN @it_decision_provider
        AND aipf7shortmemoryprovider  IN @it_short_memory_provider
        AND aipf7longmemoryprovider   IN @it_long_memory_provider
        AND aipf7agentinfoprovider    IN @it_agent_info_provider
        AND aipf7systempromptprovider IN @it_system_prompt_provider
        AND aipf7agentstatus               IN @it_status
        AND aipf7createdby            IN @it_created_by
        AND aipf7createdat            IN @it_created_at
        AND aipf7changedby            IN @it_changed_by
        AND aipf7lastchanged          IN @it_last_changed
      INTO TABLE @et_agent_k.

    IF et_agent_k IS INITIAL.
      RETURN.
    ENDIF.

    IF et_tool_agent_link IS SUPPLIED.
      SELECT aipf7agentuuid as agentuuid, aipf7tooluuid as tooluuid FROM zi_pru_agent_tool
        FOR ALL ENTRIES IN @et_agent_k
        WHERE aipf7agentuuid = @et_agent_k-agentuuid
        INTO TABLE @et_tool_agent_link.
    ENDIF.
  ENDMETHOD.

  METHOD zpru_if_adf_service~create_agent.
    DATA ls_reported TYPE zpru_if_agent_frw=>ts_adf_reported.
    DATA ls_failed   TYPE zpru_if_agent_frw=>ts_adf_failed.
    DATA lo_util     TYPE REF TO zpru_if_agent_util.

    TRY.
        lo_util ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_AGENT_UTIL`
                                                            iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    precheck_create_agent( EXPORTING it_agent_create_imp = it_agent_create_imp
                           IMPORTING et_entities         = DATA(lt_entities)
                           CHANGING  cs_reported         = ls_reported
                                     cs_failed           = ls_failed ).

    cs_failed = CORRESPONDING #( DEEP ls_failed ).
    cs_reported = CORRESPONDING #( DEEP ls_reported ).

    IF    lt_entities     <> it_agent_create_imp
       OR ls_failed-agent IS NOT INITIAL.
      RETURN.
    ENDIF.

    zpru_cl_adf_buffer=>prep_agent_buffer( VALUE #( FOR <ls_k>
                                                    IN     lt_entities
                                                    ( agent_uuid = <ls_k>-agentuuid ) ) ).

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_create>).

      IF    NOT line_exists( zpru_cl_adf_buffer=>agent_buffer[ instance-agentuuid = <ls_create>-agentuuid ] )
         OR     line_exists( zpru_cl_adf_buffer=>agent_buffer[ instance-agentuuid = <ls_create>-agentuuid
                                                               deleted             = abap_true ] ).

        ASSIGN zpru_cl_adf_buffer=>agent_buffer[ instance-agentuuid = <ls_create>-agentuuid
                                                 deleted             = abap_false ] TO FIELD-SYMBOL(<ls_buffer>).
        IF sy-subrc = 0.
          DELETE zpru_cl_adf_buffer=>agent_buffer
                 WHERE     instance-agentuuid = <ls_buffer>-instance-agentuuid
                       AND deleted             = abap_true.
        ENDIF.

        APPEND VALUE #(
            instance-agentuuid             = <ls_create>-agentuuid
            instance-agentname             = <ls_create>-agentname
            instance-agenttype             = COND #( WHEN <ls_create>-control-agenttype = abap_true
                                                      THEN <ls_create>-agenttype )
            instance-decisionprovider      = COND #( WHEN <ls_create>-control-decisionprovider = abap_true
                                                      THEN <ls_create>-decisionprovider )
            instance-shortmemoryprovider  = COND #( WHEN <ls_create>-control-shortmemoryprovider = abap_true
                                                      THEN <ls_create>-shortmemoryprovider )
            instance-longmemoryprovider   = COND #( WHEN <ls_create>-control-longmemoryprovider = abap_true
                                                      THEN <ls_create>-longmemoryprovider )
            instance-agentinfoprovider    = COND #( WHEN <ls_create>-control-agentinfoprovider = abap_true
                                                      THEN <ls_create>-agentinfoprovider )
            instance-systempromptprovider = COND #( WHEN <ls_create>-control-systempromptprovider = abap_true
                                                      THEN <ls_create>-systempromptprovider )
            instance-agentstatus                 = COND #( WHEN <ls_create>-control-agentstatus = abap_true
                                                      THEN <ls_create>-agentstatus )
            instance-createdby             = COND #( WHEN <ls_create>-control-createdby = abap_true
                                                      THEN <ls_create>-createdby )
            instance-createdat             = COND #( WHEN <ls_create>-control-createdat = abap_true
                                                      THEN <ls_create>-createdat )
            instance-changedby             = COND #( WHEN <ls_create>-control-changedby <> if_abap_behv=>mk-off
                                                      THEN <ls_create>-changedby )
            instance-lastchanged           = COND #( WHEN <ls_create>-control-lastchanged = abap_true
                                                      THEN <ls_create>-lastchanged )
            instance-locallastchanged     = COND #( WHEN <ls_create>-control-locallastchanged = abap_true
                                                      THEN <ls_create>-locallastchanged )
            changed                         = abap_true
            deleted                         = abap_false ) TO zpru_cl_adf_buffer=>agent_buffer ASSIGNING FIELD-SYMBOL(<ls_just_added>).

        fill_agent_admin_fields( EXPORTING iv_during_create = abap_true
                                 CHANGING  cs_agent         = <ls_just_added> ).

        INSERT VALUE #( agentuuid = <ls_create>-agentuuid ) INTO TABLE cs_mapped-agent.

        APPEND VALUE #( msg       = lo_util->new_message(
                                        iv_id       = zpru_if_agent_frw=>cs_message_class-zpru_msg_execution
                                        iv_number   = `001`
                                        iv_severity = zpru_if_agent_message=>sc_severity-success
                                        iv_v1       = <ls_create>-agentuuid )
                        agentuuid = <ls_create>-agentuuid ) TO cs_reported-agent.

      ELSE.
        APPEND VALUE #( agentuuid = <ls_create>-agentuuid
                        create    = abap_true
                        fail      = zpru_if_agent_frw=>cs_fail_cause-conflict )
               TO cs_failed-agent.

        APPEND VALUE #( agentuuid = <ls_create>-agentuuid
                        create    = abap_true
                        msg       = lo_util->new_message(
                                        iv_id       = zpru_if_agent_frw=>cs_message_class-zpru_msg_execution
                                        iv_number   = `002`
                                        iv_severity = zpru_if_agent_message=>sc_severity-error
                                        iv_v1       = <ls_create>-agentuuid ) )
               TO cs_reported-agent.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_adf_service~read_agent.
    DATA ls_out TYPE zpru_s_agent.

    CLEAR et_agent.

    IF it_agent_read_k IS INITIAL.
      RETURN.
    ENDIF.

    precheck_read_agent( EXPORTING it_agent_read_k = it_agent_read_k
                         IMPORTING et_entities     = DATA(lt_entities)
                         CHANGING  cs_reported     = cs_reported
                                   cs_failed       = cs_failed ).

    zpru_cl_adf_buffer=>prep_agent_buffer( VALUE #( FOR <ls_k>
                                                    IN     lt_entities
                                                    ( agent_uuid = <ls_k>-agentuuid ) ) ).

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_read>).

      ASSIGN zpru_cl_adf_buffer=>agent_buffer[ instance-agentuuid = <ls_read>-agentuuid
                                               deleted             = abap_false ] TO FIELD-SYMBOL(<ls_buffer>).
      IF sy-subrc = 0.
        CLEAR ls_out.
        ls_out-agentuuid            = <ls_buffer>-instance-agentuuid.

        ls_out-agentname            = COND #( WHEN <ls_read>-control-agentname = abap_true
                                              THEN <ls_buffer>-instance-agentname ).
        ls_out-agenttype            = COND #( WHEN <ls_read>-control-agenttype = abap_true
                                              THEN <ls_buffer>-instance-agenttype ).
        ls_out-decisionprovider     = COND #( WHEN <ls_read>-control-decisionprovider = abap_true
                                              THEN <ls_buffer>-instance-decisionprovider ).
        ls_out-shortmemoryprovider  = COND #( WHEN <ls_read>-control-shortmemoryprovider = abap_true
                                              THEN <ls_buffer>-instance-shortmemoryprovider ).
        ls_out-longmemoryprovider   = COND #( WHEN <ls_read>-control-longmemoryprovider = abap_true
                                              THEN <ls_buffer>-instance-longmemoryprovider ).
        ls_out-agentinfoprovider    = COND #( WHEN <ls_read>-control-agentinfoprovider = abap_true
                                              THEN <ls_buffer>-instance-agentinfoprovider ).
        ls_out-systempromptprovider = COND #( WHEN <ls_read>-control-systempromptprovider = abap_true
                                              THEN <ls_buffer>-instance-systempromptprovider ).
        ls_out-agentstatus               = COND #( WHEN <ls_read>-control-agentstatus = abap_true
                                              THEN <ls_buffer>-instance-agentstatus ).
        ls_out-createdby            = COND #( WHEN <ls_read>-control-createdby = abap_true
                                              THEN <ls_buffer>-instance-createdby ).
        ls_out-createdat            = COND #( WHEN <ls_read>-control-createdat = abap_true
                                              THEN <ls_buffer>-instance-createdat ).
        ls_out-changedby            = COND #( WHEN <ls_read>-control-changedby = abap_true
                                              THEN <ls_buffer>-instance-changedby ).
        ls_out-lastchanged          = COND #( WHEN <ls_read>-control-lastchanged = abap_true
                                              THEN <ls_buffer>-instance-lastchanged ).
        ls_out-locallastchanged     = COND #( WHEN <ls_read>-control-locallastchanged = abap_true
                                              THEN <ls_buffer>-instance-locallastchanged ).

        APPEND ls_out TO et_agent.

      ELSE.
        APPEND VALUE #( agentuuid = <ls_read>-agentuuid
                        fail      = zpru_if_agent_frw=>cs_fail_cause-not_found )
               TO cs_failed-agent.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_adf_service~update_agent.
    DATA lo_util TYPE REF TO zpru_if_agent_util.

    precheck_update_agent( EXPORTING it_agent_update_imp = it_agent_update_imp
                           IMPORTING et_entities         = DATA(lt_entities)
                           CHANGING  cs_reported         = cs_reported
                                     cs_failed           = cs_failed ).

    IF lt_entities IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        lo_util ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_AGENT_UTIL`
                                                            iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    zpru_cl_adf_buffer=>prep_agent_buffer( VALUE #( FOR <ls_k>
                                                    IN     lt_entities
                                                    ( agent_uuid = <ls_k>-agentuuid ) ) ).

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_update>).

      ASSIGN zpru_cl_adf_buffer=>agent_buffer[ instance-agentuuid = <ls_update>-agentuuid
                                               deleted             = abap_false ] TO FIELD-SYMBOL(<ls_buffer>).
      IF sy-subrc = 0.
        <ls_buffer>-instance-agentname             = COND #( WHEN <ls_update>-control-agentname = abap_true
                                                              THEN <ls_update>-agentname
                                                              ELSE <ls_buffer>-instance-agentname ).
        <ls_buffer>-instance-agenttype             = COND #( WHEN <ls_update>-control-agenttype = abap_true
                                                              THEN <ls_update>-agenttype
                                                              ELSE <ls_buffer>-instance-agenttype ).
        <ls_buffer>-instance-decisionprovider      = COND #( WHEN <ls_update>-control-decisionprovider = abap_true
                                                              THEN <ls_update>-decisionprovider
                                                              ELSE <ls_buffer>-instance-decisionprovider ).
        <ls_buffer>-instance-shortmemoryprovider  = COND #( WHEN <ls_update>-control-shortmemoryprovider = abap_true
                                                              THEN <ls_update>-shortmemoryprovider
                                                              ELSE <ls_buffer>-instance-shortmemoryprovider ).
        <ls_buffer>-instance-longmemoryprovider   = COND #( WHEN <ls_update>-control-longmemoryprovider = abap_true
                                                              THEN <ls_update>-longmemoryprovider
                                                              ELSE <ls_buffer>-instance-longmemoryprovider ).
        <ls_buffer>-instance-agentinfoprovider    = COND #( WHEN <ls_update>-control-agentinfoprovider = abap_true
                                                              THEN <ls_update>-agentinfoprovider
                                                              ELSE <ls_buffer>-instance-agentinfoprovider ).
        <ls_buffer>-instance-systempromptprovider = COND #( WHEN <ls_update>-control-systempromptprovider = abap_true
                                                              THEN <ls_update>-systempromptprovider
                                                              ELSE <ls_buffer>-instance-systempromptprovider ).
        <ls_buffer>-instance-agentstatus                 = COND #( WHEN <ls_update>-control-agentstatus = abap_true
                                                              THEN <ls_update>-agentstatus
                                                              ELSE <ls_buffer>-instance-agentstatus ).
        <ls_buffer>-instance-createdby             = COND #( WHEN <ls_update>-control-createdby = abap_true
                                                              THEN <ls_update>-createdby
                                                              ELSE <ls_buffer>-instance-createdby ).
        <ls_buffer>-instance-createdat             = COND #( WHEN <ls_update>-control-createdat = abap_true
                                                              THEN <ls_update>-createdat
                                                              ELSE <ls_buffer>-instance-createdat ).
        <ls_buffer>-instance-changedby             = COND #( WHEN <ls_update>-control-changedby = abap_true
                                                              THEN <ls_update>-changedby
                                                              ELSE <ls_buffer>-instance-changedby ).
        <ls_buffer>-instance-lastchanged           = COND #( WHEN <ls_update>-control-lastchanged = abap_true
                                                              THEN <ls_update>-lastchanged
                                                              ELSE <ls_buffer>-instance-lastchanged ).
        <ls_buffer>-instance-locallastchanged     = COND #( WHEN <ls_update>-control-locallastchanged = abap_true
                                                              THEN <ls_update>-locallastchanged
                                                              ELSE <ls_buffer>-instance-locallastchanged ).

        <ls_buffer>-changed = abap_true.
        <ls_buffer>-deleted = abap_false.

        fill_agent_admin_fields( EXPORTING iv_during_create = abap_false
                                 CHANGING  cs_agent         = <ls_buffer> ).

      ELSE.
        APPEND VALUE #( agentuuid = <ls_update>-agentuuid
                        update    = abap_true
                        fail      = zpru_if_agent_frw=>cs_fail_cause-not_found )
               TO cs_failed-agent.

        APPEND VALUE #( agentuuid = <ls_update>-agentuuid
                        update    = abap_true
                        msg       = lo_util->new_message(
                                        iv_id       = zpru_if_agent_frw=>cs_message_class-zpru_msg_execution
                                        iv_number   = `012`
                                        iv_severity = zpru_if_agent_message=>sc_severity-error
                                        iv_v1       = <ls_update>-agentuuid ) )
               TO cs_reported-agent.

      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_adf_service~delete_agent.
    DATA lo_util TYPE REF TO zpru_if_agent_util.

    precheck_delete_agent( EXPORTING it_agent_delete_imp = it_agent_delete_imp
                           IMPORTING et_entities         = DATA(lt_entities)
                           CHANGING  cs_reported         = cs_reported
                                     cs_failed           = cs_failed ).

    IF lt_entities IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        lo_util ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_AGENT_UTIL`
                                                            iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    zpru_cl_adf_buffer=>prep_agent_buffer( VALUE #( FOR <ls_k>
                                                    IN     lt_entities
                                                    ( agent_uuid = <ls_k>-agentuuid ) ) ).

    zpru_cl_adf_buffer=>prep_tool_buffer( VALUE #( FOR <ls_q>
                                                   IN     lt_entities
                                                   ( agent_uuid = <ls_q>-agentuuid ) ) ).

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_delete>).

      ASSIGN zpru_cl_adf_buffer=>agent_buffer[ instance-agentuuid = <ls_delete>-agentuuid
                                               deleted             = abap_false ] TO FIELD-SYMBOL(<ls_buffer>).
      IF sy-subrc = 0.
        <ls_buffer>-deleted = abap_true.
        <ls_buffer>-changed = abap_true.

        LOOP AT zpru_cl_adf_buffer=>tool_buffer ASSIGNING FIELD-SYMBOL(<ls_tool_del>)
             WHERE instance-agentuuid = <ls_buffer>-instance-agentuuid.
          <ls_tool_del>-changed = abap_true.
          <ls_tool_del>-deleted = abap_true.
        ENDLOOP.

        APPEND VALUE #( msg       = lo_util->new_message(
                                        iv_id       = zpru_if_agent_frw=>cs_message_class-zpru_msg_execution
                                        iv_number   = `001`
                                        iv_severity = zpru_if_agent_message=>sc_severity-success
                                        iv_v1       = <ls_delete>-agentuuid )
                        agentuuid = <ls_delete>-agentuuid ) TO cs_reported-agent.

      ELSE.
        APPEND VALUE #( agentuuid = <ls_delete>-agentuuid
                        delete    = abap_true
                        fail      = zpru_if_agent_frw=>cs_fail_cause-not_found )
               TO cs_failed-agent.

        APPEND VALUE #( agentuuid = <ls_delete>-agentuuid
                        delete    = abap_true
                        msg       = lo_util->new_message(
                                        iv_id       = zpru_if_agent_frw=>cs_message_class-zpru_msg_execution
                                        iv_number   = `003`
                                        iv_severity = zpru_if_agent_message=>sc_severity-error ) )
               TO cs_reported-agent.

      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_adf_service~cba_tool.
    DATA lo_util TYPE REF TO zpru_if_agent_util.

    precheck_cba_tool( EXPORTING it_tool_create_imp = it_tool_create_imp
                       IMPORTING et_entities        = DATA(lt_entities)
                       CHANGING  cs_reported        = cs_reported
                                 cs_failed          = cs_failed ).

    IF lt_entities IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        lo_util ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_AGENT_UTIL`
                                                            iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    zpru_cl_adf_buffer=>prep_agent_buffer( VALUE #( FOR <ls_k>
                                                    IN  lt_entities
                                                    ( agent_uuid = <ls_k>-agentuuid ) ) ).

    zpru_cl_adf_buffer=>prep_tool_buffer( VALUE #( FOR <ls_q>
                                                   IN     lt_entities
                                                   ( agent_uuid = <ls_q>-agentuuid
                                                     tool_uuid  = <ls_q>-tooluuid
                                                     full_key   = abap_true ) ) ).

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_create>).

      ASSIGN zpru_cl_adf_buffer=>agent_buffer[ instance-agentuuid = <ls_create>-agentuuid
                                               " TODO: variable is assigned but never used (ABAP cleaner)
                                               deleted             = abap_false ] TO FIELD-SYMBOL(<ls_parent>).
      IF sy-subrc = 0.
        IF    NOT line_exists( zpru_cl_adf_buffer=>tool_buffer[ instance-agentuuid = <ls_create>-agentuuid
                                                                instance-tooluuid  = <ls_create>-tooluuid ] )
           OR     line_exists( zpru_cl_adf_buffer=>tool_buffer[ instance-agentuuid = <ls_create>-agentuuid
                                                                instance-tooluuid  = <ls_create>-tooluuid
                                                                deleted             = abap_true ] ).

          ASSIGN zpru_cl_adf_buffer=>tool_buffer[ instance-agentuuid = <ls_create>-agentuuid
                                                  instance-tooluuid  = <ls_create>-tooluuid
                                                  deleted             = abap_false ] TO FIELD-SYMBOL(<ls_buffer>).
          IF sy-subrc = 0.
            DELETE zpru_cl_adf_buffer=>tool_buffer
                   WHERE     instance-agentuuid = <ls_buffer>-instance-agentuuid
                         AND instance-tooluuid  = <ls_buffer>-instance-tooluuid
                         AND deleted             = abap_true.
          ENDIF.

          APPEND VALUE #(
              instance-tooluuid            = <ls_create>-tooluuid
              instance-agentuuid           = <ls_create>-agentuuid
              instance-toolname            = COND #( WHEN <ls_create>-control-toolname = abap_true
                                                      THEN <ls_create>-toolname )
              instance-toolprovider        = COND #( WHEN <ls_create>-control-toolprovider = abap_true
                                                      THEN <ls_create>-toolprovider )
              instance-steptype            = COND #( WHEN <ls_create>-control-steptype = abap_true
                                                      THEN <ls_create>-steptype )
              instance-toolschemaprovider = COND #( WHEN <ls_create>-control-toolschemaprovider = abap_true
                                                      THEN <ls_create>-toolschemaprovider )
              instance-toolinfoprovider   = COND #( WHEN <ls_create>-control-toolinfoprovider = abap_true
                                                      THEN <ls_create>-toolinfoprovider )
              instance-toolisborrowed          = COND #( WHEN <ls_create>-control-toolisborrowed = abap_true
                                                      THEN <ls_create>-toolisborrowed )
              instance-toolistransient         = COND #( WHEN <ls_create>-control-toolistransient = abap_true
                                                      THEN <ls_create>-toolistransient )
              changed                       = abap_true
              deleted                       = abap_false ) TO zpru_cl_adf_buffer=>tool_buffer.

          APPEND VALUE #( agentuuid = <ls_create>-agentuuid
                          tooluuid  = <ls_create>-tooluuid ) TO cs_mapped-tool.

          APPEND VALUE #( msg       = lo_util->new_message(
                                          iv_id       = zpru_if_agent_frw=>cs_message_class-zpru_msg_execution
                                          iv_number   = `001`
                                          iv_severity = zpru_if_agent_message=>sc_severity-success
                                          iv_v1       = <ls_create>-tooluuid )
                          agentuuid = <ls_create>-agentuuid
                          tooluuid  = <ls_create>-tooluuid ) TO cs_reported-tool.

        ELSE.
          APPEND VALUE #( agentuuid = <ls_create>-agentuuid
                          tooluuid  = <ls_create>-tooluuid
                          create    = abap_true
                          fail      = zpru_if_agent_frw=>cs_fail_cause-conflict )
                 TO cs_failed-tool.

          APPEND VALUE #( agentuuid = <ls_create>-agentuuid
                          tooluuid  = <ls_create>-tooluuid
                          create    = abap_true
                          msg       = lo_util->new_message(
                                          iv_id       = zpru_if_agent_frw=>cs_message_class-zpru_msg_execution
                                          iv_number   = `001`
                                          iv_severity = zpru_if_agent_message=>sc_severity-error
                                          iv_v1       = <ls_create>-tooluuid ) )
                 TO cs_reported-tool.
        ENDIF.

      ELSE.
        APPEND VALUE #( agentuuid = <ls_create>-agentuuid
                        tooluuid  = <ls_create>-tooluuid
                        create    = abap_true
                        fail      = zpru_if_agent_frw=>cs_fail_cause-not_found )
               TO cs_failed-tool.

        APPEND VALUE #( agentuuid = <ls_create>-agentuuid
                        tooluuid  = <ls_create>-tooluuid
                        create    = abap_true
                        msg       = lo_util->new_message(
                                        iv_id       = zpru_if_agent_frw=>cs_message_class-zpru_msg_execution
                                        iv_number   = `002`
                                        iv_severity = zpru_if_agent_message=>sc_severity-error ) )
               TO cs_reported-tool.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_adf_service~rba_tool.
    DATA ls_out TYPE zpru_s_agent_tool.

    CLEAR et_tool.

    IF it_rba_tool_k IS INITIAL.
      RETURN.
    ENDIF.

    precheck_rba_tool( EXPORTING it_rba_tool_k = it_rba_tool_k
                       IMPORTING et_entities   = DATA(lt_entities)
                       CHANGING  cs_reported   = cs_reported
                                 cs_failed     = cs_failed ).

    zpru_cl_adf_buffer=>prep_agent_buffer( VALUE #( FOR <ls_k>
                                                    IN     lt_entities
                                                    ( agent_uuid = <ls_k>-agentuuid ) ) ).

    zpru_cl_adf_buffer=>prep_tool_buffer( VALUE #( FOR <ls_q>
                                                   IN     lt_entities
                                                   ( agent_uuid = <ls_q>-agentuuid ) ) ).

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_h>).
      LOOP AT zpru_cl_adf_buffer=>tool_buffer ASSIGNING FIELD-SYMBOL(<ls_t_buf>)
           WHERE     instance-agentuuid = <ls_h>-agentuuid
                 AND deleted             = abap_false.

        CLEAR ls_out.
        ls_out-tooluuid           = <ls_t_buf>-instance-tooluuid.
        ls_out-agentuuid          = COND #( WHEN <ls_h>-control-agentuuid = abap_true
                                            THEN <ls_t_buf>-instance-agentuuid ).
        ls_out-toolname           = COND #( WHEN <ls_h>-control-toolname = abap_true
                                            THEN <ls_t_buf>-instance-toolname ).
        ls_out-toolprovider       = COND #( WHEN <ls_h>-control-toolprovider = abap_true
                                            THEN <ls_t_buf>-instance-toolprovider ).
        ls_out-steptype           = COND #( WHEN <ls_h>-control-steptype = abap_true
                                            THEN <ls_t_buf>-instance-steptype ).
        ls_out-toolschemaprovider = COND #( WHEN <ls_h>-control-toolschemaprovider = abap_true
                                            THEN <ls_t_buf>-instance-toolschemaprovider ).
        ls_out-toolinfoprovider   = COND #( WHEN <ls_h>-control-toolinfoprovider = abap_true
                                            THEN <ls_t_buf>-instance-toolinfoprovider ).
        ls_out-toolisborrowed         = COND #( WHEN <ls_h>-control-toolisborrowed = abap_true
                                            THEN <ls_t_buf>-instance-toolisborrowed ).
        ls_out-toolistransient        = COND #( WHEN <ls_h>-control-toolistransient = abap_true
                                            THEN <ls_t_buf>-instance-toolistransient ).

        APPEND ls_out TO et_tool.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_adf_service~read_tool.
    DATA ls_out TYPE zpru_s_agent_tool.

    CLEAR et_tool.

    IF it_tool_read_k IS INITIAL.
      RETURN.
    ENDIF.

    precheck_read_tool( EXPORTING it_tool_read_k = it_tool_read_k
                        IMPORTING et_entities    = DATA(lt_entities)
                        CHANGING  cs_reported    = cs_reported
                                  cs_failed      = cs_failed ).

    zpru_cl_adf_buffer=>prep_agent_buffer( VALUE #( FOR <ls_k>
                                                    IN     lt_entities
                                                    ( agent_uuid = <ls_k>-agentuuid ) ) ).

    zpru_cl_adf_buffer=>prep_tool_buffer( VALUE #( FOR <ls_q>
                                                   IN     lt_entities
                                                   ( agent_uuid = <ls_q>-agentuuid
                                                     tool_uuid  = <ls_q>-tooluuid
                                                     full_key   = abap_true ) ) ).

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_read>).
      ASSIGN zpru_cl_adf_buffer=>agent_buffer[ instance-agentuuid = <ls_read>-agentuuid ] TO FIELD-SYMBOL(<ls_parent>).
      IF sy-subrc = 0 AND <ls_parent>-deleted = abap_true.
        APPEND VALUE #( agentuuid = <ls_read>-agentuuid
                        tooluuid  = <ls_read>-tooluuid
                        fail      = zpru_if_agent_frw=>cs_fail_cause-not_found )
               TO cs_failed-tool.
        CONTINUE.
      ENDIF.

      ASSIGN zpru_cl_adf_buffer=>tool_buffer[ instance-agentuuid = <ls_read>-agentuuid
                                              instance-tooluuid  = <ls_read>-tooluuid ] TO FIELD-SYMBOL(<ls_buffer>).
      IF sy-subrc = 0.
        IF <ls_buffer>-deleted = abap_true.
          APPEND VALUE #( agentuuid = <ls_read>-agentuuid
                          tooluuid  = <ls_read>-tooluuid
                          fail      = zpru_if_agent_frw=>cs_fail_cause-not_found )
                 TO cs_failed-tool.
          CONTINUE.
        ENDIF.

        CLEAR ls_out.
        ls_out-tooluuid           = <ls_buffer>-instance-tooluuid.
        ls_out-agentuuid          = COND #( WHEN <ls_read>-control-agentuuid = abap_true
                                            THEN <ls_buffer>-instance-agentuuid ).
        ls_out-toolname           = COND #( WHEN <ls_read>-control-toolname = abap_true
                                            THEN <ls_buffer>-instance-toolname ).
        ls_out-toolprovider       = COND #( WHEN <ls_read>-control-toolprovider = abap_true
                                            THEN <ls_buffer>-instance-toolprovider ).
        ls_out-steptype           = COND #( WHEN <ls_read>-control-steptype = abap_true
                                            THEN <ls_buffer>-instance-steptype ).
        ls_out-toolschemaprovider = COND #( WHEN <ls_read>-control-toolschemaprovider = abap_true
                                            THEN <ls_buffer>-instance-toolschemaprovider ).
        ls_out-toolinfoprovider   = COND #( WHEN <ls_read>-control-toolinfoprovider = abap_true
                                            THEN <ls_buffer>-instance-toolinfoprovider ).
        ls_out-toolisborrowed         = COND #( WHEN <ls_read>-control-toolisborrowed = abap_true
                                            THEN <ls_buffer>-instance-toolisborrowed ).
        ls_out-toolistransient        = COND #( WHEN <ls_read>-control-toolistransient = abap_true
                                            THEN <ls_buffer>-instance-toolistransient ).

        APPEND ls_out TO et_tool.

      ELSE.
        APPEND VALUE #( agentuuid = <ls_read>-agentuuid
                        tooluuid  = <ls_read>-tooluuid
                        fail      = zpru_if_agent_frw=>cs_fail_cause-not_found )
               TO cs_failed-tool.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_adf_service~update_tool.
    DATA lo_util TYPE REF TO zpru_if_agent_util.

    precheck_update_tool( EXPORTING it_tool_update_imp = it_tool_update_imp
                          IMPORTING et_entities        = DATA(lt_entities)
                          CHANGING  cs_reported        = cs_reported
                                    cs_failed          = cs_failed ).

    IF lt_entities IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        lo_util ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_AGENT_UTIL`
                                                            iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    zpru_cl_adf_buffer=>prep_agent_buffer( VALUE #( FOR <ls_k>
                                                    IN     lt_entities
                                                    ( agent_uuid = <ls_k>-agentuuid ) ) ).

    zpru_cl_adf_buffer=>prep_tool_buffer( VALUE #( FOR <ls_q>
                                                   IN     lt_entities
                                                   ( agent_uuid = <ls_q>-agentuuid
                                                     tool_uuid  = <ls_q>-tooluuid
                                                     full_key   = abap_true ) ) ).

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_update>).

      ASSIGN zpru_cl_adf_buffer=>agent_buffer[ instance-agentuuid = <ls_update>-agentuuid ] TO FIELD-SYMBOL(<ls_parent>).
      IF sy-subrc = 0 AND <ls_parent>-deleted = abap_true.
        APPEND VALUE #( agentuuid = <ls_update>-agentuuid
                        tooluuid  = <ls_update>-tooluuid
                        update    = abap_true
                        fail      = zpru_if_agent_frw=>cs_fail_cause-not_found )
               TO cs_failed-tool.
        CONTINUE.
      ENDIF.

      ASSIGN zpru_cl_adf_buffer=>tool_buffer[ instance-agentuuid = <ls_update>-agentuuid
                                              instance-tooluuid  = <ls_update>-tooluuid ] TO FIELD-SYMBOL(<ls_buffer>).
      IF sy-subrc = 0 AND <ls_buffer>-deleted = abap_false.
        <ls_buffer>-instance-toolname            = COND #( WHEN <ls_update>-control-toolname = abap_true
                                                            THEN <ls_update>-toolname
                                                            ELSE <ls_buffer>-instance-toolname ).
        <ls_buffer>-instance-toolprovider        = COND #( WHEN <ls_update>-control-toolprovider = abap_true
                                                            THEN <ls_update>-toolprovider
                                                            ELSE <ls_buffer>-instance-toolprovider ).
        <ls_buffer>-instance-steptype            = COND #( WHEN <ls_update>-control-steptype = abap_true
                                                            THEN <ls_update>-steptype
                                                            ELSE <ls_buffer>-instance-steptype ).
        <ls_buffer>-instance-toolschemaprovider = COND #( WHEN <ls_update>-control-toolschemaprovider = abap_true
                                                            THEN <ls_update>-toolschemaprovider
                                                            ELSE <ls_buffer>-instance-toolschemaprovider ).
        <ls_buffer>-instance-toolinfoprovider   = COND #( WHEN <ls_update>-control-toolinfoprovider = abap_true
                                                            THEN <ls_update>-toolinfoprovider
                                                            ELSE <ls_buffer>-instance-toolinfoprovider ).
        <ls_buffer>-instance-toolisborrowed          = COND #( WHEN <ls_update>-control-toolisborrowed = abap_true
                                                            THEN <ls_update>-toolisborrowed
                                                            ELSE <ls_buffer>-instance-toolisborrowed ).
        <ls_buffer>-instance-toolistransient         = COND #( WHEN <ls_update>-control-toolistransient = abap_true
                                                            THEN <ls_update>-toolistransient
                                                            ELSE <ls_buffer>-instance-toolistransient ).
        <ls_buffer>-changed = abap_true.

      ELSE.
        APPEND VALUE #( agentuuid = <ls_update>-agentuuid
                        tooluuid  = <ls_update>-tooluuid
                        update    = abap_true
                        fail      = zpru_if_agent_frw=>cs_fail_cause-not_found )
               TO cs_failed-tool.

        APPEND VALUE #( agentuuid = <ls_update>-agentuuid
                        tooluuid  = <ls_update>-tooluuid
                        update    = abap_true
                        msg       = lo_util->new_message(
                                        iv_id       = zpru_if_agent_frw=>cs_message_class-zpru_msg_execution
                                        iv_number   = `002`
                                        iv_severity = zpru_if_agent_message=>sc_severity-error ) )
               TO cs_reported-tool.

      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_adf_service~delete_tool.
    DATA lo_util TYPE REF TO zpru_if_agent_util.

    precheck_delete_tool( EXPORTING it_tool_delete_imp = it_tool_delete_imp
                          IMPORTING et_entities        = DATA(lt_entities)
                          CHANGING  cs_reported        = cs_reported
                                    cs_failed          = cs_failed ).

    IF lt_entities IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        lo_util ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_AGENT_UTIL`
                                                            iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    zpru_cl_adf_buffer=>prep_agent_buffer( VALUE #( FOR <ls_k>
                                                    IN     lt_entities
                                                    ( agent_uuid = <ls_k>-agentuuid ) ) ).

    zpru_cl_adf_buffer=>prep_tool_buffer( VALUE #( FOR <ls_q>
                                                   IN     lt_entities
                                                   ( agent_uuid = <ls_q>-agentuuid
                                                     tool_uuid  = <ls_q>-tooluuid
                                                     full_key   = abap_true ) ) ).

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_delete>).

      ASSIGN zpru_cl_adf_buffer=>agent_buffer[ instance-agentuuid = <ls_delete>-agentuuid ] TO FIELD-SYMBOL(<ls_parent>).
      IF sy-subrc = 0 AND <ls_parent>-deleted = abap_true.
        " Parent deleted implies child deleted.
      ELSE.

        ASSIGN zpru_cl_adf_buffer=>tool_buffer[ instance-agentuuid = <ls_delete>-agentuuid
                                                instance-tooluuid  = <ls_delete>-tooluuid
                                                deleted             = abap_false ] TO FIELD-SYMBOL(<ls_buffer>).
        IF sy-subrc = 0.
          <ls_buffer>-deleted = abap_true.
          <ls_buffer>-changed = abap_true.
        ELSE.
          APPEND VALUE #( agentuuid = <ls_delete>-agentuuid
                          tooluuid  = <ls_delete>-tooluuid
                          delete    = abap_true
                          fail      = zpru_if_agent_frw=>cs_fail_cause-not_found )
                 TO cs_failed-tool.

          APPEND VALUE #( agentuuid = <ls_delete>-agentuuid
                          tooluuid  = <ls_delete>-tooluuid
                          delete    = abap_true
                          msg       = lo_util->new_message(
                                          iv_id       = zpru_if_agent_frw=>cs_message_class-zpru_msg_execution
                                          iv_number   = `003`
                                          iv_severity = zpru_if_agent_message=>sc_severity-error ) )
                 TO cs_reported-tool.

        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_adf_service~clean_up.
    CLEAR zpru_cl_adf_buffer=>agent_buffer.
    CLEAR zpru_cl_adf_buffer=>tool_buffer.
  ENDMETHOD.

  METHOD zpru_if_adf_service~do_save.
    calculate_triggers( IMPORTING et_check_decision_provider_v = DATA(lt_check_decision_provider_v)
                                  et_check_short_memory_v      = DATA(lt_check_short_memory_v)
                                  et_check_long_memory_v       = DATA(lt_check_long_memory_v)
                                  et_check_agent_info_v        = DATA(lt_check_agent_info_v)
                        CHANGING  cs_reported                  = cs_reported
                                  cs_failed                    = cs_failed ).

    zpru_if_adf_service~determine( CHANGING cs_reported = cs_reported
                                            cs_failed   = cs_failed
                                            cs_mapped   = cs_mapped ).

    zpru_if_adf_service~validate( EXPORTING it_check_decision_provider_v = lt_check_decision_provider_v
                                            it_check_short_memory_v      = lt_check_short_memory_v
                                            it_check_long_memory_v       = lt_check_long_memory_v
                                            it_check_agent_info_v        = lt_check_agent_info_v
                                  CHANGING  cs_reported                  = cs_reported
                                            cs_failed                    = cs_failed ).

    IF cs_failed IS NOT INITIAL.
      RETURN.
    ENDIF.

    DATA(lv_error) = db_modify( EXPORTING iv_do_commit = iv_do_commit
                                CHANGING  cs_reported  = cs_reported
                                          cs_failed    = cs_failed
                                          cs_mapped    = cs_mapped ).

    IF lv_error = abap_true.
      IF iv_do_commit = abap_true.
        ROLLBACK WORK.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD zpru_if_adf_service~determine.
    " Placeholder for determination logic
  ENDMETHOD.

  METHOD zpru_if_adf_service~validate.
    DATA lo_adf_validator TYPE REF TO zpru_if_adf_validator.

    TRY.
        lo_adf_validator ?= zpru_cl_agent_service_mngr=>get_service(
                                iv_service = `ZPRU_IF_ADF_VALIDATOR`
                                iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    IF it_check_decision_provider_v IS NOT INITIAL.

      lo_adf_validator->check_decision_provider( EXPORTING it_keys     = it_check_decision_provider_v
                                                 CHANGING  cs_reported = cs_reported
                                                           cs_failed   = cs_failed ).
    ENDIF.

    IF it_check_short_memory_v IS NOT INITIAL.
      lo_adf_validator->check_short_memory( EXPORTING it_keys     = it_check_short_memory_v
                                            CHANGING  cs_reported = cs_reported
                                                      cs_failed   = cs_failed ).
    ENDIF.

    IF it_check_long_memory_v IS NOT INITIAL.
      lo_adf_validator->check_long_memory( EXPORTING it_keys     = it_check_long_memory_v
                                           CHANGING  cs_reported = cs_reported
                                                     cs_failed   = cs_failed ).
    ENDIF.
  ENDMETHOD.

  METHOD db_modify.
    " TODO: parameter IV_DO_COMMIT is never used (ABAP cleaner)
    " TODO: parameter CS_REPORTED is never used or assigned (ABAP cleaner)
    " TODO: parameter CS_FAILED is never used or assigned (ABAP cleaner)
    " TODO: parameter CS_MAPPED is never used or assigned (ABAP cleaner)

    rv_error = abap_false.

    collect_changes( IMPORTING et_modify_agent = DATA(lt_mod_agent)
                               et_modify_tool  = DATA(lt_mod_tool)
                               et_delete_agent = DATA(lt_del_agent)
                               et_delete_tool  = DATA(lt_del_tool) ).

    cascade_deletes( CHANGING ct_delete_agent = lt_del_agent
                              ct_delete_tool  = lt_del_tool ).

    rv_error = apply_db_changes( it_modify_agent = lt_mod_agent
                                 it_modify_tool  = lt_mod_tool
                                 it_delete_agent = lt_del_agent
                                 it_delete_tool  = lt_del_tool ).
  ENDMETHOD.

  METHOD cascade_deletes.
*    LOOP AT ct_delete_agent ASSIGNING FIELD-SYMBOL(<ls_agent>).
*      zpru_cl_adf_buffer=>prep_tool_buffer( VALUE #( ( agent_uuid = <ls_agent>-agent_uuid ) ) ).
*
*      LOOP AT zpru_cl_adf_buffer=>tool_buffer ASSIGNING FIELD-SYMBOL(<ls_buf_tool>) WHERE instance-agent_uuid = <ls_agent>-agent_uuid.
*        IF NOT line_exists( ct_delete_tool[ tool_uuid = <ls_buf_tool>-instance-tool_uuid ] ).
*          APPEND <ls_buf_tool>-instance TO ct_delete_tool.
*        ENDIF.
*      ENDLOOP.
*    ENDLOOP.
  ENDMETHOD.

  METHOD collect_changes.
*    LOOP AT zpru_cl_adf_buffer=>agent_buffer ASSIGNING FIELD-SYMBOL(<ls_agent>) WHERE changed = abap_true.
*      IF <ls_agent>-deleted = abap_true.
*        APPEND <ls_agent>-instance TO et_delete_agent.
*      ELSE.
*        APPEND <ls_agent>-instance TO et_modify_agent.
*      ENDIF.
*    ENDLOOP.
*
*    LOOP AT zpru_cl_adf_buffer=>tool_buffer ASSIGNING FIELD-SYMBOL(<ls_tool>) WHERE changed = abap_true.
*      IF <ls_tool>-deleted = abap_true.
*        APPEND <ls_tool>-instance TO et_delete_tool.
*      ELSE.
*        APPEND <ls_tool>-instance TO et_modify_tool.
*      ENDIF.
*    ENDLOOP.
  ENDMETHOD.

  METHOD apply_db_changes.
*    rv_error = abap_false.
*
*    IF it_modify_agent IS NOT INITIAL.
*      MODIFY zpru_agent FROM TABLE @it_modify_agent.
*      IF sy-subrc <> 0.
*        rv_error = abap_true.
*      ENDIF.
*    ENDIF.
*
*    IF it_delete_agent IS NOT INITIAL.
*      DELETE zpru_agent FROM TABLE @it_delete_agent.
*      IF sy-subrc <> 0.
*        rv_error = abap_true.
*      ENDIF.
*    ENDIF.
*
*    IF it_modify_tool IS NOT INITIAL.
*      MODIFY zpru_agent_tool FROM TABLE @it_modify_tool.
*      IF sy-subrc <> 0.
*        rv_error = abap_true.
*      ENDIF.
*    ENDIF.
*
*    IF it_delete_tool IS NOT INITIAL.
*      DELETE zpru_agent_tool FROM TABLE @it_delete_tool.
*      IF sy-subrc <> 0.
*        rv_error = abap_true.
*      ENDIF.
*    ENDIF.
  ENDMETHOD.

  METHOD fill_agent_admin_fields.
*    GET TIME STAMP FIELD DATA(lv_now).
*
*    IF iv_during_create = abap_true.
*      cs_agent-instance-created_by = COND #( WHEN cs_agent-instance-created_by IS INITIAL
*                                             THEN sy-uname
*                                             ELSE cs_agent-instance-created_by ).
*      cs_agent-instance-created_at = COND #( WHEN cs_agent-instance-created_at IS INITIAL
*                                             THEN lv_now
*                                             ELSE cs_agent-instance-created_at ).
*    ENDIF.
*
*    cs_agent-instance-last_changed       = lv_now.
*    cs_agent-instance-changed_by         = sy-uname.
*    cs_agent-instance-local_last_changed = lv_now.
  ENDMETHOD.

  METHOD calculate_triggers.
*    " TODO: parameter CS_REPORTED is never used or assigned (ABAP cleaner)
*    " TODO: parameter CS_FAILED is never used or assigned (ABAP cleaner)
*
*    DATA lt_agent_2_proc              LIKE zpru_cl_adf_buffer=>agent_buffer.
*    DATA lt_tool_2_proc               LIKE zpru_cl_adf_buffer=>tool_buffer.
*    DATA lo_agent_descr               TYPE REF TO cl_abap_structdescr.
*    DATA lo_tool_descr                TYPE REF TO cl_abap_structdescr.
*    DATA lt_check_decision_provider_v TYPE zpru_if_adf_type_and_constant=>tt_agent_read_k. " create and update field "decision_provider"
*    " TODO: variable is assigned but never used (ABAP cleaner)
*    DATA lt_check_short_memory_v      TYPE zpru_if_adf_type_and_constant=>tt_agent_read_k. " create and update field "short_memory_provider"
*    " TODO: variable is assigned but never used (ABAP cleaner)
*    DATA lt_check_long_memory_v       TYPE zpru_if_adf_type_and_constant=>tt_agent_read_k. " create and update field "long_memory_provider"
*    " TODO: variable is assigned but never used (ABAP cleaner)
*    DATA lt_check_agent_info_v        TYPE zpru_if_adf_type_and_constant=>tt_agent_read_k. " create and update field "agent_info_provider"
*
*    CLEAR et_check_decision_provider_v.
*    CLEAR et_check_short_memory_v.
*    CLEAR et_check_long_memory_v.
*    CLEAR et_check_agent_info_v.
*
**    LOOP AT zpru_cl_adf_buffer=>agent_buffer ASSIGNING FIELD-SYMBOL(<ls_agent_buffer>).
**      APPEND INITIAL LINE TO lt_agent_2_proc ASSIGNING FIELD-SYMBOL(<ls_agent_2_process>).
**      <ls_agent_2_process>-instance = CORRESPONDING #( <ls_agent_buffer>-instance ).
**      <ls_agent_2_process>-deleted  = <ls_agent_buffer>-deleted.
**
**      LOOP AT zpru_cl_adf_buffer=>tool_buffer ASSIGNING FIELD-SYMBOL(<ls_tool_buffer>)
**           WHERE instance-agent_uuid = <ls_agent_buffer>-instance-agent_uuid.
**        APPEND INITIAL LINE TO lt_tool_2_proc ASSIGNING FIELD-SYMBOL(<ls_tool_2_process>).
**        <ls_tool_2_process>-instance = CORRESPONDING #( <ls_tool_buffer>-instance ).
**        <ls_tool_2_process>-deleted  = <ls_tool_buffer>-deleted.
**      ENDLOOP.
**    ENDLOOP.
*
*    IF lt_agent_2_proc IS INITIAL AND lt_tool_2_proc IS INITIAL.
*      RETURN.
*    ENDIF.
*
*    IF lt_agent_2_proc IS NOT INITIAL.
*      SELECT * FROM ZI_PRU_AGENT AS agent
*        FOR ALL ENTRIES IN @lt_agent_2_proc
*        WHERE agent~agentuuid = @lt_agent_2_proc-instance-agent_uuid
*        INTO TABLE @DATA(lt_agent_db_state).
*    ENDIF.
*
*    IF lt_tool_2_proc IS NOT INITIAL.
*      SELECT * FROM zpru_agent_tool AS tool
*        FOR ALL ENTRIES IN @lt_tool_2_proc
*        WHERE tool~tool_uuid = @lt_tool_2_proc-instance-tool_uuid
*        INTO TABLE @DATA(lt_tool_db_state).
*    ENDIF.
*
*    lo_agent_descr ?= cl_abap_structdescr=>describe_by_name( 'ZPRU_AGENT' ).
*    DATA(lt_agent_fields) = lo_agent_descr->get_symbols( ).
*    lo_tool_descr ?= cl_abap_structdescr=>describe_by_name( 'ZPRU_AGENT_TOOL' ).
*    DATA(lt_tool_fields) = lo_tool_descr->get_symbols( ).
*
*    LOOP AT lt_agent_2_proc ASSIGNING fIELD-SYMBOL(<ls_agent_2_process>).
*
*      " calculate CREATE trigger
*      IF     NOT line_exists( lt_agent_db_state[ agentuuid = <ls_agent_2_process>-instance-agent_uuid ] )
*         AND     <ls_agent_2_process>-deleted = abap_false.
*        APPEND INITIAL LINE TO lt_check_decision_provider_v ASSIGNING FIELD-SYMBOL(<ls_check_decision_provider_v>).
*        <ls_check_decision_provider_v> = CORRESPONDING #( <ls_agent_2_process>-instance ).
*
*        APPEND INITIAL LINE TO lt_check_short_memory_v ASSIGNING FIELD-SYMBOL(<ls_check_short_memory_v>).
*        <ls_check_short_memory_v> = CORRESPONDING #( <ls_agent_2_process>-instance ).
*
*        APPEND INITIAL LINE TO lt_check_long_memory_v ASSIGNING FIELD-SYMBOL(<ls_check_long_memory_v>).
*        <ls_check_long_memory_v> = CORRESPONDING #( <ls_agent_2_process>-instance ).
*
*        APPEND INITIAL LINE TO lt_check_agent_info_v ASSIGNING FIELD-SYMBOL(<ls_check_agent_info_v>).
*        <ls_check_agent_info_v> = CORRESPONDING #( <ls_agent_2_process>-instance ).
*      ENDIF.
*
*      ASSIGN lt_agent_db_state[ agentuuid = <ls_agent_2_process>-instance-agent_uuid ] TO FIELD-SYMBOL(<ls_agent_db_state>).
*      IF sy-subrc <> 0.
*        CONTINUE.
*      ENDIF.
*
*      " calculate DELETE trigger before update
*      " just skip deleted entries
*      IF <ls_agent_2_process>-deleted = abap_true.
*        CONTINUE.
*      ENDIF.
*
*      " calculate UPDATE trigger
*      LOOP AT lt_agent_fields ASSIGNING FIELD-SYMBOL(<lv_agent_fields>).
*
*        ASSIGN COMPONENT <lv_agent_fields>-name OF STRUCTURE <ls_agent_2_process>-instance TO FIELD-SYMBOL(<lv_buffer_value>).
*        IF sy-subrc <> 0.
*          CONTINUE.
*        ENDIF.
*
*        ASSIGN COMPONENT <lv_agent_fields>-name OF STRUCTURE <ls_agent_db_state> TO FIELD-SYMBOL(<lv_db_value>).
*        IF sy-subrc <> 0.
*          CONTINUE.
*        ENDIF.
*
*        IF <lv_buffer_value> <> <lv_db_value>.
**          APPEND INITIAL LINE TO validation / determination
*          EXIT.
*        ENDIF.
*      ENDLOOP.
*
*      " FIELD decision_provider
*      ASSIGN COMPONENT 'DECISION_PROVIDER' OF STRUCTURE <ls_agent_2_process>-instance TO <lv_buffer_value>.
*      IF sy-subrc = 0.
*        ASSIGN COMPONENT 'DECISION_PROVIDER' OF STRUCTURE <ls_agent_db_state> TO <lv_db_value>.
*        IF sy-subrc = 0.
*          IF <lv_buffer_value> <> <lv_db_value>.
*            APPEND INITIAL LINE TO lt_check_decision_provider_v ASSIGNING <ls_check_decision_provider_v>.
*            <ls_check_decision_provider_v> = CORRESPONDING #( <ls_agent_2_process>-instance ).
*          ENDIF.
*        ENDIF.
*      ENDIF.
*
*      " FIELD short_memory_provider
*      ASSIGN COMPONENT 'SHORT_MEMORY_PROVIDER' OF STRUCTURE <ls_agent_2_process>-instance TO <lv_buffer_value>.
*      IF sy-subrc = 0.
*        ASSIGN COMPONENT 'SHORT_MEMORY_PROVIDER' OF STRUCTURE <ls_agent_db_state> TO <lv_db_value>.
*        IF sy-subrc = 0.
*          IF <lv_buffer_value> <> <lv_db_value>.
*            APPEND INITIAL LINE TO lt_check_short_memory_v ASSIGNING <ls_check_short_memory_v>.
*            <ls_check_short_memory_v> = CORRESPONDING #( <ls_agent_2_process>-instance ).
*          ENDIF.
*        ENDIF.
*      ENDIF.
*
*      " FIELD long_memory_provider
*      ASSIGN COMPONENT 'LONG_MEMORY_PROVIDER' OF STRUCTURE <ls_agent_2_process>-instance TO <lv_buffer_value>.
*      IF sy-subrc = 0.
*        ASSIGN COMPONENT 'LONG_MEMORY_PROVIDER' OF STRUCTURE <ls_agent_db_state> TO <lv_db_value>.
*        IF sy-subrc = 0.
*          IF <lv_buffer_value> <> <lv_db_value>.
*            APPEND INITIAL LINE TO lt_check_long_memory_v ASSIGNING <ls_check_long_memory_v>.
*            <ls_check_long_memory_v> = CORRESPONDING #( <ls_agent_2_process>-instance ).
*          ENDIF.
*        ENDIF.
*      ENDIF.
*
*      " FIELD agent_info_provider
*      ASSIGN COMPONENT 'AGENT_INFO_PROVIDER' OF STRUCTURE <ls_agent_2_process>-instance TO <lv_buffer_value>.
*      IF sy-subrc = 0.
*        ASSIGN COMPONENT 'AGENT_INFO_PROVIDER' OF STRUCTURE <ls_agent_db_state> TO <lv_db_value>.
*        IF sy-subrc = 0.
*          IF <lv_buffer_value> <> <lv_db_value>.
*            APPEND INITIAL LINE TO lt_check_agent_info_v ASSIGNING <ls_check_agent_info_v>.
*            <ls_check_agent_info_v> = CORRESPONDING #( <ls_agent_2_process>-instance ).
*          ENDIF.
*        ENDIF.
*      ENDIF.
*
*    ENDLOOP.
*
*    LOOP AT lt_tool_2_proc ASSIGNING fiELD-SYMBOL(<ls_tool_2_process>).
*
*      " CREATE
*      IF     NOT line_exists( lt_tool_db_state[ tool_uuid = <ls_tool_2_process>-instance-tool_uuid ] )
*         AND     <ls_tool_2_process>-deleted = abap_false.
**        APPEND INITIAL LINE TO validation / determination
*      ENDIF.
*
*      ASSIGN lt_tool_db_state[ tool_uuid = <ls_tool_2_process>-instance-tool_uuid ] TO FIELD-SYMBOL(<ls_tool_db_state>).
*      IF sy-subrc <> 0.
*        CONTINUE.
*      ENDIF.
*
*      " calc DELETE trigger before update
*      " just skip deleted entries
*      IF <ls_tool_2_process>-deleted = abap_true.
*        CONTINUE.
*      ENDIF.
*
*      " UPDATE
*      LOOP AT lt_tool_fields ASSIGNING FIELD-SYMBOL(<ls_tool_fields>).
*
*        ASSIGN COMPONENT <ls_tool_fields>-name OF STRUCTURE <ls_tool_2_process>-instance TO <lv_buffer_value>.
*        IF sy-subrc <> 0.
*          CONTINUE.
*        ENDIF.
*
*        ASSIGN COMPONENT <ls_tool_fields>-name OF STRUCTURE <ls_tool_db_state> TO <lv_db_value>.
*        IF sy-subrc <> 0.
*          CONTINUE.
*        ENDIF.
*
*        IF <lv_buffer_value> <> <lv_db_value>.
**          APPEND INITIAL LINE TO validation / determination
*          EXIT.
*        ENDIF.
*      ENDLOOP.
*
**      " FIELD field
**      ASSIGN COMPONENT 'FIELD' OF STRUCTURE <ls_tool_2_PROCESS>-instance TO <lv_buffer_value>.
**      IF sy-subrc = 0.
**        ASSIGN COMPONENT 'FIELD' OF STRUCTURE <ls_tool_db_state> TO <lv_db_value>.
**        IF sy-subrc = 0.
**          IF <lv_buffer_value> <> <lv_db_value>.
**            APPEND INITIAL LINE TO validation / determination
**          ENDIF.
**        ENDIF.
**      ENDIF.
*    ENDLOOP.
*
*    SORT lt_check_decision_provider_v BY table_line.
*    DELETE ADJACENT DUPLICATES FROM lt_check_decision_provider_v COMPARING table_line.
*
*    et_check_decision_provider_v = lt_check_decision_provider_v.
  ENDMETHOD.
ENDCLASS.
