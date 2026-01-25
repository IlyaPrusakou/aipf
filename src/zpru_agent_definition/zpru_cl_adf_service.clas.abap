CLASS zpru_cl_adf_service DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zpru_if_agent_frw.
    INTERFACES zpru_if_adf_service.

  PROTECTED SECTION.
    TYPES tt_agent      TYPE STANDARD TABLE OF zpru_agent WITH EMPTY KEY.
    TYPES tt_agent_tool TYPE STANDARD TABLE OF zpru_agent_tool WITH EMPTY KEY.

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

    SELECT agent_uuid FROM zpru_agent
      WHERE agent_name             IN @it_agent_name
        AND agent_type             IN @it_agent_type
        AND decision_provider      IN @it_decision_provider
        AND short_memory_provider  IN @it_short_memory_provider
        AND long_memory_provider   IN @it_long_memory_provider
        AND agent_info_provider    IN @it_agent_info_provider
        AND system_prompt_provider IN @it_system_prompt_provider
        AND status                 IN @it_status
        AND created_by             IN @it_created_by
        AND created_at             IN @it_created_at
        AND changed_by             IN @it_changed_by
        AND last_changed           IN @it_last_changed
      INTO TABLE @et_agent_k.

    IF et_agent_k IS INITIAL.
      RETURN.
    ENDIF.

    IF et_tool_agent_link IS SUPPLIED.
      SELECT agent_uuid, tool_uuid FROM zpru_agent_tool
        FOR ALL ENTRIES IN @et_agent_k
        WHERE agent_uuid = @et_agent_k-agent_uuid
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
                                                    ( agent_uuid = <ls_k>-agent_uuid ) ) ).

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_create>).

      IF    NOT line_exists( zpru_cl_adf_buffer=>agent_buffer[ instance-agent_uuid = <ls_create>-agent_uuid ] )
         OR     line_exists( zpru_cl_adf_buffer=>agent_buffer[ instance-agent_uuid = <ls_create>-agent_uuid
                                                               deleted             = abap_true ] ).

        ASSIGN zpru_cl_adf_buffer=>agent_buffer[ instance-agent_uuid = <ls_create>-agent_uuid
                                                 deleted             = abap_false ] TO FIELD-SYMBOL(<ls_buffer>).
        IF sy-subrc = 0.
          DELETE zpru_cl_adf_buffer=>agent_buffer
                 WHERE     instance-agent_uuid = <ls_buffer>-instance-agent_uuid
                       AND deleted             = abap_true.
        ENDIF.

        APPEND VALUE #(
            instance-agent_uuid             = <ls_create>-agent_uuid
            instance-agent_name             = <ls_create>-agent_name
            instance-agent_type             = COND #( WHEN <ls_create>-control-agent_type = abap_true
                                                      THEN <ls_create>-agent_type )
            instance-decision_provider      = COND #( WHEN <ls_create>-control-decision_provider = abap_true
                                                      THEN <ls_create>-decision_provider )
            instance-short_memory_provider  = COND #( WHEN <ls_create>-control-short_memory_provider = abap_true
                                                      THEN <ls_create>-short_memory_provider )
            instance-long_memory_provider   = COND #( WHEN <ls_create>-control-long_memory_provider = abap_true
                                                      THEN <ls_create>-long_memory_provider )
            instance-agent_info_provider    = COND #( WHEN <ls_create>-control-agent_info_provider = abap_true
                                                      THEN <ls_create>-agent_info_provider )
            instance-system_prompt_provider = COND #( WHEN <ls_create>-control-system_prompt_provider = abap_true
                                                      THEN <ls_create>-system_prompt_provider )
            instance-status                 = COND #( WHEN <ls_create>-control-status = abap_true
                                                      THEN <ls_create>-status )
            instance-created_by             = COND #( WHEN <ls_create>-control-created_by = abap_true
                                                      THEN <ls_create>-created_by )
            instance-created_at             = COND #( WHEN <ls_create>-control-created_at = abap_true
                                                      THEN <ls_create>-created_at )
            instance-changed_by             = COND #( WHEN <ls_create>-control-changed_by <> if_abap_behv=>mk-off
                                                      THEN <ls_create>-changed_by )
            instance-last_changed           = COND #( WHEN <ls_create>-control-last_changed = abap_true
                                                      THEN <ls_create>-last_changed )
            instance-local_last_changed     = COND #( WHEN <ls_create>-control-local_last_changed = abap_true
                                                      THEN <ls_create>-local_last_changed )
            changed                         = abap_true
            deleted                         = abap_false ) TO zpru_cl_adf_buffer=>agent_buffer ASSIGNING FIELD-SYMBOL(<ls_just_added>).

        fill_agent_admin_fields( EXPORTING iv_during_create = abap_true
                                 CHANGING  cs_agent         = <ls_just_added> ).

        INSERT VALUE #( agent_uuid = <ls_create>-agent_uuid ) INTO TABLE cs_mapped-agent.

        APPEND VALUE #( msg        = lo_util->new_message(
                                         iv_id       = zpru_if_agent_frw=>cs_message_class-zpru_msg_execution
                                         iv_number   = `001`
                                         iv_severity = zpru_if_agent_message=>sc_severity-success
                                         iv_v1       = <ls_create>-agent_uuid )
                        agent_uuid = <ls_create>-agent_uuid ) TO cs_reported-agent.

      ELSE.
        APPEND VALUE #( agent_uuid = <ls_create>-agent_uuid
                        create     = abap_true
                        fail       = zpru_if_agent_frw=>cs_fail_cause-conflict )
               TO cs_failed-agent.

        APPEND VALUE #( agent_uuid = <ls_create>-agent_uuid
                        create     = abap_true
                        msg        = lo_util->new_message(
                                         iv_id       = zpru_if_agent_frw=>cs_message_class-zpru_msg_execution
                                         iv_number   = `002`
                                         iv_severity = zpru_if_agent_message=>sc_severity-error
                                         iv_v1       = <ls_create>-agent_uuid ) )
               TO cs_reported-agent.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_adf_service~read_agent.
    DATA ls_out TYPE zpru_agent.

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
                                                    ( agent_uuid = <ls_k>-agent_uuid ) ) ).

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_read>).

      ASSIGN zpru_cl_adf_buffer=>agent_buffer[ instance-agent_uuid = <ls_read>-agent_uuid
                                               deleted             = abap_false ] TO FIELD-SYMBOL(<ls_buffer>).
      IF sy-subrc = 0.
        CLEAR ls_out.
        ls_out-agent_uuid             = <ls_buffer>-instance-agent_uuid.

        ls_out-agent_name             = COND #( WHEN <ls_read>-control-agent_name = abap_true
                                                THEN <ls_buffer>-instance-agent_name ).
        ls_out-agent_type             = COND #( WHEN <ls_read>-control-agent_type = abap_true
                                                THEN <ls_buffer>-instance-agent_type ).
        ls_out-decision_provider      = COND #( WHEN <ls_read>-control-decision_provider = abap_true
                                                THEN <ls_buffer>-instance-decision_provider ).
        ls_out-short_memory_provider  = COND #( WHEN <ls_read>-control-short_memory_provider = abap_true
                                                THEN <ls_buffer>-instance-short_memory_provider ).
        ls_out-long_memory_provider   = COND #( WHEN <ls_read>-control-long_memory_provider = abap_true
                                                THEN <ls_buffer>-instance-long_memory_provider ).
        ls_out-agent_info_provider    = COND #( WHEN <ls_read>-control-agent_info_provider = abap_true
                                                THEN <ls_buffer>-instance-agent_info_provider ).
        ls_out-system_prompt_provider = COND #( WHEN <ls_read>-control-system_prompt_provider = abap_true
                                                THEN <ls_buffer>-instance-system_prompt_provider ).
        ls_out-status                 = COND #( WHEN <ls_read>-control-status = abap_true
                                                THEN <ls_buffer>-instance-status ).
        ls_out-created_by             = COND #( WHEN <ls_read>-control-created_by = abap_true
                                                THEN <ls_buffer>-instance-created_by ).
        ls_out-created_at             = COND #( WHEN <ls_read>-control-created_at = abap_true
                                                THEN <ls_buffer>-instance-created_at ).
        ls_out-changed_by             = COND #( WHEN <ls_read>-control-changed_by = abap_true
                                                THEN <ls_buffer>-instance-changed_by ).
        ls_out-last_changed           = COND #( WHEN <ls_read>-control-last_changed = abap_true
                                                THEN <ls_buffer>-instance-last_changed ).
        ls_out-local_last_changed     = COND #( WHEN <ls_read>-control-local_last_changed = abap_true
                                                THEN <ls_buffer>-instance-local_last_changed ).

        APPEND ls_out TO et_agent.

      ELSE.
        APPEND VALUE #( agent_uuid = <ls_read>-agent_uuid
                        fail       = zpru_if_agent_frw=>cs_fail_cause-not_found )
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
                                                    ( agent_uuid = <ls_k>-agent_uuid ) ) ).

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_update>).

      ASSIGN zpru_cl_adf_buffer=>agent_buffer[ instance-agent_uuid = <ls_update>-agent_uuid
                                               deleted             = abap_false ] TO FIELD-SYMBOL(<ls_buffer>).
      IF sy-subrc = 0.
        <ls_buffer>-instance-agent_name             = COND #( WHEN <ls_update>-control-agent_name = abap_true
                                                              THEN <ls_update>-agent_name
                                                              ELSE <ls_buffer>-instance-agent_name ).
        <ls_buffer>-instance-agent_type             = COND #( WHEN <ls_update>-control-agent_type = abap_true
                                                              THEN <ls_update>-agent_type
                                                              ELSE <ls_buffer>-instance-agent_type ).
        <ls_buffer>-instance-decision_provider      = COND #( WHEN <ls_update>-control-decision_provider = abap_true
                                                              THEN <ls_update>-decision_provider
                                                              ELSE <ls_buffer>-instance-decision_provider ).
        <ls_buffer>-instance-short_memory_provider  = COND #( WHEN <ls_update>-control-short_memory_provider = abap_true
                                                              THEN <ls_update>-short_memory_provider
                                                              ELSE <ls_buffer>-instance-short_memory_provider ).
        <ls_buffer>-instance-long_memory_provider   = COND #( WHEN <ls_update>-control-long_memory_provider = abap_true
                                                              THEN <ls_update>-long_memory_provider
                                                              ELSE <ls_buffer>-instance-long_memory_provider ).
        <ls_buffer>-instance-agent_info_provider    = COND #( WHEN <ls_update>-control-agent_info_provider = abap_true
                                                              THEN <ls_update>-agent_info_provider
                                                              ELSE <ls_buffer>-instance-agent_info_provider ).
        <ls_buffer>-instance-system_prompt_provider = COND #( WHEN <ls_update>-control-system_prompt_provider = abap_true
                                                              THEN <ls_update>-system_prompt_provider
                                                              ELSE <ls_buffer>-instance-system_prompt_provider ).
        <ls_buffer>-instance-status                 = COND #( WHEN <ls_update>-control-status = abap_true
                                                              THEN <ls_update>-status
                                                              ELSE <ls_buffer>-instance-status ).
        <ls_buffer>-instance-created_by             = COND #( WHEN <ls_update>-control-created_by = abap_true
                                                              THEN <ls_update>-created_by
                                                              ELSE <ls_buffer>-instance-created_by ).
        <ls_buffer>-instance-created_at             = COND #( WHEN <ls_update>-control-created_at = abap_true
                                                              THEN <ls_update>-created_at
                                                              ELSE <ls_buffer>-instance-created_at ).
        <ls_buffer>-instance-changed_by             = COND #( WHEN <ls_update>-control-changed_by = abap_true
                                                              THEN <ls_update>-changed_by
                                                              ELSE <ls_buffer>-instance-changed_by ).
        <ls_buffer>-instance-last_changed           = COND #( WHEN <ls_update>-control-last_changed = abap_true
                                                              THEN <ls_update>-last_changed
                                                              ELSE <ls_buffer>-instance-last_changed ).
        <ls_buffer>-instance-local_last_changed     = COND #( WHEN <ls_update>-control-local_last_changed = abap_true
                                                              THEN <ls_update>-local_last_changed
                                                              ELSE <ls_buffer>-instance-local_last_changed ).

        <ls_buffer>-changed = abap_true.
        <ls_buffer>-deleted = abap_false.

        fill_agent_admin_fields( EXPORTING iv_during_create = abap_false
                                 CHANGING  cs_agent         = <ls_buffer> ).

      ELSE.
        APPEND VALUE #( agent_uuid = <ls_update>-agent_uuid
                        update     = abap_true
                        fail       = zpru_if_agent_frw=>cs_fail_cause-not_found )
               TO cs_failed-agent.

        APPEND VALUE #( agent_uuid = <ls_update>-agent_uuid
                        update     = abap_true
                        msg        = lo_util->new_message(
                                         iv_id       = zpru_if_agent_frw=>cs_message_class-zpru_msg_execution
                                         iv_number   = `012`
                                         iv_severity = zpru_if_agent_message=>sc_severity-error
                                         iv_v1       = <ls_update>-agent_uuid ) )
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
                                                    ( agent_uuid = <ls_k>-agent_uuid ) ) ).

    zpru_cl_adf_buffer=>prep_tool_buffer( VALUE #( FOR <ls_q>
                                                   IN     lt_entities
                                                   ( agent_uuid = <ls_q>-agent_uuid ) ) ).

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_delete>).

      ASSIGN zpru_cl_adf_buffer=>agent_buffer[ instance-agent_uuid = <ls_delete>-agent_uuid
                                               deleted             = abap_false ] TO FIELD-SYMBOL(<ls_buffer>).
      IF sy-subrc = 0.
        <ls_buffer>-deleted = abap_true.
        <ls_buffer>-changed = abap_true.

        LOOP AT zpru_cl_adf_buffer=>tool_buffer ASSIGNING FIELD-SYMBOL(<ls_tool_del>)
             WHERE instance-agent_uuid = <ls_buffer>-instance-agent_uuid.
          <ls_tool_del>-changed = abap_true.
          <ls_tool_del>-deleted = abap_true.
        ENDLOOP.

        APPEND VALUE #( msg        = lo_util->new_message(
                                         iv_id       = zpru_if_agent_frw=>cs_message_class-zpru_msg_execution
                                         iv_number   = `001`
                                         iv_severity = zpru_if_agent_message=>sc_severity-success
                                         iv_v1       = <ls_delete>-agent_uuid )
                        agent_uuid = <ls_delete>-agent_uuid ) TO cs_reported-agent.

      ELSE.
        APPEND VALUE #( agent_uuid = <ls_delete>-agent_uuid
                        delete     = abap_true
                        fail       = zpru_if_agent_frw=>cs_fail_cause-not_found )
               TO cs_failed-agent.

        APPEND VALUE #( agent_uuid = <ls_delete>-agent_uuid
                        delete     = abap_true
                        msg        = lo_util->new_message(
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
                                                    ( agent_uuid = <ls_k>-agent_uuid ) ) ).

    zpru_cl_adf_buffer=>prep_tool_buffer( VALUE #( FOR <ls_q>
                                                   IN     lt_entities
                                                   ( agent_uuid = <ls_q>-agent_uuid
                                                     tool_uuid  = <ls_q>-tool_uuid
                                                     full_key   = abap_true ) ) ).

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_create>).

      ASSIGN zpru_cl_adf_buffer=>agent_buffer[ instance-agent_uuid = <ls_create>-agent_uuid
                                               " TODO: variable is assigned but never used (ABAP cleaner)
                                               deleted             = abap_false ] TO FIELD-SYMBOL(<ls_parent>).
      IF sy-subrc = 0.
        IF    NOT line_exists( zpru_cl_adf_buffer=>tool_buffer[ instance-agent_uuid = <ls_create>-agent_uuid
                                                                instance-tool_uuid  = <ls_create>-tool_uuid ] )
           OR     line_exists( zpru_cl_adf_buffer=>tool_buffer[ instance-agent_uuid = <ls_create>-agent_uuid
                                                                instance-tool_uuid  = <ls_create>-tool_uuid
                                                                deleted             = abap_true ] ).

          ASSIGN zpru_cl_adf_buffer=>tool_buffer[ instance-agent_uuid = <ls_create>-agent_uuid
                                                  instance-tool_uuid  = <ls_create>-tool_uuid
                                                  deleted             = abap_false ] TO FIELD-SYMBOL(<ls_buffer>).
          IF sy-subrc = 0.
            DELETE zpru_cl_adf_buffer=>tool_buffer
                   WHERE     instance-agent_uuid = <ls_buffer>-instance-agent_uuid
                         AND instance-tool_uuid  = <ls_buffer>-instance-tool_uuid
                         AND deleted             = abap_true.
          ENDIF.

          APPEND VALUE #(
              instance-tool_uuid            = <ls_create>-tool_uuid
              instance-agent_uuid           = <ls_create>-agent_uuid
              instance-tool_name            = COND #( WHEN <ls_create>-control-tool_name = abap_true
                                                      THEN <ls_create>-tool_name )
              instance-tool_provider        = COND #( WHEN <ls_create>-control-tool_provider = abap_true
                                                      THEN <ls_create>-tool_provider )
              instance-step_type            = COND #( WHEN <ls_create>-control-step_type = abap_true
                                                      THEN <ls_create>-step_type )
              instance-tool_schema_provider = COND #( WHEN <ls_create>-control-tool_schema_provider = abap_true
                                                      THEN <ls_create>-tool_schema_provider )
              instance-tool_info_provider   = COND #( WHEN <ls_create>-control-tool_info_provider = abap_true
                                                      THEN <ls_create>-tool_info_provider )
              changed                       = abap_true
              deleted                       = abap_false ) TO zpru_cl_adf_buffer=>tool_buffer.

          APPEND VALUE #( agent_uuid = <ls_create>-agent_uuid
                          tool_uuid  = <ls_create>-tool_uuid ) TO cs_mapped-tool.

          APPEND VALUE #( msg        = lo_util->new_message(
                                           iv_id       = zpru_if_agent_frw=>cs_message_class-zpru_msg_execution
                                           iv_number   = `001`
                                           iv_severity = zpru_if_agent_message=>sc_severity-success
                                           iv_v1       = <ls_create>-tool_uuid )
                          agent_uuid = <ls_create>-agent_uuid
                          tool_uuid  = <ls_create>-tool_uuid ) TO cs_reported-tool.

        ELSE.
          APPEND VALUE #( agent_uuid = <ls_create>-agent_uuid
                          tool_uuid  = <ls_create>-tool_uuid
                          create     = abap_true
                          fail       = zpru_if_agent_frw=>cs_fail_cause-conflict )
                 TO cs_failed-tool.

          APPEND VALUE #( agent_uuid = <ls_create>-agent_uuid
                          tool_uuid  = <ls_create>-tool_uuid
                          create     = abap_true
                          msg        = lo_util->new_message(
                                           iv_id       = zpru_if_agent_frw=>cs_message_class-zpru_msg_execution
                                           iv_number   = `001`
                                           iv_severity = zpru_if_agent_message=>sc_severity-error
                                           iv_v1       = <ls_create>-tool_uuid ) )
                 TO cs_reported-tool.
        ENDIF.

      ELSE.
        APPEND VALUE #( agent_uuid = <ls_create>-agent_uuid
                        tool_uuid  = <ls_create>-tool_uuid
                        create     = abap_true
                        fail       = zpru_if_agent_frw=>cs_fail_cause-not_found )
               TO cs_failed-tool.

        APPEND VALUE #( agent_uuid = <ls_create>-agent_uuid
                        tool_uuid  = <ls_create>-tool_uuid
                        create     = abap_true
                        msg        = lo_util->new_message(
                                         iv_id       = zpru_if_agent_frw=>cs_message_class-zpru_msg_execution
                                         iv_number   = `002`
                                         iv_severity = zpru_if_agent_message=>sc_severity-error ) )
               TO cs_reported-tool.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_adf_service~rba_tool.
    DATA ls_out TYPE zpru_agent_tool.

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
                                                    ( agent_uuid = <ls_k>-agent_uuid ) ) ).

    zpru_cl_adf_buffer=>prep_tool_buffer( VALUE #( FOR <ls_q>
                                                   IN     lt_entities
                                                   ( agent_uuid = <ls_q>-agent_uuid ) ) ).

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_h>).
      LOOP AT zpru_cl_adf_buffer=>tool_buffer ASSIGNING FIELD-SYMBOL(<ls_t_buf>)
           WHERE     instance-agent_uuid = <ls_h>-agent_uuid
                 AND deleted             = abap_false.

        CLEAR ls_out.
        ls_out-tool_uuid            = <ls_t_buf>-instance-tool_uuid.
        ls_out-agent_uuid           = COND #( WHEN <ls_h>-control-agent_uuid = abap_true
                                              THEN <ls_t_buf>-instance-agent_uuid ).
        ls_out-tool_name            = COND #( WHEN <ls_h>-control-tool_name = abap_true
                                              THEN <ls_t_buf>-instance-tool_name ).
        ls_out-tool_provider        = COND #( WHEN <ls_h>-control-tool_provider = abap_true
                                              THEN <ls_t_buf>-instance-tool_provider ).
        ls_out-step_type            = COND #( WHEN <ls_h>-control-step_type = abap_true
                                              THEN <ls_t_buf>-instance-step_type ).
        ls_out-tool_schema_provider = COND #( WHEN <ls_h>-control-tool_schema_provider = abap_true
                                              THEN <ls_t_buf>-instance-tool_schema_provider ).
        ls_out-tool_info_provider   = COND #( WHEN <ls_h>-control-tool_info_provider = abap_true
                                              THEN <ls_t_buf>-instance-tool_info_provider ).

        APPEND ls_out TO et_tool.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_adf_service~read_tool.
    DATA ls_out TYPE zpru_agent_tool.

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
                                                    ( agent_uuid = <ls_k>-agent_uuid ) ) ).

    zpru_cl_adf_buffer=>prep_tool_buffer( VALUE #( FOR <ls_q>
                                                   IN     lt_entities
                                                   ( agent_uuid = <ls_q>-agent_uuid
                                                     tool_uuid  = <ls_q>-tool_uuid
                                                     full_key   = abap_true ) ) ).

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_read>).
      ASSIGN zpru_cl_adf_buffer=>agent_buffer[ instance-agent_uuid = <ls_read>-agent_uuid ] TO FIELD-SYMBOL(<ls_parent>).
      IF sy-subrc = 0 AND <ls_parent>-deleted = abap_true.
        APPEND VALUE #( agent_uuid = <ls_read>-agent_uuid
                        tool_uuid  = <ls_read>-tool_uuid
                        fail       = zpru_if_agent_frw=>cs_fail_cause-not_found )
               TO cs_failed-tool.
        CONTINUE.
      ENDIF.

      ASSIGN zpru_cl_adf_buffer=>tool_buffer[ instance-agent_uuid = <ls_read>-agent_uuid
                                              instance-tool_uuid  = <ls_read>-tool_uuid ] TO FIELD-SYMBOL(<ls_buffer>).
      IF sy-subrc = 0.
        IF <ls_buffer>-deleted = abap_true.
          APPEND VALUE #( agent_uuid = <ls_read>-agent_uuid
                          tool_uuid  = <ls_read>-tool_uuid
                          fail       = zpru_if_agent_frw=>cs_fail_cause-not_found )
                 TO cs_failed-tool.
          CONTINUE.
        ENDIF.

        CLEAR ls_out.
        ls_out-tool_uuid            = <ls_buffer>-instance-tool_uuid.
        ls_out-agent_uuid           = COND #( WHEN <ls_read>-control-agent_uuid = abap_true
                                              THEN <ls_buffer>-instance-agent_uuid ).
        ls_out-tool_name            = COND #( WHEN <ls_read>-control-tool_name = abap_true
                                              THEN <ls_buffer>-instance-tool_name ).
        ls_out-tool_provider        = COND #( WHEN <ls_read>-control-tool_provider = abap_true
                                              THEN <ls_buffer>-instance-tool_provider ).
        ls_out-step_type            = COND #( WHEN <ls_read>-control-step_type = abap_true
                                              THEN <ls_buffer>-instance-step_type ).
        ls_out-tool_schema_provider = COND #( WHEN <ls_read>-control-tool_schema_provider = abap_true
                                              THEN <ls_buffer>-instance-tool_schema_provider ).
        ls_out-tool_info_provider   = COND #( WHEN <ls_read>-control-tool_info_provider = abap_true
                                              THEN <ls_buffer>-instance-tool_info_provider ).

        APPEND ls_out TO et_tool.

      ELSE.
        APPEND VALUE #( agent_uuid = <ls_read>-agent_uuid
                        tool_uuid  = <ls_read>-tool_uuid
                        fail       = zpru_if_agent_frw=>cs_fail_cause-not_found )
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
                                                    ( agent_uuid = <ls_k>-agent_uuid ) ) ).

    zpru_cl_adf_buffer=>prep_tool_buffer( VALUE #( FOR <ls_q>
                                                   IN     lt_entities
                                                   ( agent_uuid = <ls_q>-agent_uuid
                                                     tool_uuid  = <ls_q>-tool_uuid
                                                     full_key   = abap_true ) ) ).

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_update>).

      ASSIGN zpru_cl_adf_buffer=>agent_buffer[ instance-agent_uuid = <ls_update>-agent_uuid ] TO FIELD-SYMBOL(<ls_parent>).
      IF sy-subrc = 0 AND <ls_parent>-deleted = abap_true.
        APPEND VALUE #( agent_uuid = <ls_update>-agent_uuid
                        tool_uuid  = <ls_update>-tool_uuid
                        update     = abap_true
                        fail       = zpru_if_agent_frw=>cs_fail_cause-not_found )
               TO cs_failed-tool.
        CONTINUE.
      ENDIF.

      ASSIGN zpru_cl_adf_buffer=>tool_buffer[ instance-agent_uuid = <ls_update>-agent_uuid
                                              instance-tool_uuid  = <ls_update>-tool_uuid ] TO FIELD-SYMBOL(<ls_buffer>).
      IF sy-subrc = 0 AND <ls_buffer>-deleted = abap_false.
        <ls_buffer>-instance-tool_name            = COND #( WHEN <ls_update>-control-tool_name = abap_true
                                                            THEN <ls_update>-tool_name
                                                            ELSE <ls_buffer>-instance-tool_name ).
        <ls_buffer>-instance-tool_provider        = COND #( WHEN <ls_update>-control-tool_provider = abap_true
                                                            THEN <ls_update>-tool_provider
                                                            ELSE <ls_buffer>-instance-tool_provider ).
        <ls_buffer>-instance-step_type            = COND #( WHEN <ls_update>-control-step_type = abap_true
                                                            THEN <ls_update>-step_type
                                                            ELSE <ls_buffer>-instance-step_type ).
        <ls_buffer>-instance-tool_schema_provider = COND #( WHEN <ls_update>-control-tool_schema_provider = abap_true
                                                            THEN <ls_update>-tool_schema_provider
                                                            ELSE <ls_buffer>-instance-tool_schema_provider ).
        <ls_buffer>-instance-tool_info_provider   = COND #( WHEN <ls_update>-control-tool_info_provider = abap_true
                                                            THEN <ls_update>-tool_info_provider
                                                            ELSE <ls_buffer>-instance-tool_info_provider ).
        <ls_buffer>-changed = abap_true.

      ELSE.
        APPEND VALUE #( agent_uuid = <ls_update>-agent_uuid
                        tool_uuid  = <ls_update>-tool_uuid
                        update     = abap_true
                        fail       = zpru_if_agent_frw=>cs_fail_cause-not_found )
               TO cs_failed-tool.

        APPEND VALUE #( agent_uuid = <ls_update>-agent_uuid
                        tool_uuid  = <ls_update>-tool_uuid
                        update     = abap_true
                        msg        = lo_util->new_message(
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
                                                    ( agent_uuid = <ls_k>-agent_uuid ) ) ).

    zpru_cl_adf_buffer=>prep_tool_buffer( VALUE #( FOR <ls_q>
                                                   IN     lt_entities
                                                   ( agent_uuid = <ls_q>-agent_uuid
                                                     tool_uuid  = <ls_q>-tool_uuid
                                                     full_key   = abap_true ) ) ).

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_delete>).

      ASSIGN zpru_cl_adf_buffer=>agent_buffer[ instance-agent_uuid = <ls_delete>-agent_uuid ] TO FIELD-SYMBOL(<ls_parent>).
      IF sy-subrc = 0 AND <ls_parent>-deleted = abap_true.
        " Parent deleted implies child deleted.
      ELSE.

        ASSIGN zpru_cl_adf_buffer=>tool_buffer[ instance-agent_uuid = <ls_delete>-agent_uuid
                                                instance-tool_uuid  = <ls_delete>-tool_uuid
                                                deleted             = abap_false ] TO FIELD-SYMBOL(<ls_buffer>).
        IF sy-subrc = 0.
          <ls_buffer>-deleted = abap_true.
          <ls_buffer>-changed = abap_true.
        ELSE.
          APPEND VALUE #( agent_uuid = <ls_delete>-agent_uuid
                          tool_uuid  = <ls_delete>-tool_uuid
                          delete     = abap_true
                          fail       = zpru_if_agent_frw=>cs_fail_cause-not_found )
                 TO cs_failed-tool.

          APPEND VALUE #( agent_uuid = <ls_delete>-agent_uuid
                          tool_uuid  = <ls_delete>-tool_uuid
                          delete     = abap_true
                          msg        = lo_util->new_message(
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
    LOOP AT ct_delete_agent ASSIGNING FIELD-SYMBOL(<ls_agent>).
      zpru_cl_adf_buffer=>prep_tool_buffer( VALUE #( ( agent_uuid = <ls_agent>-agent_uuid ) ) ).

      LOOP AT zpru_cl_adf_buffer=>tool_buffer ASSIGNING FIELD-SYMBOL(<ls_buf_tool>) WHERE instance-agent_uuid = <ls_agent>-agent_uuid.
        IF NOT line_exists( ct_delete_tool[ tool_uuid = <ls_buf_tool>-instance-tool_uuid ] ).
          APPEND <ls_buf_tool>-instance TO ct_delete_tool.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD collect_changes.
    LOOP AT zpru_cl_adf_buffer=>agent_buffer ASSIGNING FIELD-SYMBOL(<ls_agent>) WHERE changed = abap_true.
      IF <ls_agent>-deleted = abap_true.
        APPEND <ls_agent>-instance TO et_delete_agent.
      ELSE.
        APPEND <ls_agent>-instance TO et_modify_agent.
      ENDIF.
    ENDLOOP.

    LOOP AT zpru_cl_adf_buffer=>tool_buffer ASSIGNING FIELD-SYMBOL(<ls_tool>) WHERE changed = abap_true.
      IF <ls_tool>-deleted = abap_true.
        APPEND <ls_tool>-instance TO et_delete_tool.
      ELSE.
        APPEND <ls_tool>-instance TO et_modify_tool.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD apply_db_changes.
    rv_error = abap_false.

    IF it_modify_agent IS NOT INITIAL.
      MODIFY zpru_agent FROM TABLE @it_modify_agent.
      IF sy-subrc <> 0.
        rv_error = abap_true.
      ENDIF.
    ENDIF.

    IF it_delete_agent IS NOT INITIAL.
      DELETE zpru_agent FROM TABLE @it_delete_agent.
      IF sy-subrc <> 0.
        rv_error = abap_true.
      ENDIF.
    ENDIF.

    IF it_modify_tool IS NOT INITIAL.
      MODIFY zpru_agent_tool FROM TABLE @it_modify_tool.
      IF sy-subrc <> 0.
        rv_error = abap_true.
      ENDIF.
    ENDIF.

    IF it_delete_tool IS NOT INITIAL.
      DELETE zpru_agent_tool FROM TABLE @it_delete_tool.
      IF sy-subrc <> 0.
        rv_error = abap_true.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD fill_agent_admin_fields.
    GET TIME STAMP FIELD DATA(lv_now).

    IF iv_during_create = abap_true.
      cs_agent-instance-created_by = COND #( WHEN cs_agent-instance-created_by IS INITIAL
                                             THEN sy-uname
                                             ELSE cs_agent-instance-created_by ).
      cs_agent-instance-created_at = COND #( WHEN cs_agent-instance-created_at IS INITIAL
                                             THEN lv_now
                                             ELSE cs_agent-instance-created_at ).
    ENDIF.

    cs_agent-instance-last_changed       = lv_now.
    cs_agent-instance-changed_by         = sy-uname.
    cs_agent-instance-local_last_changed = lv_now.
  ENDMETHOD.

  METHOD calculate_triggers.
    " TODO: parameter CS_REPORTED is never used or assigned (ABAP cleaner)
    " TODO: parameter CS_FAILED is never used or assigned (ABAP cleaner)

    DATA lt_agent_2_proc              LIKE zpru_cl_adf_buffer=>agent_buffer.
    DATA lt_tool_2_proc               LIKE zpru_cl_adf_buffer=>tool_buffer.
    DATA lo_agent_descr               TYPE REF TO cl_abap_structdescr.
    DATA lo_tool_descr                TYPE REF TO cl_abap_structdescr.
    DATA lt_check_decision_provider_v TYPE zpru_if_adf_type_and_constant=>tt_agent_read_k. " create and update field "decision_provider"
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA lt_check_short_memory_v      TYPE zpru_if_adf_type_and_constant=>tt_agent_read_k. " create and update field "short_memory_provider"
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA lt_check_long_memory_v       TYPE zpru_if_adf_type_and_constant=>tt_agent_read_k. " create and update field "long_memory_provider"
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA lt_check_agent_info_v        TYPE zpru_if_adf_type_and_constant=>tt_agent_read_k. " create and update field "agent_info_provider"

    CLEAR et_check_decision_provider_v.
    CLEAR et_check_short_memory_v.
    CLEAR et_check_long_memory_v.
    CLEAR et_check_agent_info_v.

    LOOP AT zpru_cl_adf_buffer=>agent_buffer ASSIGNING FIELD-SYMBOL(<ls_agent_buffer>).
      APPEND INITIAL LINE TO lt_agent_2_proc ASSIGNING FIELD-SYMBOL(<ls_agent_2_process>).
      <ls_agent_2_process>-instance = CORRESPONDING #( <ls_agent_buffer>-instance ).
      <ls_agent_2_process>-deleted  = <ls_agent_buffer>-deleted.

      LOOP AT zpru_cl_adf_buffer=>tool_buffer ASSIGNING FIELD-SYMBOL(<ls_tool_buffer>)
           WHERE instance-agent_uuid = <ls_agent_buffer>-instance-agent_uuid.
        APPEND INITIAL LINE TO lt_tool_2_proc ASSIGNING FIELD-SYMBOL(<ls_tool_2_process>).
        <ls_tool_2_process>-instance = CORRESPONDING #( <ls_tool_buffer>-instance ).
        <ls_tool_2_process>-deleted  = <ls_tool_buffer>-deleted.
      ENDLOOP.
    ENDLOOP.

    IF lt_agent_2_proc IS INITIAL AND lt_tool_2_proc IS INITIAL.
      RETURN.
    ENDIF.

    IF lt_agent_2_proc IS NOT INITIAL.
      SELECT * FROM zpru_agent AS agent
        FOR ALL ENTRIES IN @lt_agent_2_proc
        WHERE agent~agent_uuid = @lt_agent_2_proc-instance-agent_uuid
        INTO TABLE @DATA(lt_agent_db_state).
    ENDIF.

    IF lt_tool_2_proc IS NOT INITIAL.
      SELECT * FROM zpru_agent_tool AS tool
        FOR ALL ENTRIES IN @lt_tool_2_proc
        WHERE tool~tool_uuid = @lt_tool_2_proc-instance-tool_uuid
        INTO TABLE @DATA(lt_tool_db_state).
    ENDIF.

    lo_agent_descr ?= cl_abap_structdescr=>describe_by_name( 'ZPRU_AGENT' ).
    DATA(lt_agent_fields) = lo_agent_descr->get_symbols( ).
    lo_tool_descr ?= cl_abap_structdescr=>describe_by_name( 'ZPRU_AGENT_TOOL' ).
    DATA(lt_tool_fields) = lo_tool_descr->get_symbols( ).

    LOOP AT lt_agent_2_proc ASSIGNING <ls_agent_2_process>.

      " calculate CREATE trigger
      IF     NOT line_exists( lt_agent_db_state[ agent_uuid = <ls_agent_2_process>-instance-agent_uuid ] )
         AND     <ls_agent_2_process>-deleted = abap_false.
        APPEND INITIAL LINE TO lt_check_decision_provider_v ASSIGNING FIELD-SYMBOL(<ls_check_decision_provider_v>).
        <ls_check_decision_provider_v> = CORRESPONDING #( <ls_agent_2_process>-instance ).

        APPEND INITIAL LINE TO lt_check_short_memory_v ASSIGNING FIELD-SYMBOL(<ls_check_short_memory_v>).
        <ls_check_short_memory_v> = CORRESPONDING #( <ls_agent_2_process>-instance ).

        APPEND INITIAL LINE TO lt_check_long_memory_v ASSIGNING FIELD-SYMBOL(<ls_check_long_memory_v>).
        <ls_check_long_memory_v> = CORRESPONDING #( <ls_agent_2_process>-instance ).

        APPEND INITIAL LINE TO lt_check_agent_info_v ASSIGNING FIELD-SYMBOL(<ls_check_agent_info_v>).
        <ls_check_agent_info_v> = CORRESPONDING #( <ls_agent_2_process>-instance ).
      ENDIF.

      ASSIGN lt_agent_db_state[ agent_uuid = <ls_agent_2_process>-instance-agent_uuid ] TO FIELD-SYMBOL(<ls_agent_db_state>).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      " calculate DELETE trigger before update
      " just skip deleted entries
      IF <ls_agent_2_process>-deleted = abap_true.
        CONTINUE.
      ENDIF.

      " calculate UPDATE trigger
      LOOP AT lt_agent_fields ASSIGNING FIELD-SYMBOL(<lv_agent_fields>).

        ASSIGN COMPONENT <lv_agent_fields>-name OF STRUCTURE <ls_agent_2_process>-instance TO FIELD-SYMBOL(<lv_buffer_value>).
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.

        ASSIGN COMPONENT <lv_agent_fields>-name OF STRUCTURE <ls_agent_db_state> TO FIELD-SYMBOL(<lv_db_value>).
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.

        IF <lv_buffer_value> <> <lv_db_value>.
*          APPEND INITIAL LINE TO validation / determination
          EXIT.
        ENDIF.
      ENDLOOP.

      " FIELD decision_provider
      ASSIGN COMPONENT 'DECISION_PROVIDER' OF STRUCTURE <ls_agent_2_process>-instance TO <lv_buffer_value>.
      IF sy-subrc = 0.
        ASSIGN COMPONENT 'DECISION_PROVIDER' OF STRUCTURE <ls_agent_db_state> TO <lv_db_value>.
        IF sy-subrc = 0.
          IF <lv_buffer_value> <> <lv_db_value>.
            APPEND INITIAL LINE TO lt_check_decision_provider_v ASSIGNING <ls_check_decision_provider_v>.
            <ls_check_decision_provider_v> = CORRESPONDING #( <ls_agent_2_process>-instance ).
          ENDIF.
        ENDIF.
      ENDIF.

      " FIELD short_memory_provider
      ASSIGN COMPONENT 'SHORT_MEMORY_PROVIDER' OF STRUCTURE <ls_agent_2_process>-instance TO <lv_buffer_value>.
      IF sy-subrc = 0.
        ASSIGN COMPONENT 'SHORT_MEMORY_PROVIDER' OF STRUCTURE <ls_agent_db_state> TO <lv_db_value>.
        IF sy-subrc = 0.
          IF <lv_buffer_value> <> <lv_db_value>.
            APPEND INITIAL LINE TO lt_check_short_memory_v ASSIGNING <ls_check_short_memory_v>.
            <ls_check_short_memory_v> = CORRESPONDING #( <ls_agent_2_process>-instance ).
          ENDIF.
        ENDIF.
      ENDIF.

      " FIELD long_memory_provider
      ASSIGN COMPONENT 'LONG_MEMORY_PROVIDER' OF STRUCTURE <ls_agent_2_process>-instance TO <lv_buffer_value>.
      IF sy-subrc = 0.
        ASSIGN COMPONENT 'LONG_MEMORY_PROVIDER' OF STRUCTURE <ls_agent_db_state> TO <lv_db_value>.
        IF sy-subrc = 0.
          IF <lv_buffer_value> <> <lv_db_value>.
            APPEND INITIAL LINE TO lt_check_long_memory_v ASSIGNING <ls_check_long_memory_v>.
            <ls_check_long_memory_v> = CORRESPONDING #( <ls_agent_2_process>-instance ).
          ENDIF.
        ENDIF.
      ENDIF.

      " FIELD agent_info_provider
      ASSIGN COMPONENT 'AGENT_INFO_PROVIDER' OF STRUCTURE <ls_agent_2_process>-instance TO <lv_buffer_value>.
      IF sy-subrc = 0.
        ASSIGN COMPONENT 'AGENT_INFO_PROVIDER' OF STRUCTURE <ls_agent_db_state> TO <lv_db_value>.
        IF sy-subrc = 0.
          IF <lv_buffer_value> <> <lv_db_value>.
            APPEND INITIAL LINE TO lt_check_agent_info_v ASSIGNING <ls_check_agent_info_v>.
            <ls_check_agent_info_v> = CORRESPONDING #( <ls_agent_2_process>-instance ).
          ENDIF.
        ENDIF.
      ENDIF.

    ENDLOOP.

    LOOP AT lt_tool_2_proc ASSIGNING <ls_tool_2_process>.

      " CREATE
      IF     NOT line_exists( lt_tool_db_state[ tool_uuid = <ls_tool_2_process>-instance-tool_uuid ] )
         AND     <ls_tool_2_process>-deleted = abap_false.
*        APPEND INITIAL LINE TO validation / determination
      ENDIF.

      ASSIGN lt_tool_db_state[ tool_uuid = <ls_tool_2_process>-instance-tool_uuid ] TO FIELD-SYMBOL(<ls_tool_db_state>).
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      " calc DELETE trigger before update
      " just skip deleted entries
      IF <ls_tool_2_process>-deleted = abap_true.
        CONTINUE.
      ENDIF.

      " UPDATE
      LOOP AT lt_tool_fields ASSIGNING FIELD-SYMBOL(<ls_tool_fields>).

        ASSIGN COMPONENT <ls_tool_fields>-name OF STRUCTURE <ls_tool_2_process>-instance TO <lv_buffer_value>.
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.

        ASSIGN COMPONENT <ls_tool_fields>-name OF STRUCTURE <ls_tool_db_state> TO <lv_db_value>.
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.

        IF <lv_buffer_value> <> <lv_db_value>.
*          APPEND INITIAL LINE TO validation / determination
          EXIT.
        ENDIF.
      ENDLOOP.

*      " FIELD field
*      ASSIGN COMPONENT 'FIELD' OF STRUCTURE <ls_tool_2_PROCESS>-instance TO <lv_buffer_value>.
*      IF sy-subrc = 0.
*        ASSIGN COMPONENT 'FIELD' OF STRUCTURE <ls_tool_db_state> TO <lv_db_value>.
*        IF sy-subrc = 0.
*          IF <lv_buffer_value> <> <lv_db_value>.
*            APPEND INITIAL LINE TO validation / determination
*          ENDIF.
*        ENDIF.
*      ENDIF.
    ENDLOOP.

    SORT lt_check_decision_provider_v BY table_line.
    DELETE ADJACENT DUPLICATES FROM lt_check_decision_provider_v COMPARING table_line.

    et_check_decision_provider_v = lt_check_decision_provider_v.
  ENDMETHOD.

ENDCLASS.
