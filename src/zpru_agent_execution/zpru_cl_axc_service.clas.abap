CLASS zpru_cl_axc_service DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zpru_if_agent_frw.
    INTERFACES zpru_if_axc_service.

  PROTECTED SECTION.
    METHODS precheck_create_header
      IMPORTING it_head_create_imp TYPE zpru_if_axc_type_and_constant=>tt_head_create_imp
      EXPORTING et_entities        TYPE zpru_if_axc_type_and_constant=>tt_head_create_imp
      CHANGING  cs_reported        TYPE zpru_if_agent_frw=>ts_axc_reported
                cs_failed          TYPE zpru_if_agent_frw=>ts_axc_failed.

    METHODS precheck_update_header
      IMPORTING it_head_update_imp TYPE zpru_if_axc_type_and_constant=>tt_head_update_imp
      EXPORTING et_entities        TYPE zpru_if_axc_type_and_constant=>tt_head_update_imp
      CHANGING  cs_reported        TYPE zpru_if_agent_frw=>ts_axc_reported
                cs_failed          TYPE zpru_if_agent_frw=>ts_axc_failed.

    METHODS precheck_delete_header
      IMPORTING it_head_delete_imp TYPE zpru_if_axc_type_and_constant=>tt_header_delete_imp
      EXPORTING et_entities        TYPE zpru_if_axc_type_and_constant=>tt_header_delete_imp
      CHANGING  cs_reported        TYPE zpru_if_agent_frw=>ts_axc_reported
                cs_failed          TYPE zpru_if_agent_frw=>ts_axc_failed.

    METHODS precheck_cba_query
      IMPORTING it_axc_query_imp TYPE zpru_if_axc_type_and_constant=>tt_query_create_imp
      EXPORTING et_entities      TYPE zpru_if_axc_type_and_constant=>tt_query_create_imp
      CHANGING  cs_reported      TYPE zpru_if_agent_frw=>ts_axc_reported
                cs_failed        TYPE zpru_if_agent_frw=>ts_axc_failed.

    METHODS precheck_read_header
      IMPORTING it_head_read_k TYPE zpru_if_axc_type_and_constant=>tt_head_read_k
      EXPORTING et_entities    TYPE zpru_if_axc_type_and_constant=>tt_head_read_k
      CHANGING  cs_reported    TYPE zpru_if_agent_frw=>ts_axc_reported
                cs_failed      TYPE zpru_if_agent_frw=>ts_axc_failed.

    METHODS precheck_read_query
      IMPORTING it_query_read_k TYPE zpru_if_axc_type_and_constant=>tt_query_read_k
      EXPORTING et_entities     TYPE zpru_if_axc_type_and_constant=>tt_query_read_k
      CHANGING  cs_reported     TYPE zpru_if_agent_frw=>ts_axc_reported
                cs_failed       TYPE zpru_if_agent_frw=>ts_axc_failed.

    METHODS precheck_update_query
      IMPORTING it_query_update_imp TYPE zpru_if_axc_type_and_constant=>tt_query_update_imp
      EXPORTING et_entities         TYPE zpru_if_axc_type_and_constant=>tt_query_update_imp
      CHANGING  cs_reported         TYPE zpru_if_agent_frw=>ts_axc_reported
                cs_failed           TYPE zpru_if_agent_frw=>ts_axc_failed.

    METHODS precheck_delete_query
      IMPORTING it_query_delete_imp TYPE zpru_if_axc_type_and_constant=>tt_query_delete_imp
      EXPORTING et_entities         TYPE zpru_if_axc_type_and_constant=>tt_query_delete_imp
      CHANGING  cs_reported         TYPE zpru_if_agent_frw=>ts_axc_reported
                cs_failed           TYPE zpru_if_agent_frw=>ts_axc_failed.

    METHODS precheck_rba_query
      IMPORTING it_rba_query_k TYPE zpru_if_axc_type_and_constant=>tt_rba_query_k
      EXPORTING et_entities    TYPE zpru_if_axc_type_and_constant=>tt_rba_query_k
      CHANGING  cs_reported    TYPE zpru_if_agent_frw=>ts_axc_reported
                cs_failed      TYPE zpru_if_agent_frw=>ts_axc_failed.

    METHODS precheck_cba_step
      IMPORTING it_axc_step_imp TYPE zpru_if_axc_type_and_constant=>tt_step_create_imp
      EXPORTING et_entities     TYPE zpru_if_axc_type_and_constant=>tt_step_create_imp
      CHANGING  cs_reported     TYPE zpru_if_agent_frw=>ts_axc_reported
                cs_failed       TYPE zpru_if_agent_frw=>ts_axc_failed.

    METHODS precheck_rba_step
      IMPORTING it_rba_step_k TYPE zpru_if_axc_type_and_constant=>tt_rba_step_k
      EXPORTING et_entities   TYPE zpru_if_axc_type_and_constant=>tt_rba_step_k
      CHANGING  cs_reported   TYPE zpru_if_agent_frw=>ts_axc_reported
                cs_failed     TYPE zpru_if_agent_frw=>ts_axc_failed.

    METHODS precheck_read_step
      IMPORTING it_step_read_k TYPE zpru_if_axc_type_and_constant=>tt_step_read_k
      EXPORTING et_entities    TYPE zpru_if_axc_type_and_constant=>tt_step_read_k
      CHANGING  cs_reported    TYPE zpru_if_agent_frw=>ts_axc_reported
                cs_failed      TYPE zpru_if_agent_frw=>ts_axc_failed.

    METHODS precheck_update_step
      IMPORTING it_step_update_imp TYPE zpru_if_axc_type_and_constant=>tt_step_update_imp
      EXPORTING et_entities        TYPE zpru_if_axc_type_and_constant=>tt_step_update_imp
      CHANGING  cs_reported        TYPE zpru_if_agent_frw=>ts_axc_reported
                cs_failed          TYPE zpru_if_agent_frw=>ts_axc_failed.

    METHODS precheck_delete_step
      IMPORTING it_step_delete_imp TYPE zpru_if_axc_type_and_constant=>tt_step_delete_imp
      EXPORTING et_entities        TYPE zpru_if_axc_type_and_constant=>tt_step_delete_imp
      CHANGING  cs_reported        TYPE zpru_if_agent_frw=>ts_axc_reported
                cs_failed          TYPE zpru_if_agent_frw=>ts_axc_failed.

    METHODS db_modify
      IMPORTING iv_do_commit    TYPE abap_boolean
      CHANGING  cs_reported     TYPE zpru_if_agent_frw=>ts_axc_reported
                cs_failed       TYPE zpru_if_agent_frw=>ts_axc_failed
                cs_mapped       TYPE zpru_if_agent_frw=>ts_axc_mapped
      RETURNING VALUE(rv_error) TYPE abap_boolean.

ENDCLASS.


CLASS zpru_cl_axc_service IMPLEMENTATION.
  METHOD db_modify.
    " db_modify is kept for compatibility with do_save, but logic is now in CRUD methods via EML.
    rv_error = abap_false.
  ENDMETHOD.



  METHOD zpru_if_axc_service~cba_step.
    DATA lo_util TYPE REF TO zpru_if_agent_util.

    IF it_axc_step_imp IS INITIAL.
      RETURN.
    ENDIF.

    precheck_cba_step( EXPORTING it_axc_step_imp = it_axc_step_imp
                       IMPORTING et_entities     = DATA(lt_entities)
                       CHANGING  cs_reported     = cs_reported
                                 cs_failed       = cs_failed ).

    IF lt_entities IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        lo_util ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_AGENT_UTIL`
                                                            iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    zpru_cl_axc_buffer=>prep_step_buffer( VALUE #( FOR <ls_k>
                                                   IN lt_entities
                                                   ( query_uuid = <ls_k>-query_uuid ) ) ).

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_create>).

      IF    NOT line_exists( zpru_cl_axc_buffer=>step_buffer[ instance-query_uuid = <ls_create>-query_uuid
                                                              instance-step_uuid  = <ls_create>-step_uuid ] )
         OR     line_exists( zpru_cl_axc_buffer=>step_buffer[ instance-query_uuid = <ls_create>-query_uuid
                                                              instance-step_uuid  = <ls_create>-step_uuid
                                                              deleted             = abap_true ] ).

        ASSIGN zpru_cl_axc_buffer=>step_buffer[ instance-query_uuid = <ls_create>-query_uuid
                                                instance-step_uuid  = <ls_create>-step_uuid
                                                deleted             = abap_false ] TO FIELD-SYMBOL(<ls_buffer>).
        IF sy-subrc = 0.
          DELETE zpru_cl_axc_buffer=>step_buffer
                 WHERE     instance-query_uuid = <ls_buffer>-instance-query_uuid
                       AND instance-step_uuid  = <ls_buffer>-instance-step_uuid
                       AND deleted             = abap_true.
        ENDIF.

        APPEND VALUE #( instance-step_uuid       = <ls_create>-step_uuid
                        instance-query_uuid      = COND #( WHEN <ls_create>-control-query_uuid = abap_true
                                                           THEN <ls_create>-query_uuid )
                        instance-run_uuid        = COND #( WHEN <ls_create>-control-run_uuid = abap_true
                                                           THEN <ls_create>-run_uuid )
                        instance-tool_uuid       = COND #( WHEN <ls_create>-control-tool_uuid = abap_true
                                                           THEN <ls_create>-tool_uuid )
                        instance-step_number     = COND #( WHEN <ls_create>-control-step_number = abap_true
                                                           THEN <ls_create>-step_number )
                        instance-execution_seq   = COND #( WHEN <ls_create>-control-execution_seq = abap_true
                                                           THEN <ls_create>-execution_seq )
                        instance-start_timestamp = COND #( WHEN <ls_create>-control-start_timestamp = abap_true
                                                           THEN <ls_create>-start_timestamp )
                        instance-end_timestamp   = COND #( WHEN <ls_create>-control-end_timestamp = abap_true
                                                           THEN <ls_create>-end_timestamp )
                        instance-step_status     = COND #( WHEN <ls_create>-control-step_status = abap_true
                                                           THEN <ls_create>-step_status )
                        instance-input_prompt    = COND #( WHEN <ls_create>-control-input_prompt = abap_true
                                                           THEN <ls_create>-input_prompt )
                        instance-output_prompt   = COND #( WHEN <ls_create>-control-output_prompt = abap_true
                                                           THEN <ls_create>-output_prompt )
                        changed                  = abap_true
                        " TODO: variable is assigned but never used (ABAP cleaner)
                        deleted                  = abap_false ) TO zpru_cl_axc_buffer=>step_buffer ASSIGNING FIELD-SYMBOL(<ls_just_added>).

        INSERT VALUE #( step_uuid = <ls_create>-step_uuid ) INTO TABLE cs_mapped-step.

        APPEND VALUE #( msg       = lo_util->new_message(
                                        iv_id       = zpru_if_agent_frw=>cs_message_class-zpru_msg_execution
                                        iv_number   = `007`
                                        iv_severity = zpru_if_agent_message=>sc_severity-success
                                        iv_v1       = <ls_create>-step_uuid )
                        step_uuid = <ls_create>-step_uuid ) TO cs_reported-step.

      ELSE.

        APPEND VALUE #( step_uuid = <ls_create>-step_uuid
                        create    = abap_true
                        fail      = zpru_if_agent_frw=>cs_fail_cause-conflict )
               TO cs_failed-step.

        APPEND VALUE #( step_uuid = <ls_create>-step_uuid
                        create    = abap_true
                        msg       = lo_util->new_message(
                                        iv_id       = zpru_if_agent_frw=>cs_message_class-zpru_msg_execution
                                        iv_number   = `005`
                                        iv_severity = zpru_if_agent_message=>sc_severity-error
                                        iv_v1       = <ls_create>-step_uuid ) )
               TO cs_reported-step.

      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_axc_service~rba_step.
    DATA ls_out TYPE zpru_axc_step.

    CLEAR et_axc_step.

    IF it_rba_step_k IS INITIAL.
      RETURN.
    ENDIF.

    precheck_rba_step( EXPORTING it_rba_step_k = it_rba_step_k
                       IMPORTING et_entities   = DATA(lt_entities)
                       CHANGING  cs_reported   = cs_reported
                                 cs_failed     = cs_failed ).

    IF lt_entities IS INITIAL.
      RETURN.
    ENDIF.

    zpru_cl_axc_buffer=>prep_step_buffer( VALUE #( FOR <ls_k> IN lt_entities
                                                   ( query_uuid = <ls_k>-query_uuid ) ) ).

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_h>).
      LOOP AT zpru_cl_axc_buffer=>step_buffer ASSIGNING FIELD-SYMBOL(<ls_s_buf>)
           WHERE     instance-query_uuid = <ls_h>-query_uuid
                 AND deleted             = abap_false.

        CLEAR ls_out.
        ls_out-step_uuid       = <ls_s_buf>-instance-step_uuid.
        ls_out-query_uuid      = COND #( WHEN <ls_h>-control-query_uuid = abap_true
                                         THEN <ls_s_buf>-instance-query_uuid ).
        ls_out-run_uuid        = COND #( WHEN <ls_h>-control-run_uuid = abap_true
                                         THEN <ls_s_buf>-instance-run_uuid ).
        ls_out-tool_uuid       = COND #( WHEN <ls_h>-control-tool_uuid = abap_true
                                         THEN <ls_s_buf>-instance-tool_uuid ).
        ls_out-step_number     = COND #( WHEN <ls_h>-control-step_number = abap_true
                                         THEN <ls_s_buf>-instance-step_number ).
        ls_out-execution_seq   = COND #( WHEN <ls_h>-control-execution_seq = abap_true
                                         THEN <ls_s_buf>-instance-execution_seq ).
        ls_out-start_timestamp = COND #( WHEN <ls_h>-control-start_timestamp = abap_true
                                         THEN <ls_s_buf>-instance-start_timestamp ).
        ls_out-end_timestamp   = COND #( WHEN <ls_h>-control-end_timestamp = abap_true
                                         THEN <ls_s_buf>-instance-end_timestamp ).
        ls_out-step_status     = COND #( WHEN <ls_h>-control-step_status = abap_true
                                         THEN <ls_s_buf>-instance-step_status ).
        ls_out-input_prompt    = COND #( WHEN <ls_h>-control-input_prompt = abap_true
                                         THEN <ls_s_buf>-instance-input_prompt ).
        ls_out-output_prompt   = COND #( WHEN <ls_h>-control-output_prompt = abap_true
                                         THEN <ls_s_buf>-instance-output_prompt ).
        APPEND ls_out TO et_axc_step.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_axc_service~read_step.
    DATA ls_out  TYPE zpru_axc_step.
    DATA lo_util TYPE REF TO zpru_if_agent_util.

    CLEAR et_axc_step.

    IF it_step_read_k IS INITIAL.
      RETURN.
    ENDIF.

    precheck_read_step( EXPORTING it_step_read_k = it_step_read_k
                        IMPORTING et_entities    = DATA(lt_entities)
                        CHANGING  cs_reported    = cs_reported
                                  cs_failed      = cs_failed ).

    IF lt_entities IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        lo_util ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_AGENT_UTIL`
                                                            iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    zpru_cl_axc_buffer=>prep_step_buffer( VALUE #( FOR <ls_k> IN lt_entities
                                                   ( step_uuid = <ls_k>-step_uuid
                                                     full_key  = abap_true ) ) ).

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_ent>).
      ASSIGN zpru_cl_axc_buffer=>step_buffer[ instance-step_uuid = <ls_ent>-step_uuid
                                              deleted            = abap_false ] TO FIELD-SYMBOL(<ls_buf>).
      IF sy-subrc = 0.
        CLEAR ls_out.
        ls_out-step_uuid       = <ls_buf>-instance-step_uuid.
        ls_out-query_uuid      = COND #( WHEN <ls_ent>-control-query_uuid = abap_true
                                         THEN <ls_buf>-instance-query_uuid ).
        ls_out-run_uuid        = COND #( WHEN <ls_ent>-control-run_uuid = abap_true
                                         THEN <ls_buf>-instance-run_uuid ).
        ls_out-tool_uuid       = COND #( WHEN <ls_ent>-control-tool_uuid = abap_true
                                         THEN <ls_buf>-instance-tool_uuid ).
        ls_out-step_number     = COND #( WHEN <ls_ent>-control-step_number = abap_true
                                         THEN <ls_buf>-instance-step_number ).
        ls_out-execution_seq   = COND #( WHEN <ls_ent>-control-execution_seq = abap_true
                                         THEN <ls_buf>-instance-execution_seq ).
        ls_out-start_timestamp = COND #( WHEN <ls_ent>-control-start_timestamp = abap_true
                                         THEN <ls_buf>-instance-start_timestamp ).
        ls_out-end_timestamp   = COND #( WHEN <ls_ent>-control-end_timestamp = abap_true
                                         THEN <ls_buf>-instance-end_timestamp ).
        ls_out-step_status     = COND #( WHEN <ls_ent>-control-step_status = abap_true
                                         THEN <ls_buf>-instance-step_status ).
        ls_out-input_prompt    = COND #( WHEN <ls_ent>-control-input_prompt = abap_true
                                         THEN <ls_buf>-instance-input_prompt ).
        ls_out-output_prompt   = COND #( WHEN <ls_ent>-control-output_prompt = abap_true
                                         THEN <ls_buf>-instance-output_prompt ).

        APPEND ls_out TO et_axc_step.
      ELSE.
        APPEND VALUE #( step_uuid = <ls_ent>-step_uuid
                        fail      = zpru_if_agent_frw=>cs_fail_cause-not_found )
               TO cs_failed-step.

        APPEND VALUE #( step_uuid = <ls_ent>-step_uuid
                        msg       = lo_util->new_message(
                                        iv_id       = zpru_if_agent_frw=>cs_message_class-zpru_msg_execution
                                        iv_number   = `010`
                                        iv_severity = zpru_if_agent_message=>sc_severity-error
                                        iv_v1       = <ls_ent>-step_uuid ) )
               TO cs_reported-step.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_axc_service~update_step.
    DATA lo_util TYPE REF TO zpru_if_agent_util.

    IF it_step_update_imp IS INITIAL.
      RETURN.
    ENDIF.

    precheck_update_step( EXPORTING it_step_update_imp = it_step_update_imp
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

    zpru_cl_axc_buffer=>prep_step_buffer( VALUE #( FOR <ls_k> IN lt_entities
                                                   ( query_uuid = <ls_k>-query_uuid
                                                     step_uuid  = <ls_k>-step_uuid
                                                     full_key   = abap_true ) ) ).

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_update>).

      ASSIGN zpru_cl_axc_buffer=>step_buffer[ instance-query_uuid = <ls_update>-query_uuid
                                              instance-step_uuid  = <ls_update>-step_uuid
                                              deleted             = abap_false ] TO FIELD-SYMBOL(<ls_buf>).

      IF sy-subrc = 0.
        <ls_buf>-instance-query_uuid      = COND #( WHEN <ls_update>-control-query_uuid = abap_true
                                                    THEN <ls_update>-query_uuid
                                                    ELSE <ls_buf>-instance-query_uuid ).
        <ls_buf>-instance-run_uuid        = COND #( WHEN <ls_update>-control-run_uuid = abap_true
                                                    THEN <ls_update>-run_uuid
                                                    ELSE <ls_buf>-instance-run_uuid ).
        <ls_buf>-instance-tool_uuid       = COND #( WHEN <ls_update>-control-tool_uuid = abap_true
                                                    THEN <ls_update>-tool_uuid
                                                    ELSE <ls_buf>-instance-tool_uuid ).
        <ls_buf>-instance-step_number     = COND #( WHEN <ls_update>-control-step_number = abap_true
                                                    THEN <ls_update>-step_number
                                                    ELSE <ls_buf>-instance-step_number ).
        <ls_buf>-instance-execution_seq   = COND #( WHEN <ls_update>-control-execution_seq = abap_true
                                                    THEN <ls_update>-execution_seq
                                                    ELSE <ls_buf>-instance-execution_seq ).
        <ls_buf>-instance-start_timestamp = COND #( WHEN <ls_update>-control-start_timestamp = abap_true
                                                    THEN <ls_update>-start_timestamp
                                                    ELSE <ls_buf>-instance-start_timestamp ).
        <ls_buf>-instance-end_timestamp   = COND #( WHEN <ls_update>-control-end_timestamp = abap_true
                                                    THEN <ls_update>-end_timestamp
                                                    ELSE <ls_buf>-instance-end_timestamp ).
        <ls_buf>-instance-step_status     = COND #( WHEN <ls_update>-control-step_status = abap_true
                                                    THEN <ls_update>-step_status
                                                    ELSE <ls_buf>-instance-step_status ).
        <ls_buf>-instance-input_prompt    = COND #( WHEN <ls_update>-control-input_prompt = abap_true
                                                    THEN <ls_update>-input_prompt
                                                    ELSE <ls_buf>-instance-input_prompt ).
        <ls_buf>-instance-output_prompt   = COND #( WHEN <ls_update>-control-output_prompt = abap_true
                                                    THEN <ls_update>-output_prompt
                                                    ELSE <ls_buf>-instance-output_prompt ).

        <ls_buf>-changed = abap_true.
        <ls_buf>-deleted = abap_false.

      ELSE.
        APPEND VALUE #( step_uuid = <ls_update>-step_uuid
                        update    = abap_true
                        fail      = zpru_if_agent_frw=>cs_fail_cause-not_found )
               TO cs_failed-step.

        APPEND VALUE #( step_uuid = <ls_update>-step_uuid
                        update    = abap_true
                        msg       = lo_util->new_message(
                                        iv_id       = zpru_if_agent_frw=>cs_message_class-zpru_msg_execution
                                        iv_number   = `010`
                                        iv_severity = zpru_if_agent_message=>sc_severity-error
                                        iv_v1       = <ls_update>-query_uuid ) )
               TO cs_reported-step.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_axc_service~delete_step.
    DATA lo_util TYPE REF TO zpru_if_agent_util.

    IF it_step_delete_imp IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        lo_util ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_AGENT_UTIL`
                                                            iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    precheck_delete_step( EXPORTING it_step_delete_imp = it_step_delete_imp
                          IMPORTING et_entities        = DATA(lt_entities)
                          CHANGING  cs_reported        = cs_reported
                                    cs_failed          = cs_failed ).

    IF lt_entities IS INITIAL.
      RETURN.
    ENDIF.

    zpru_cl_axc_buffer=>prep_step_buffer( VALUE #( FOR <ls_k> IN lt_entities
                                                   ( step_uuid = <ls_k>-step_uuid
                                                     full_key  = abap_true ) ) ).

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_delete>).

      ASSIGN zpru_cl_axc_buffer=>step_buffer[ instance-step_uuid = <ls_delete>-step_uuid
                                              deleted            = abap_false ] TO FIELD-SYMBOL(<ls_buf>).
      IF sy-subrc = 0.
        <ls_buf>-deleted = abap_true.
        <ls_buf>-changed = abap_true.
      ELSE.
        APPEND VALUE #( step_uuid = <ls_delete>-step_uuid
                        delete    = abap_true
                        fail      = zpru_if_agent_frw=>cs_fail_cause-not_found )
               TO cs_failed-step.

        APPEND VALUE #( step_uuid = <ls_delete>-step_uuid
                        delete    = abap_true
                        msg       = lo_util->new_message(
                                        iv_id       = zpru_if_agent_frw=>cs_message_class-zpru_msg_execution
                                        iv_number   = `006`
                                        iv_severity = zpru_if_agent_message=>sc_severity-error
                                        iv_v1       = <ls_delete>-step_uuid ) )
               TO cs_reported-step.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_axc_service~determine.
    " Placeholder for business logic that determines what to persist.
    " Currently a no-op; callers populate buffers via CBA/UPDATE/DELETE flows.
    IF cs_mapped IS INITIAL.
      " leave as-is
    ENDIF.
  ENDMETHOD.

  METHOD zpru_if_axc_service~validate.
    " Placeholder for validation logic. Add domain-specific checks here.
    " For now, we keep this minimal: if there are existing failures, leave them.
    IF cs_failed-header IS NOT INITIAL OR cs_failed-query IS NOT INITIAL OR cs_failed-step IS NOT INITIAL.
      " existing failures already present
    ENDIF.
  ENDMETHOD.

  METHOD zpru_if_axc_service~clean_up.
    " Clear in-memory buffers and mapped entries after save/rollback.
    CLEAR cs_mapped.
    CLEAR: zpru_cl_axc_buffer=>header_buffer,
           zpru_cl_axc_buffer=>query_buffer,
           zpru_cl_axc_buffer=>step_buffer.
  ENDMETHOD.

  METHOD zpru_if_axc_service~do_save.
    DATA(lv_err) = abap_false.

    " Run determination step
    me->zpru_if_axc_service~determine( CHANGING cs_reported = cs_reported
                                                cs_failed   = cs_failed
                                                cs_mapped   = cs_mapped ).

    " Run validation
    me->zpru_if_axc_service~validate( CHANGING cs_reported = cs_reported
                                               cs_failed   = cs_failed ).

    " If validation produced failures, skip DB modification
    IF cs_failed-header IS NOT INITIAL OR cs_failed-query IS NOT INITIAL OR cs_failed-step IS NOT INITIAL.
      RETURN.
    ENDIF.

    " Persist buffers to DB (returns abap_true on error)
    lv_err = db_modify( EXPORTING iv_do_commit = iv_do_commit
                        CHANGING  cs_reported  = cs_reported
                                  cs_failed    = cs_failed
                                  cs_mapped    = cs_mapped ).

    IF lv_err = abap_true.
      IF iv_do_commit = abap_true.
        ROLLBACK WORK.
      ENDIF.
      " leave failure info in cs_failed
      me->zpru_if_axc_service~clean_up( CHANGING cs_mapped = cs_mapped ).
      RETURN.
    ENDIF.

    IF iv_do_commit = abap_true.
      COMMIT WORK AND WAIT.
    ENDIF.

    me->zpru_if_axc_service~clean_up( CHANGING cs_mapped = cs_mapped ).
  ENDMETHOD.

  METHOD zpru_if_axc_service~get_actual_query.
    CLEAR et_axc_head_query_link.

    IF it_axc_head_k IS INITIAL.
      RETURN.
    ENDIF.

    zpru_if_axc_service~rba_query(
      EXPORTING it_rba_query_k = VALUE #( FOR <ls_h> IN it_axc_head_k
                                          ( run_uuid = <ls_h>-run_uuid
                                            control  = VALUE #( run_uuid         = abap_true
                                                                query_number     = abap_true
                                                                query_uuid       = abap_true
                                                                language         = abap_true
                                                                execution_status = abap_true
                                                                start_timestamp  = abap_true
                                                                end_timestamp    = abap_true
                                                                input_prompt     = abap_true
                                                                decision_log     = abap_true
                                                                output_response  = abap_true )  ) )
      IMPORTING et_axc_query   = DATA(lt_query_candidates)
      CHANGING  cs_reported    = cs_reported
                cs_failed      = cs_failed ).

    LOOP AT it_axc_head_k ASSIGNING FIELD-SYMBOL(<ls_axc_head_k>).

      DATA(lt_query_copy) = lt_query_candidates.
      DELETE lt_query_copy WHERE run_uuid <> <ls_axc_head_k>-run_uuid.
      DELETE lt_query_copy WHERE execution_status <> zpru_if_axc_type_and_constant=>sc_query_status-new.

      SORT lt_query_copy BY start_timestamp ASCENDING.

      APPEND INITIAL LINE TO et_axc_head_query_link ASSIGNING FIELD-SYMBOL(<ls_axc_head_query_link>).
      <ls_axc_head_query_link>-run_uuid   = <ls_axc_head_k>-run_uuid.
      <ls_axc_head_query_link>-query_uuid = VALUE #( lt_query_copy[ 1 ]-query_uuid OPTIONAL ).

    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_axc_service~cba_query.
    DATA lo_util TYPE REF TO zpru_if_agent_util.

    IF it_axc_query_imp IS INITIAL.
      RETURN.
    ENDIF.

    precheck_cba_query( EXPORTING it_axc_query_imp = it_axc_query_imp
                        IMPORTING et_entities      = DATA(lt_entities)
                        CHANGING  cs_reported      = cs_reported
                                  cs_failed        = cs_failed ).

    IF lt_entities IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        lo_util ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_AGENT_UTIL`
                                                            iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    zpru_cl_axc_buffer=>prep_query_buffer( VALUE #( FOR <ls_k>
                                                    IN     lt_entities
                                                    ( run_uuid   = <ls_k>-run_uuid ) ) ).

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_create>).

      IF    NOT line_exists( zpru_cl_axc_buffer=>query_buffer[ instance-run_uuid   = <ls_create>-run_uuid
                                                               instance-query_uuid = <ls_create>-query_uuid ] )
         OR     line_exists( zpru_cl_axc_buffer=>query_buffer[ instance-run_uuid   = <ls_create>-run_uuid
                                                               instance-query_uuid = <ls_create>-query_uuid
                                                               deleted             = abap_true ] ).

        ASSIGN zpru_cl_axc_buffer=>query_buffer[ instance-run_uuid   = <ls_create>-run_uuid
                                                 instance-query_uuid = <ls_create>-query_uuid
                                                 deleted             = abap_true ] TO FIELD-SYMBOL(<ls_buffer>).
        IF sy-subrc = 0.
          DELETE zpru_cl_axc_buffer=>query_buffer
                 WHERE     instance-run_uuid   = <ls_buffer>-instance-run_uuid
                       AND instance-query_uuid = <ls_buffer>-instance-query_uuid
                       AND deleted             = abap_true.
        ENDIF.

        APPEND VALUE #( instance-query_uuid       = <ls_create>-query_uuid
                        instance-run_uuid         = COND #( WHEN <ls_create>-control-run_uuid = abap_true
                                                            THEN <ls_create>-run_uuid )
                        instance-language         = COND #( WHEN <ls_create>-control-language = abap_true
                                                            THEN <ls_create>-language )
                        instance-query_number     = COND #( WHEN <ls_create>-control-query_number = abap_true
                                                            THEN <ls_create>-query_number )
                        instance-execution_status = COND #( WHEN <ls_create>-control-execution_status = abap_true
                                                            THEN <ls_create>-execution_status )
                        instance-start_timestamp  = COND #( WHEN <ls_create>-control-start_timestamp = abap_true
                                                            THEN <ls_create>-start_timestamp )
                        instance-end_timestamp    = COND #( WHEN <ls_create>-control-end_timestamp = abap_true
                                                            THEN <ls_create>-end_timestamp )
                        instance-input_prompt     = COND #( WHEN <ls_create>-control-input_prompt = abap_true
                                                            THEN <ls_create>-input_prompt )
                        instance-decision_log     = COND #( WHEN <ls_create>-control-decision_log = abap_true
                                                            THEN <ls_create>-decision_log )
                        instance-output_response  = COND #( WHEN <ls_create>-control-output_response = abap_true
                                                            THEN <ls_create>-output_response )
                        changed                   = abap_true
                        deleted                   = abap_false ) TO zpru_cl_axc_buffer=>query_buffer ASSIGNING FIELD-SYMBOL(<ls_just_added>).

        GET TIME STAMP FIELD DATA(lv_now).
        <ls_just_added>-instance-start_timestamp = COND #( WHEN <ls_just_added>-instance-start_timestamp IS INITIAL
                                                           THEN lv_now
                                                           ELSE <ls_just_added>-instance-start_timestamp ).

        INSERT VALUE #( query_uuid = <ls_create>-query_uuid ) INTO TABLE cs_mapped-query.

        APPEND VALUE #( msg        = lo_util->new_message(
                                         iv_id       = zpru_if_agent_frw=>cs_message_class-zpru_msg_execution
                                         iv_number   = `008`
                                         iv_severity = zpru_if_agent_message=>sc_severity-success
                                         iv_v1       = <ls_just_added>-instance-query_uuid )
                        query_uuid = <ls_create>-query_uuid ) TO cs_reported-query.

      ELSE.

        APPEND VALUE #( query_uuid = <ls_create>-query_uuid
                        create     = abap_true
                        fail       = zpru_if_agent_frw=>cs_fail_cause-conflict )
               TO cs_failed-query.

        APPEND VALUE #( query_uuid = <ls_create>-query_uuid
                        create     = abap_true
                        msg        = lo_util->new_message(
                                         iv_id       = zpru_if_agent_frw=>cs_message_class-zpru_msg_execution
                                         iv_number   = `009`
                                         iv_severity = zpru_if_agent_message=>sc_severity-error
                                         iv_v1       = <ls_create>-query_uuid ) )
               TO cs_reported-query.

      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_axc_service~lock.
  ENDMETHOD.

  METHOD zpru_if_axc_service~rba_query.
    DATA ls_out TYPE zpru_axc_query.

    CLEAR et_axc_query.

    IF it_rba_query_k IS INITIAL.
      RETURN.
    ENDIF.

    precheck_rba_query( EXPORTING it_rba_query_k = it_rba_query_k
                        IMPORTING et_entities    = DATA(lt_entities)
                        CHANGING  cs_reported    = cs_reported
                                  cs_failed      = cs_failed ).

    IF lt_entities IS INITIAL.
      RETURN.
    ENDIF.

    zpru_cl_axc_buffer=>prep_query_buffer( VALUE #( FOR <ls_k> IN lt_entities
                                                    ( run_uuid   = <ls_k>-run_uuid ) ) ).

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_h>).
      LOOP AT zpru_cl_axc_buffer=>query_buffer ASSIGNING FIELD-SYMBOL(<ls_q_buf>)
           WHERE     instance-run_uuid = <ls_h>-run_uuid
                 AND deleted           = abap_false.

        CLEAR ls_out.

        ls_out-query_uuid       = <ls_q_buf>-instance-query_uuid.
        ls_out-run_uuid         = COND #( WHEN <ls_h>-control-run_uuid = abap_true
                                          THEN <ls_q_buf>-instance-run_uuid ).
        ls_out-language         = COND #( WHEN <ls_h>-control-language = abap_true
                                          THEN <ls_q_buf>-instance-language ).
        ls_out-query_number     = COND #( WHEN <ls_h>-control-query_number = abap_true
                                          THEN <ls_q_buf>-instance-query_number ).
        ls_out-start_timestamp  = COND #( WHEN <ls_h>-control-start_timestamp = abap_true
                                          THEN <ls_q_buf>-instance-start_timestamp ).
        ls_out-end_timestamp    = COND #( WHEN <ls_h>-control-end_timestamp = abap_true
                                          THEN <ls_q_buf>-instance-end_timestamp ).
        ls_out-execution_status = COND #( WHEN <ls_h>-control-execution_status = abap_true
                                          THEN <ls_q_buf>-instance-execution_status ).
        ls_out-input_prompt     = COND #( WHEN <ls_h>-control-input_prompt = abap_true
                                          THEN <ls_q_buf>-instance-input_prompt ).
        ls_out-decision_log     = COND #( WHEN <ls_h>-control-decision_log = abap_true
                                          THEN <ls_q_buf>-instance-decision_log ).
        ls_out-output_response  = COND #( WHEN <ls_h>-control-output_response = abap_true
                                          THEN <ls_q_buf>-instance-output_response ).

        APPEND ls_out TO et_axc_query.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_axc_service~read_query.
    DATA ls_out  TYPE zpru_axc_query.
    DATA lo_util TYPE REF TO zpru_if_agent_util.

    CLEAR et_axc_query.

    IF it_query_read_k IS INITIAL.
      RETURN.
    ENDIF.

    precheck_read_query( EXPORTING it_query_read_k = it_query_read_k
                         IMPORTING et_entities     = DATA(lt_entities)
                         CHANGING  cs_reported     = cs_reported
                                   cs_failed       = cs_failed ).

    IF lt_entities IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        lo_util ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_AGENT_UTIL`
                                                            iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    zpru_cl_axc_buffer=>prep_query_buffer( VALUE #( FOR <ls_q> IN lt_entities
                                                    ( query_uuid = <ls_q>-query_uuid
                                                      full_key   = abap_true ) ) ).

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_ent>).
      ASSIGN zpru_cl_axc_buffer=>query_buffer[ instance-query_uuid = <ls_ent>-query_uuid
                                               deleted             = abap_false ] TO FIELD-SYMBOL(<ls_buf>).
      IF sy-subrc = 0.
        CLEAR ls_out.
        ls_out-query_uuid       = <ls_buf>-instance-query_uuid.
        ls_out-run_uuid         = COND #( WHEN <ls_ent>-control-run_uuid = abap_true
                                          THEN <ls_buf>-instance-run_uuid ).
        ls_out-language         = COND #( WHEN <ls_ent>-control-language = abap_true
                                          THEN <ls_buf>-instance-language ).
        ls_out-query_number     = COND #( WHEN <ls_ent>-control-query_number = abap_true
                                          THEN <ls_buf>-instance-query_number ).
        ls_out-start_timestamp  = COND #( WHEN <ls_ent>-control-start_timestamp = abap_true
                                          THEN <ls_buf>-instance-start_timestamp ).
        ls_out-end_timestamp    = COND #( WHEN <ls_ent>-control-end_timestamp = abap_true
                                          THEN <ls_buf>-instance-end_timestamp ).
        ls_out-execution_status = COND #( WHEN <ls_ent>-control-execution_status = abap_true
                                          THEN <ls_buf>-instance-execution_status ).
        ls_out-input_prompt     = COND #( WHEN <ls_ent>-control-input_prompt = abap_true
                                          THEN <ls_buf>-instance-input_prompt ).
        ls_out-decision_log     = COND #( WHEN <ls_ent>-control-decision_log = abap_true
                                          THEN <ls_buf>-instance-decision_log ).
        ls_out-output_response  = COND #( WHEN <ls_ent>-control-output_response = abap_true
                                          THEN <ls_buf>-instance-output_response ).

        APPEND ls_out TO et_axc_query.
      ELSE.
        APPEND VALUE #( query_uuid = <ls_ent>-query_uuid
                        fail       = zpru_if_agent_frw=>cs_fail_cause-not_found )
               TO cs_failed-query.

        APPEND VALUE #( query_uuid = <ls_ent>-query_uuid
                        msg        = lo_util->new_message(
                                         iv_id       = zpru_if_agent_frw=>cs_message_class-zpru_msg_execution
                                         iv_number   = `011`
                                         iv_severity = zpru_if_agent_message=>sc_severity-error
                                         iv_v1       = <ls_ent>-query_uuid ) )
               TO cs_reported-query.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_axc_service~update_query.
    DATA lo_util TYPE REF TO zpru_if_agent_util.

    IF it_query_update_imp IS INITIAL.
      RETURN.
    ENDIF.

    precheck_update_query( EXPORTING it_query_update_imp = it_query_update_imp
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

    zpru_cl_axc_buffer=>prep_query_buffer( VALUE #( FOR <ls_k> IN lt_entities
                                                    ( run_uuid   = <ls_k>-run_uuid
                                                      query_uuid = <ls_k>-query_uuid
                                                      full_key   = abap_true ) ) ).

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_update>).

      ASSIGN zpru_cl_axc_buffer=>query_buffer[ instance-run_uuid   = <ls_update>-run_uuid
                                               instance-query_uuid = <ls_update>-query_uuid
                                               deleted             = abap_false ] TO FIELD-SYMBOL(<ls_buf>).

      IF sy-subrc = 0.
        <ls_buf>-instance-language         = COND #( WHEN <ls_update>-control-language = abap_true
                                                     THEN <ls_update>-language
                                                     ELSE <ls_buf>-instance-language ).
        <ls_buf>-instance-query_number     = COND #( WHEN <ls_update>-control-query_number = abap_true
                                                     THEN <ls_update>-query_number
                                                     ELSE <ls_buf>-instance-query_number ).
        <ls_buf>-instance-start_timestamp  = COND #( WHEN <ls_update>-control-start_timestamp = abap_true
                                                     THEN <ls_update>-start_timestamp
                                                     ELSE <ls_buf>-instance-start_timestamp ).
        <ls_buf>-instance-end_timestamp    = COND #( WHEN <ls_update>-control-end_timestamp = abap_true
                                                     THEN <ls_update>-end_timestamp
                                                     ELSE <ls_buf>-instance-end_timestamp ).
        <ls_buf>-instance-execution_status = COND #( WHEN <ls_update>-control-execution_status = abap_true
                                                     THEN <ls_update>-execution_status
                                                     ELSE <ls_buf>-instance-execution_status ).
        <ls_buf>-instance-input_prompt     = COND #( WHEN <ls_update>-control-input_prompt = abap_true
                                                     THEN <ls_update>-input_prompt
                                                     ELSE <ls_buf>-instance-input_prompt ).
        <ls_buf>-instance-decision_log     = COND #( WHEN <ls_update>-control-decision_log = abap_true
                                                     THEN <ls_update>-decision_log
                                                     ELSE <ls_buf>-instance-decision_log ).
        <ls_buf>-instance-output_response  = COND #( WHEN <ls_update>-control-output_response = abap_true
                                                     THEN <ls_update>-output_response
                                                     ELSE <ls_buf>-instance-output_response ).

        <ls_buf>-changed = abap_true.
        <ls_buf>-deleted = abap_false.

      ELSE.
        APPEND VALUE #( query_uuid = <ls_update>-query_uuid
                        update     = abap_true
                        fail       = zpru_if_agent_frw=>cs_fail_cause-not_found )
               TO cs_failed-query.

        APPEND VALUE #( query_uuid = <ls_update>-query_uuid
                        update     = abap_true
                        msg        = lo_util->new_message(
                                         iv_id       = zpru_if_agent_frw=>cs_message_class-zpru_msg_execution
                                         iv_number   = `011`
                                         iv_severity = zpru_if_agent_message=>sc_severity-error
                                         iv_v1       = <ls_update>-query_uuid ) )
               TO cs_reported-query.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_axc_service~delete_query.
    DATA lo_util TYPE REF TO zpru_if_agent_util.

    IF it_query_delete_imp IS INITIAL.
      RETURN.
    ENDIF.

    precheck_delete_query( EXPORTING it_query_delete_imp = it_query_delete_imp
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

    zpru_cl_axc_buffer=>prep_query_buffer( VALUE #( FOR <ls_k> IN lt_entities
                                                    ( query_uuid = <ls_k>-query_uuid
                                                      full_key   = abap_true ) ) ).

    zpru_cl_axc_buffer=>prep_step_buffer( VALUE #( FOR <ls_q> IN lt_entities
                                                   ( query_uuid = <ls_q>-query_uuid ) )  ).

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_delete>).

      ASSIGN zpru_cl_axc_buffer=>query_buffer[ instance-query_uuid = <ls_delete>-query_uuid
                                               deleted             = abap_false ] TO FIELD-SYMBOL(<ls_buf>).
      IF sy-subrc = 0.
        <ls_buf>-deleted = abap_true.
        <ls_buf>-changed = abap_true.

        LOOP AT zpru_cl_axc_buffer=>step_buffer ASSIGNING FIELD-SYMBOL(<ls_step_del>)
             WHERE instance-query_uuid = <ls_delete>-query_uuid.
          <ls_step_del>-changed = abap_true.
          <ls_step_del>-deleted = abap_true.
        ENDLOOP.
      ELSE.
        APPEND VALUE #( query_uuid = <ls_delete>-query_uuid
                        delete     = abap_true
                        fail       = zpru_if_agent_frw=>cs_fail_cause-not_found )
               TO cs_failed-query.

        APPEND VALUE #( query_uuid = <ls_delete>-query_uuid
                        delete     = abap_true
                        msg        = lo_util->new_message(
                                         iv_id       = zpru_if_agent_frw=>cs_message_class-zpru_msg_execution
                                         iv_number   = `011`
                                         iv_severity = zpru_if_agent_message=>sc_severity-error
                                         iv_v1       = <ls_delete>-query_uuid ) )
               TO cs_reported-query.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_axc_service~read_header.
    DATA ls_out  TYPE zpru_axc_head.
    DATA lo_util TYPE REF TO zpru_if_agent_util.

    CLEAR et_axc_head.

    IF it_head_read_k IS INITIAL.
      RETURN.
    ENDIF.

    precheck_read_header( EXPORTING it_head_read_k = it_head_read_k
                          IMPORTING et_entities    = DATA(lt_entities)
                          CHANGING  cs_reported    = cs_reported
                                    cs_failed      = cs_failed ).

    IF lt_entities IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        lo_util ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_AGENT_UTIL`
                                                            iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    zpru_cl_axc_buffer=>prep_header_buffer( VALUE #( FOR <ls_k> IN lt_entities
                                                     ( run_uuid = <ls_k>-run_uuid ) ) ).

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_req>).

      ASSIGN zpru_cl_axc_buffer=>header_buffer[ instance-run_uuid = <ls_req>-run_uuid
                                                deleted           = abap_false ] TO FIELD-SYMBOL(<ls_buf>).
      IF sy-subrc = 0.
        CLEAR ls_out.

        ls_out-run_uuid           = <ls_buf>-instance-run_uuid.
        ls_out-run_id             = COND #( WHEN <ls_req>-control-run_id = abap_true
                                            THEN <ls_buf>-instance-run_id ).
        ls_out-agent_uuid         = COND #( WHEN <ls_req>-control-agent_uuid = abap_true
                                            THEN <ls_buf>-instance-agent_uuid ).
        ls_out-user_id            = COND #( WHEN <ls_req>-control-user_id = abap_true
                                            THEN <ls_buf>-instance-user_id ).
        ls_out-start_timestamp    = COND #( WHEN <ls_req>-control-start_timestamp = abap_true
                                            THEN <ls_buf>-instance-start_timestamp ).
        ls_out-end_timestamp      = COND #( WHEN <ls_req>-control-end_timestamp = abap_true
                                            THEN <ls_buf>-instance-end_timestamp ).
        ls_out-created_by         = COND #( WHEN <ls_req>-control-created_by = abap_true
                                            THEN <ls_buf>-instance-created_by ).
        ls_out-created_at         = COND #( WHEN <ls_req>-control-created_at = abap_true
                                            THEN <ls_buf>-instance-created_at ).
        ls_out-changed_by         = COND #( WHEN <ls_req>-control-changed_by = abap_true
                                            THEN <ls_buf>-instance-changed_by ).
        ls_out-last_changed       = COND #( WHEN <ls_req>-control-last_changed = abap_true
                                            THEN <ls_buf>-instance-last_changed ).
        ls_out-local_last_changed = COND #( WHEN <ls_req>-control-local_last_changed = abap_true
                                            THEN <ls_buf>-instance-local_last_changed ).

        APPEND ls_out TO et_axc_head.
      ELSE.
        APPEND VALUE #( run_uuid = <ls_req>-run_uuid
                        fail     = zpru_if_agent_frw=>cs_fail_cause-not_found )
               TO cs_failed-header.

        APPEND VALUE #( run_uuid = <ls_req>-run_uuid
                        msg      = lo_util->new_message(
                                       iv_id       = zpru_if_agent_frw=>cs_message_class-zpru_msg_execution
                                       iv_number   = `012`
                                       iv_severity = zpru_if_agent_message=>sc_severity-error
                                       iv_v1       = <ls_req>-run_uuid ) )
               TO cs_reported-header.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_axc_service~create_header.
    DATA ls_reported TYPE zpru_if_agent_frw=>ts_axc_reported.
    DATA ls_failed   TYPE zpru_if_agent_frw=>ts_axc_failed.
    DATA lt_create_in TYPE TABLE FOR CREATE zr_pru_axc_head.

    precheck_create_header( EXPORTING it_head_create_imp = it_head_create_imp
                            IMPORTING et_entities        = DATA(lt_entities)
                            CHANGING  cs_reported        = ls_reported
                                      cs_failed          = ls_failed ).

    cs_failed = CORRESPONDING #( DEEP ls_failed ).
    cs_reported = CORRESPONDING #( DEEP ls_reported ).

    IF    lt_entities       <> it_head_create_imp
       OR ls_failed-header IS NOT INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_create>).
      APPEND INITIAL LINE TO lt_create_in ASSIGNING FIELD-SYMBOL(<ls_create_in>).
      <ls_create_in>-runuuid        = <ls_create>-run_uuid.
      <ls_create_in>-runid          = COND #( WHEN <ls_create>-control-run_id = abap_true THEN <ls_create>-run_id ).
      <ls_create_in>-agentuuid      = COND #( WHEN <ls_create>-control-agent_uuid = abap_true THEN <ls_create>-agent_uuid ).
      <ls_create_in>-userid         = COND #( WHEN <ls_create>-control-user_id    = abap_true THEN <ls_create>-user_id ).
      <ls_create_in>-starttimestamp = COND #( WHEN <ls_create>-control-start_timestamp = abap_true THEN <ls_create>-start_timestamp ).
      <ls_create_in>-endtimestamp   = COND #( WHEN <ls_create>-control-end_timestamp   = abap_true THEN <ls_create>-end_timestamp ).
      <ls_create_in>-createdby      = COND #( WHEN <ls_create>-control-created_by      = abap_true THEN <ls_create>-created_by ).
      <ls_create_in>-createdat      = COND #( WHEN <ls_create>-control-created_at      = abap_true THEN <ls_create>-created_at ).
      <ls_create_in>-changedby      = COND #( WHEN <ls_create>-control-changed_by      = abap_true THEN <ls_create>-changed_by ).
      <ls_create_in>-lastchanged    = COND #( WHEN <ls_create>-control-last_changed    = abap_true THEN <ls_create>-last_changed ).
      <ls_create_in>-locallastchanged = COND #( WHEN <ls_create>-control-local_last_changed = abap_true THEN <ls_create>-local_last_changed ).

      <ls_create_in>-%control-runid          = COND #( WHEN <ls_create>-control-run_id          = abap_true THEN if_abap_behv=>mk-on ).
      <ls_create_in>-%control-agentuuid      = COND #( WHEN <ls_create>-control-agent_uuid      = abap_true THEN if_abap_behv=>mk-on ).
      <ls_create_in>-%control-userid         = COND #( WHEN <ls_create>-control-user_id         = abap_true THEN if_abap_behv=>mk-on ).
      <ls_create_in>-%control-starttimestamp = COND #( WHEN <ls_create>-control-start_timestamp = abap_true THEN if_abap_behv=>mk-on ).
      <ls_create_in>-%control-endtimestamp   = COND #( WHEN <ls_create>-control-end_timestamp   = abap_true THEN if_abap_behv=>mk-on ).
      <ls_create_in>-%control-createdby      = COND #( WHEN <ls_create>-control-created_by      = abap_true THEN if_abap_behv=>mk-on ).
      <ls_create_in>-%control-createdat      = COND #( WHEN <ls_create>-control-created_at      = abap_true THEN if_abap_behv=>mk-on ).
      <ls_create_in>-%control-changedby      = COND #( WHEN <ls_create>-control-changed_by      = abap_true THEN if_abap_behv=>mk-on ).
      <ls_create_in>-%control-lastchanged    = COND #( WHEN <ls_create>-control-last_changed    = abap_true THEN if_abap_behv=>mk-on ).
      <ls_create_in>-%control-locallastchanged = COND #( WHEN <ls_create>-control-local_last_changed = abap_true THEN if_abap_behv=>mk-on ).
    ENDLOOP.

    MODIFY ENTITIES OF zr_pru_axc_head
           ENTITY executionHeader
           CREATE AUTO FILL CID WITH lt_create_in
           MAPPED DATA(ls_mapped_eml)
           FAILED DATA(ls_failed_eml)
           REPORTED DATA(ls_reported_eml).

    LOOP AT ls_failed_eml-executionheader ASSIGNING FIELD-SYMBOL(<ls_failed_header>).
      APPEND INITIAL LINE TO cs_failed-header ASSIGNING FIELD-SYMBOL(<ls_failed_target>).
      <ls_failed_target>-run_uuid = <ls_failed_header>-runuuid.
      <ls_failed_target>-fail     = CONV #( <ls_failed_header>-%fail-cause ).
    ENDLOOP.

    LOOP AT ls_reported_eml-executionheader ASSIGNING FIELD-SYMBOL(<ls_reported_header>).
      APPEND INITIAL LINE TO cs_reported-header ASSIGNING FIELD-SYMBOL(<ls_reported_header_target>).
      <ls_reported_header_target>-run_uuid = <ls_reported_header>-runuuid.
*      <ls_reported_header_target>-msg      = <ls_reported_header>-%msg.
    ENDLOOP.

    LOOP AT ls_mapped_eml-executionheader ASSIGNING FIELD-SYMBOL(<ls_mapped_header>).
      APPEND INITIAL LINE TO cs_mapped-header ASSIGNING FIELD-SYMBOL(<ls_mapped_header_target>).
      <ls_mapped_header_target>-run_uuid = <ls_mapped_header>-runuuid.
    ENDLOOP.
  ENDMETHOD.


  METHOD zpru_if_axc_service~update_header.
    DATA ls_reported TYPE zpru_if_agent_frw=>ts_axc_reported.
    DATA ls_failed   TYPE zpru_if_agent_frw=>ts_axc_failed.
    DATA lt_update_in TYPE TABLE FOR UPDATE zr_pru_axc_head.

    precheck_update_header( EXPORTING it_head_update_imp = it_head_update_imp
                            IMPORTING et_entities        = DATA(lt_entities)
                            CHANGING  cs_reported        = ls_reported
                                      cs_failed          = ls_failed ).

    cs_failed = CORRESPONDING #( DEEP ls_failed ).
    cs_reported = CORRESPONDING #( DEEP ls_reported ).

    IF    lt_entities       <> it_head_update_imp
       OR ls_failed-header IS NOT INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_update>).
      APPEND INITIAL LINE TO lt_update_in ASSIGNING FIELD-SYMBOL(<ls_update_in>).
      <ls_update_in>-runuuid        = <ls_update>-run_uuid.
      <ls_update_in>-runid          = COND #( WHEN <ls_update>-control-run_id = abap_true THEN <ls_update>-run_id ).
      <ls_update_in>-agentuuid      = COND #( WHEN <ls_update>-control-agent_uuid = abap_true THEN <ls_update>-agent_uuid ).
      <ls_update_in>-userid         = COND #( WHEN <ls_update>-control-user_id    = abap_true THEN <ls_update>-user_id ).
      <ls_update_in>-starttimestamp = COND #( WHEN <ls_update>-control-start_timestamp = abap_true THEN <ls_update>-start_timestamp ).
      <ls_update_in>-endtimestamp   = COND #( WHEN <ls_update>-control-end_timestamp   = abap_true THEN <ls_update>-end_timestamp ).
      <ls_update_in>-createdby      = COND #( WHEN <ls_update>-control-created_by      = abap_true THEN <ls_update>-created_by ).
      <ls_update_in>-createdat      = COND #( WHEN <ls_update>-control-created_at      = abap_true THEN <ls_update>-created_at ).
      <ls_update_in>-changedby      = COND #( WHEN <ls_update>-control-changed_by      = abap_true THEN <ls_update>-changed_by ).
      <ls_update_in>-lastchanged    = COND #( WHEN <ls_update>-control-last_changed    = abap_true THEN <ls_update>-last_changed ).
      <ls_update_in>-locallastchanged = COND #( WHEN <ls_update>-control-local_last_changed = abap_true THEN <ls_update>-local_last_changed ).

      <ls_update_in>-%control-runid          = COND #( WHEN <ls_update>-control-run_id          = abap_true THEN if_abap_behv=>mk-on ).
      <ls_update_in>-%control-agentuuid      = COND #( WHEN <ls_update>-control-agent_uuid      = abap_true THEN if_abap_behv=>mk-on ).
      <ls_update_in>-%control-userid         = COND #( WHEN <ls_update>-control-user_id         = abap_true THEN if_abap_behv=>mk-on ).
      <ls_update_in>-%control-starttimestamp = COND #( WHEN <ls_update>-control-start_timestamp = abap_true THEN if_abap_behv=>mk-on ).
      <ls_update_in>-%control-endtimestamp   = COND #( WHEN <ls_update>-control-end_timestamp   = abap_true THEN if_abap_behv=>mk-on ).
      <ls_update_in>-%control-createdby      = COND #( WHEN <ls_update>-control-created_by      = abap_true THEN if_abap_behv=>mk-on ).
      <ls_update_in>-%control-createdat      = COND #( WHEN <ls_update>-control-created_at      = abap_true THEN if_abap_behv=>mk-on ).
      <ls_update_in>-%control-changedby      = COND #( WHEN <ls_update>-control-changed_by      = abap_true THEN if_abap_behv=>mk-on ).
      <ls_update_in>-%control-lastchanged    = COND #( WHEN <ls_update>-control-last_changed    = abap_true THEN if_abap_behv=>mk-on ).
      <ls_update_in>-%control-locallastchanged = COND #( WHEN <ls_update>-control-local_last_changed = abap_true THEN if_abap_behv=>mk-on ).
    ENDLOOP.

    MODIFY ENTITIES OF zr_pru_axc_head
           ENTITY executionHeader
           UPDATE FROM lt_update_in
           FAILED DATA(ls_failed_eml)
           REPORTED DATA(ls_reported_eml).

    LOOP AT ls_failed_eml-executionheader ASSIGNING FIELD-SYMBOL(<ls_failed_header>).
      APPEND INITIAL LINE TO cs_failed-header ASSIGNING FIELD-SYMBOL(<ls_failed_header_target>).
      <ls_failed_header_target>-run_uuid = <ls_failed_header>-runuuid.
      <ls_failed_header_target>-fail     = CONV #( <ls_failed_header>-%fail-cause ).
    ENDLOOP.

    LOOP AT ls_reported_eml-executionheader ASSIGNING FIELD-SYMBOL(<ls_reported_header>).
      APPEND INITIAL LINE TO cs_reported-header ASSIGNING FIELD-SYMBOL(<ls_reported_header_target>).
      <ls_reported_header_target>-run_uuid = <ls_reported_header>-runuuid.
*      <ls_reported_header_target>-msg      = <ls_reported_header>-%msg.
    ENDLOOP.
  ENDMETHOD.


  METHOD zpru_if_axc_service~delete_header.
    DATA ls_reported TYPE zpru_if_agent_frw=>ts_axc_reported.
    DATA ls_failed   TYPE zpru_if_agent_frw=>ts_axc_failed.
    DATA lt_delete_in TYPE TABLE FOR DELETE zr_pru_axc_head.

    precheck_delete_header( EXPORTING it_head_delete_imp = it_head_delete_imp
                            IMPORTING et_entities        = DATA(lt_entities)
                            CHANGING  cs_reported        = ls_reported
                                      cs_failed          = ls_failed ).

    cs_failed = CORRESPONDING #( DEEP ls_failed ).
    cs_reported = CORRESPONDING #( DEEP ls_reported ).

    IF    lt_entities       <> it_head_delete_imp
       OR ls_failed-header IS NOT INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_delete>).
      APPEND INITIAL LINE TO lt_delete_in ASSIGNING FIELD-SYMBOL(<ls_delete_in>).
      <ls_delete_in>-runuuid = <ls_delete>-run_uuid.
    ENDLOOP.

    MODIFY ENTITIES OF zr_pru_axc_head
           ENTITY executionHeader
           DELETE FROM lt_delete_in
           FAILED DATA(ls_failed_eml)
           REPORTED DATA(ls_reported_eml).

    LOOP AT ls_failed_eml-executionheader ASSIGNING FIELD-SYMBOL(<ls_failed_header>).
      APPEND INITIAL LINE TO cs_failed-header ASSIGNING FIELD-SYMBOL(<ls_failed_header_target>).
      <ls_failed_header_target>-run_uuid = <ls_failed_header>-runuuid.
      <ls_failed_header_target>-fail     = CONV #( <ls_failed_header>-%fail-cause ).
    ENDLOOP.

    LOOP AT ls_reported_eml-executionheader ASSIGNING FIELD-SYMBOL(<ls_reported_header>).
      APPEND INITIAL LINE TO cs_reported-header ASSIGNING FIELD-SYMBOL(<ls_reported_header_target>).
      <ls_reported_header_target>-run_uuid = <ls_reported_header>-runuuid.
*      <ls_reported_header_target>-msg      = <ls_reported_header>-%msg.
    ENDLOOP.
  ENDMETHOD.

  METHOD precheck_update_header.
    DATA lo_pre TYPE REF TO zpru_if_axc_precheck.

    TRY.
        lo_pre ?= zpru_cl_agent_service_mngr=>get_service(
                      iv_service = `ZPRU_IF_AXC_PRECHECK`
                      iv_context = zpru_if_agent_frw=>cs_context-st_agent_execution ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    lo_pre->precheck_update_header( EXPORTING it_head_update_imp = it_head_update_imp
                                    IMPORTING et_entities        = et_entities
                                    CHANGING  cs_reported        = cs_reported
                                              cs_failed          = cs_failed ).
  ENDMETHOD.

  METHOD precheck_delete_header.
    DATA lo_pre TYPE REF TO zpru_if_axc_precheck.

    TRY.
        lo_pre ?= zpru_cl_agent_service_mngr=>get_service(
                      iv_service = `ZPRU_IF_AXC_PRECHECK`
                      iv_context = zpru_if_agent_frw=>cs_context-st_agent_execution ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    lo_pre->precheck_delete_header( EXPORTING it_head_delete_imp = it_head_delete_imp
                                    IMPORTING et_entities        = et_entities
                                    CHANGING  cs_reported        = cs_reported
                                              cs_failed          = cs_failed ).
  ENDMETHOD.

  METHOD precheck_create_header.
    DATA lo_pre TYPE REF TO zpru_if_axc_precheck.

    TRY.
        lo_pre ?= zpru_cl_agent_service_mngr=>get_service(
                      iv_service = `ZPRU_IF_AXC_PRECHECK`
                      iv_context = zpru_if_agent_frw=>cs_context-st_agent_execution ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    lo_pre->precheck_create_header( EXPORTING it_head_create_imp = it_head_create_imp
                                    IMPORTING et_entities        = et_entities
                                    CHANGING  cs_reported        = cs_reported
                                              cs_failed          = cs_failed ).
  ENDMETHOD.

  METHOD precheck_cba_query.
    DATA lo_pre TYPE REF TO zpru_if_axc_precheck.

    TRY.
        lo_pre ?= zpru_cl_agent_service_mngr=>get_service(
                      iv_service = `ZPRU_IF_AXC_PRECHECK`
                      iv_context = zpru_if_agent_frw=>cs_context-st_agent_execution ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    lo_pre->precheck_cba_query( EXPORTING it_axc_query_imp = it_axc_query_imp
                                IMPORTING et_entities      = et_entities
                                CHANGING  cs_reported      = cs_reported
                                          cs_failed        = cs_failed ).
  ENDMETHOD.

  METHOD precheck_read_header.
    DATA lo_pre TYPE REF TO zpru_if_axc_precheck.

    TRY.
        lo_pre ?= zpru_cl_agent_service_mngr=>get_service(
                      iv_service = `ZPRU_IF_AXC_PRECHECK`
                      iv_context = zpru_if_agent_frw=>cs_context-st_agent_execution ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    lo_pre->precheck_read_header( EXPORTING it_head_read_k = it_head_read_k
                                  IMPORTING et_entities    = et_entities
                                  CHANGING  cs_reported    = cs_reported
                                            cs_failed      = cs_failed ).
  ENDMETHOD.

  METHOD precheck_rba_query.
    DATA lo_pre TYPE REF TO zpru_if_axc_precheck.

    TRY.
        lo_pre ?= zpru_cl_agent_service_mngr=>get_service(
                      iv_service = `ZPRU_IF_AXC_PRECHECK`
                      iv_context = zpru_if_agent_frw=>cs_context-st_agent_execution ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    lo_pre->precheck_rba_query( EXPORTING it_rba_query_k = it_rba_query_k
                                IMPORTING et_entities    = et_entities
                                CHANGING  cs_reported    = cs_reported
                                          cs_failed      = cs_failed ).
  ENDMETHOD.

  METHOD precheck_read_query.
    DATA lo_pre TYPE REF TO zpru_if_axc_precheck.

    TRY.
        lo_pre ?= zpru_cl_agent_service_mngr=>get_service(
                      iv_service = `ZPRU_IF_AXC_PRECHECK`
                      iv_context = zpru_if_agent_frw=>cs_context-st_agent_execution ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    lo_pre->precheck_read_query( EXPORTING it_query_read_k = it_query_read_k
                                 IMPORTING et_entities     = et_entities
                                 CHANGING  cs_reported     = cs_reported
                                           cs_failed       = cs_failed ).
  ENDMETHOD.

  METHOD precheck_update_query.
    DATA lo_pre TYPE REF TO zpru_if_axc_precheck.

    TRY.
        lo_pre ?= zpru_cl_agent_service_mngr=>get_service(
                      iv_service = `ZPRU_IF_AXC_PRECHECK`
                      iv_context = zpru_if_agent_frw=>cs_context-st_agent_execution ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    lo_pre->precheck_update_query( EXPORTING it_query_update_imp = it_query_update_imp
                                   IMPORTING et_entities         = et_entities
                                   CHANGING  cs_reported         = cs_reported
                                             cs_failed           = cs_failed ).
  ENDMETHOD.

  METHOD precheck_delete_query.
    DATA lo_pre TYPE REF TO zpru_if_axc_precheck.

    TRY.
        lo_pre ?= zpru_cl_agent_service_mngr=>get_service(
                      iv_service = `ZPRU_IF_AXC_PRECHECK`
                      iv_context = zpru_if_agent_frw=>cs_context-st_agent_execution ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    lo_pre->precheck_delete_query( EXPORTING it_query_delete_imp = it_query_delete_imp
                                   IMPORTING et_entities         = et_entities
                                   CHANGING  cs_reported         = cs_reported
                                             cs_failed           = cs_failed ).
  ENDMETHOD.

  METHOD precheck_cba_step.
    DATA lo_pre TYPE REF TO zpru_if_axc_precheck.

    TRY.
        lo_pre ?= zpru_cl_agent_service_mngr=>get_service(
                      iv_service = `ZPRU_IF_AXC_PRECHECK`
                      iv_context = zpru_if_agent_frw=>cs_context-st_agent_execution ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    lo_pre->precheck_cba_step( EXPORTING it_axc_step_imp = it_axc_step_imp
                               IMPORTING et_entities     = et_entities
                               CHANGING  cs_reported     = cs_reported
                                         cs_failed       = cs_failed ).
  ENDMETHOD.

  METHOD precheck_rba_step.
    DATA lo_pre TYPE REF TO zpru_if_axc_precheck.

    TRY.
        lo_pre ?= zpru_cl_agent_service_mngr=>get_service(
                      iv_service = `ZPRU_IF_AXC_PRECHECK`
                      iv_context = zpru_if_agent_frw=>cs_context-st_agent_execution ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    lo_pre->precheck_rba_step( EXPORTING it_rba_step_k = it_rba_step_k
                               IMPORTING et_entities   = et_entities
                               CHANGING  cs_reported   = cs_reported
                                         cs_failed     = cs_failed ).
  ENDMETHOD.

  METHOD precheck_read_step.
    DATA lo_pre TYPE REF TO zpru_if_axc_precheck.

    TRY.
        lo_pre ?= zpru_cl_agent_service_mngr=>get_service(
                      iv_service = `ZPRU_IF_AXC_PRECHECK`
                      iv_context = zpru_if_agent_frw=>cs_context-st_agent_execution ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    lo_pre->precheck_read_step( EXPORTING it_step_read_k = it_step_read_k
                                IMPORTING et_entities    = et_entities
                                CHANGING  cs_reported    = cs_reported
                                          cs_failed      = cs_failed ).
  ENDMETHOD.

  METHOD precheck_update_step.
    DATA lo_pre TYPE REF TO zpru_if_axc_precheck.

    TRY.
        lo_pre ?= zpru_cl_agent_service_mngr=>get_service(
                      iv_service = `ZPRU_IF_AXC_PRECHECK`
                      iv_context = zpru_if_agent_frw=>cs_context-st_agent_execution ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    lo_pre->precheck_update_step( EXPORTING it_step_update_imp = it_step_update_imp
                                  IMPORTING et_entities        = et_entities
                                  CHANGING  cs_reported        = cs_reported
                                            cs_failed          = cs_failed ).
  ENDMETHOD.

  METHOD precheck_delete_step.
    DATA lo_pre TYPE REF TO zpru_if_axc_precheck.

    TRY.
        lo_pre ?= zpru_cl_agent_service_mngr=>get_service(
                      iv_service = `ZPRU_IF_AXC_PRECHECK`
                      iv_context = zpru_if_agent_frw=>cs_context-st_agent_execution ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    lo_pre->precheck_delete_step( EXPORTING it_step_delete_imp = it_step_delete_imp
                                  IMPORTING et_entities        = et_entities
                                  CHANGING  cs_reported        = cs_reported
                                            cs_failed          = cs_failed ).
  ENDMETHOD.

  METHOD zpru_if_axc_service~generate_run_id.
    DATA lv_run_id_base      TYPE i.
    DATA lv_run_id_base_char TYPE zpru_de_run_id.

    IF iv_run_id_base IS NOT INITIAL.
      lv_run_id_base_char = CONV #( iv_run_id_base ).
      rv_run_id = |{ lv_run_id_base_char ALPHA = IN }|.
      RETURN.
    ENDIF.

    SELECT SINGLE MAX( run_id ) FROM zpru_axc_head
      INTO @DATA(lv_last_db_run_id).

    DATA(lt_head_buffer) = zpru_cl_axc_buffer=>header_buffer.
    SORT lt_head_buffer BY instance-run_id DESCENDING.
    DATA(lv_last_buf_run_id) = VALUE #( lt_head_buffer[ 1 ]-instance-run_id OPTIONAL ).

    IF lv_last_db_run_id IS INITIAL AND lv_last_buf_run_id IS INITIAL.
      lv_run_id_base = 1.
      lv_run_id_base_char = CONV #( lv_run_id_base ).
      rv_run_id = |{ lv_run_id_base_char ALPHA = IN }|.
      RETURN.
    ENDIF.

    IF lv_last_db_run_id = lv_last_buf_run_id.
      lv_run_id_base = CONV #( lv_last_buf_run_id ).
    ENDIF.

    IF lv_run_id_base IS INITIAL.
      IF lv_last_db_run_id < lv_last_buf_run_id.
        lv_run_id_base = CONV #( lv_last_buf_run_id ).
      ELSE.
        lv_run_id_base = CONV #( lv_last_db_run_id ).
      ENDIF.
    ENDIF.

    lv_run_id_base += 1.

    lv_run_id_base_char = CONV #( lv_run_id_base ).
    rv_run_id = |{ lv_run_id_base_char ALPHA = IN }|.
  ENDMETHOD.

  METHOD zpru_if_axc_service~generate_query_number.
    DATA ls_reported TYPE zpru_if_agent_frw=>ts_axc_reported.
    DATA ls_failed   TYPE zpru_if_agent_frw=>ts_axc_failed.

    IF iv_query_number_base IS NOT INITIAL.
      rv_query_number = iv_query_number_base + 1.
      RETURN.
    ENDIF.

    IF iv_run_uuid IS INITIAL.
      RETURN.
    ENDIF.

    zpru_if_axc_service~rba_query(
      EXPORTING it_rba_query_k = VALUE #( ( run_uuid = iv_run_uuid
                                            control  = VALUE #( query_number = abap_true ) ) )
      IMPORTING et_axc_query   = DATA(lt_axc_query)
      CHANGING  cs_reported    = ls_reported
                cs_failed      = ls_failed   ).

    SORT lt_axc_query BY query_number DESCENDING.
    DATA(lv_last_query_number) = VALUE #( lt_axc_query[ 1 ]-query_number OPTIONAL ).

    lv_last_query_number += 1.
    rv_query_number = lv_last_query_number.
  ENDMETHOD.

  METHOD zpru_if_axc_service~generate_step_number.
    DATA ls_reported TYPE zpru_if_agent_frw=>ts_axc_reported.
    DATA ls_failed   TYPE zpru_if_agent_frw=>ts_axc_failed.

    IF iv_step_number_base IS NOT INITIAL.
      rv_step_number = iv_step_number_base + 1.
      RETURN.
    ENDIF.

    IF iv_query_uuid IS INITIAL.
      RETURN.
    ENDIF.

    zpru_if_axc_service~rba_step( EXPORTING it_rba_step_k = VALUE #( ( query_uuid = iv_query_uuid
                                                                       control    = VALUE #(
                                                                           step_number = abap_true ) ) )
                                  IMPORTING et_axc_step   = DATA(lt_axc_step)
                                  CHANGING  cs_reported   = ls_reported
                                            cs_failed     = ls_failed   ).

    SORT lt_axc_step BY step_number DESCENDING.
    DATA(lv_last_step_number) = VALUE #( lt_axc_step[ 1 ]-step_number OPTIONAL ).

    lv_last_step_number += 1.
    rv_step_number = lv_last_step_number.
  ENDMETHOD.
ENDCLASS.
