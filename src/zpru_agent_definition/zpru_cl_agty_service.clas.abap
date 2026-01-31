CLASS zpru_cl_agty_service DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zpru_if_agent_frw.
    INTERFACES zpru_if_agty_service.

    TYPES: BEGIN OF ts_agty_buffer,
             instance TYPE zpru_agent_type,
             changed  TYPE abap_bool,
             deleted  TYPE abap_bool,
           END OF ts_agty_buffer.

    TYPES tt_agty_buffer TYPE STANDARD TABLE OF ts_agty_buffer WITH EMPTY KEY.

    CLASS-DATA gt_agty_buffer TYPE tt_agty_buffer.

  PROTECTED SECTION.
    METHODS precheck_create_agent_type
      IMPORTING it_agty_create_imp TYPE zpru_if_agty_crud=>tt_agty_create_imp
      EXPORTING et_entities        TYPE zpru_if_agty_crud=>tt_agty_create_imp
      CHANGING  cs_reported        TYPE zpru_if_agent_frw=>ts_agty_bndl_reported OPTIONAL
                cs_failed          TYPE zpru_if_agent_frw=>ts_agty_bndl_failed   OPTIONAL.

    METHODS precheck_update_agent_type
      IMPORTING it_agty_update_imp TYPE zpru_if_agty_crud=>tt_agty_update_imp
      EXPORTING et_entities        TYPE zpru_if_agty_crud=>tt_agty_update_imp
      CHANGING  cs_reported        TYPE zpru_if_agent_frw=>ts_agty_bndl_reported OPTIONAL
                cs_failed          TYPE zpru_if_agent_frw=>ts_agty_bndl_failed   OPTIONAL.

    METHODS precheck_delete_agent_type
      IMPORTING it_agty_delete_imp TYPE zpru_if_agty_crud=>tt_agty_delete_imp
      EXPORTING et_entities        TYPE zpru_if_agty_crud=>tt_agty_delete_imp
      CHANGING  cs_reported        TYPE zpru_if_agent_frw=>ts_agty_bndl_reported OPTIONAL
                cs_failed          TYPE zpru_if_agent_frw=>ts_agty_bndl_failed   OPTIONAL.

    METHODS precheck_read_agent_type
      IMPORTING it_agty_read_k TYPE zpru_if_agty_crud=>tt_agty_read_k
      EXPORTING et_entities    TYPE zpru_if_agty_crud=>tt_agty_read_k
      CHANGING  cs_reported    TYPE zpru_if_agent_frw=>ts_agty_bndl_reported OPTIONAL
                cs_failed      TYPE zpru_if_agent_frw=>ts_agty_bndl_failed   OPTIONAL.

    METHODS fill_agty_admin_fields
      IMPORTING iv_during_create TYPE abap_boolean DEFAULT abap_false
      CHANGING  cs_agty          TYPE ts_agty_buffer.
ENDCLASS.


CLASS zpru_cl_agty_service IMPLEMENTATION.
  METHOD zpru_if_agty_service~clean_up.
  ENDMETHOD.

  METHOD zpru_if_agty_service~create_agent_type.
    DATA ls_reported TYPE zpru_if_agent_frw=>ts_agty_bndl_reported.
    DATA ls_failed   TYPE zpru_if_agent_frw=>ts_agty_bndl_failed.
    DATA lt_create_in TYPE TABLE FOR CREATE zr_pru_agent_type\\zrpruagenttype.

    precheck_create_agent_type( EXPORTING it_agty_create_imp = it_agty_create_imp
                                IMPORTING et_entities        = DATA(lt_entities)
                                CHANGING  cs_reported        = ls_reported
                                          cs_failed          = ls_failed ).

    cs_failed = CORRESPONDING #( DEEP ls_failed ).
    cs_reported = CORRESPONDING #( DEEP ls_reported ).

    IF    lt_entities          <> it_agty_create_imp
       OR ls_failed-agent_type IS NOT INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_create>).

      APPEND INITIAL LINE TO lt_create_in ASSIGNING FIELD-SYMBOL(<ls_create_in>).
      <ls_create_in>-agenttype      = <ls_create>-agent_type.
      <ls_create_in>-shortmemvolume = COND #( WHEN <ls_create>-control-short_mem_volume = abap_true
                                              THEN <ls_create>-short_mem_volume ).
      <ls_create_in>-%control-shortmemvolume = COND #( WHEN <ls_create>-control-short_mem_volume = abap_true
                                                       THEN if_abap_behv=>mk-on ).
      <ls_create_in>-discardstrategy = COND #( WHEN <ls_create>-control-discard_strategy = abap_true
                                               THEN <ls_create>-discard_strategy ).
      <ls_create_in>-%control-discardstrategy = COND #( WHEN <ls_create>-control-discard_strategy = abap_true
                                                        THEN if_abap_behv=>mk-on ).
      <ls_create_in>-summarystrategy = COND #( WHEN <ls_create>-control-summary_strategy = abap_true
                                               THEN <ls_create>-summary_strategy ).
      <ls_create_in>-%control-summarystrategy = COND #( WHEN <ls_create>-control-summary_strategy = abap_true
                                                        THEN if_abap_behv=>mk-on ).
      <ls_create_in>-maxnumbloop = COND #( WHEN <ls_create>-control-max_numb_loop = abap_true
                                           THEN <ls_create>-max_numb_loop ).
      <ls_create_in>-%control-maxnumbloop = COND #( WHEN <ls_create>-control-max_numb_loop = abap_true
                                                    THEN if_abap_behv=>mk-on ).
      <ls_create_in>-createdby = COND #( WHEN <ls_create>-control-created_by = abap_true
                                         THEN <ls_create>-created_by ).
      <ls_create_in>-%control-createdby = COND #( WHEN <ls_create>-control-created_by = abap_true
                                                  THEN if_abap_behv=>mk-on ).
      <ls_create_in>-createdat = COND #( WHEN <ls_create>-control-created_at = abap_true
                                         THEN <ls_create>-created_at ).
      <ls_create_in>-%control-createdat = COND #( WHEN <ls_create>-control-created_at = abap_true
                                                  THEN if_abap_behv=>mk-on ).
      <ls_create_in>-changedby = COND #( WHEN <ls_create>-control-changed_by <> if_abap_behv=>mk-off
                                         THEN <ls_create>-changed_by ).
      <ls_create_in>-%control-changedby = COND #( WHEN <ls_create>-control-changed_by <> if_abap_behv=>mk-off
                                                  THEN if_abap_behv=>mk-on ).
      <ls_create_in>-lastchanged = COND #( WHEN <ls_create>-control-last_changed = abap_true
                                           THEN <ls_create>-last_changed ).
      <ls_create_in>-%control-lastchanged = COND #( WHEN <ls_create>-control-last_changed = abap_true
                                                    THEN if_abap_behv=>mk-on ).
      <ls_create_in>-locallastchanged = COND #( WHEN <ls_create>-control-local_last_changed = abap_true
                                                THEN <ls_create>-local_last_changed ).
      <ls_create_in>-%control-locallastchanged = COND #( WHEN <ls_create>-control-local_last_changed = abap_true
                                                         THEN if_abap_behv=>mk-on ).
    ENDLOOP.

    MODIFY ENTITIES OF zr_pru_agent_type
           ENTITY zrpruagenttype
           CREATE AUTO FILL CID WITH lt_create_in
           MAPPED DATA(ls_cr_mapped)
           REPORTED DATA(ls_cr_reported)
           FAILED DATA(ls_cr_failed).

    LOOP AT ls_cr_failed-zrpruagenttype ASSIGNING FIELD-SYMBOL(<ls_failed_agenttype>).
      APPEND INITIAL LINE TO cs_failed-agent_type ASSIGNING FIELD-SYMBOL(<ls_failed_target>).
      <ls_failed_target>-agent_type = <ls_failed_agenttype>-agenttype.
      <ls_failed_target>-fail       = CONV #( <ls_failed_agenttype>-%fail-cause ).
      <ls_failed_target>-create     = <ls_failed_agenttype>-%create.
    ENDLOOP.

    LOOP AT ls_cr_reported-zrpruagenttype ASSIGNING FIELD-SYMBOL(<ls_reported_agenttype>).
      APPEND INITIAL LINE TO cs_reported-agent_type ASSIGNING FIELD-SYMBOL(<ls_reported_target>).
      <ls_reported_target>-agent_type = <ls_reported_agenttype>-agenttype.
*      <ls_reported_target>-msg        =  <ls_reported_agenttype>-%msg.
      <ls_reported_target>-create     = <ls_reported_agenttype>-%create.
    ENDLOOP.

    LOOP AT ls_cr_mapped-zrpruagenttype ASSIGNING FIELD-SYMBOL(<ls_mapped_agenttype>).
      APPEND INITIAL LINE TO cs_mapped-agent_type ASSIGNING FIELD-SYMBOL(<ls_mapped_target>).
      <ls_mapped_target>-agent_type = <ls_mapped_agenttype>-agenttype.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_agty_service~delete_agent_type.
    DATA lo_util TYPE REF TO zpru_if_agent_util.

    precheck_delete_agent_type( EXPORTING it_agty_delete_imp = it_agty_delete_imp
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

*    zpru_cl_adf_buffer=>prep_agent_buffer( VALUE #( FOR <ls_k>
*                                                    IN     lt_entities
*                                                    ( agent_uuid = <ls_k>-agent_uuid ) ) ).

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_delete>).

      ASSIGN gt_agty_buffer[ instance-agent_type = <ls_delete>-agent_type
                             deleted             = abap_false ] TO FIELD-SYMBOL(<ls_buffer>).
      IF sy-subrc = 0.
        <ls_buffer>-deleted = abap_true.
        <ls_buffer>-changed = abap_true.

        APPEND VALUE #( msg        = lo_util->new_message(
                                         iv_id       = zpru_if_agent_frw=>cs_message_class-zpru_msg_execution
                                         iv_number   = `001`
                                         iv_severity = zpru_if_agent_message=>sc_severity-success
                                         iv_v1       = <ls_delete>-agent_type )
                        agent_type = <ls_delete>-agent_type ) TO cs_reported-agent_type.

      ELSE.
        APPEND VALUE #( agent_type = <ls_delete>-agent_type
                        delete     = abap_true
                        fail       = zpru_if_agent_frw=>cs_fail_cause-not_found )
               TO cs_failed-agent_type.

        APPEND VALUE #( agent_type = <ls_delete>-agent_type
                        delete     = abap_true
                        msg        = lo_util->new_message(
                                         iv_id       = zpru_if_agent_frw=>cs_message_class-zpru_msg_execution
                                         iv_number   = `003`
                                         iv_severity = zpru_if_agent_message=>sc_severity-error ) )
               TO cs_reported-agent_type.

      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_agty_service~determine.
  ENDMETHOD.

  METHOD zpru_if_agty_service~do_save.
  ENDMETHOD.

  METHOD zpru_if_agty_service~query_agent_type.
    CLEAR et_agty_k.

    SELECT agent_type FROM zpru_agent_type
      WHERE agent_type       IN @it_agent_type
        AND short_mem_volume IN @it_short_mem_volume
        AND discard_strategy IN @it_discard_strategy
        AND summary_strategy IN @it_summary_strategy
        AND max_numb_loop    IN @it_max_numb_loop
        AND created_by       IN @it_created_by
        AND created_at       IN @it_created_at
        AND changed_by       IN @it_changed_by
        AND last_changed     IN @it_last_changed
      INTO TABLE @et_agty_k.
  ENDMETHOD.

  METHOD zpru_if_agty_service~read_agent_type.
    DATA ls_out TYPE zpru_agent_type.

    CLEAR et_agty.

    IF it_agty_read_k IS INITIAL.
      RETURN.
    ENDIF.

    precheck_read_agent_type( EXPORTING it_agty_read_k = it_agty_read_k
                              IMPORTING et_entities    = DATA(lt_entities)
                              CHANGING  cs_reported    = cs_reported
                                        cs_failed      = cs_failed ).

*    zpru_cl_adf_buffer=>prep_agent_buffer( VALUE #( FOR <ls_k>
*                                                    IN     lt_entities
*                                                    ( agent_uuid = <ls_k>-agent_uuid ) ) ).

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_read>).

      ASSIGN gt_agty_buffer[ instance-agent_type = <ls_read>-agent_type
                             deleted             = abap_false ] TO FIELD-SYMBOL(<ls_buffer>).
      IF sy-subrc = 0.
        CLEAR ls_out.
        ls_out-agent_type         = <ls_buffer>-instance-agent_type.

        ls_out-short_mem_volume   = COND #( WHEN <ls_read>-control-short_mem_volume = abap_true
                                            THEN <ls_buffer>-instance-short_mem_volume ).
        ls_out-discard_strategy   = COND #( WHEN <ls_read>-control-discard_strategy = abap_true
                                            THEN <ls_buffer>-instance-discard_strategy ).
        ls_out-summary_strategy   = COND #( WHEN <ls_read>-control-summary_strategy = abap_true
                                            THEN <ls_buffer>-instance-summary_strategy ).
        ls_out-max_numb_loop      = COND #( WHEN <ls_read>-control-max_numb_loop = abap_true
                                            THEN <ls_buffer>-instance-max_numb_loop ).
        ls_out-created_by         = COND #( WHEN <ls_read>-control-created_by = abap_true
                                            THEN <ls_buffer>-instance-created_by ).
        ls_out-created_at         = COND #( WHEN <ls_read>-control-created_at = abap_true
                                            THEN <ls_buffer>-instance-created_at ).
        ls_out-changed_by         = COND #( WHEN <ls_read>-control-changed_by = abap_true
                                            THEN <ls_buffer>-instance-changed_by ).
        ls_out-last_changed       = COND #( WHEN <ls_read>-control-last_changed = abap_true
                                            THEN <ls_buffer>-instance-last_changed ).
        ls_out-local_last_changed = COND #( WHEN <ls_read>-control-local_last_changed = abap_true
                                            THEN <ls_buffer>-instance-local_last_changed ).

        APPEND ls_out TO et_agty.

      ELSE.
        APPEND VALUE #( agent_type = <ls_read>-agent_type
                        fail       = zpru_if_agent_frw=>cs_fail_cause-not_found )
               TO cs_failed-agent_type.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_agty_service~update_agent_type.
    DATA lo_util TYPE REF TO zpru_if_agent_util.

    precheck_update_agent_type( EXPORTING it_agty_update_imp = it_agty_update_imp
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

*    zpru_cl_adf_buffer=>prep_agent_buffer( VALUE #( FOR <ls_k>
*                                                    IN     lt_entities
*                                                    ( agent_uuid = <ls_k>-agent_uuid ) ) ).

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_update>).

      ASSIGN gt_agty_buffer[ instance-agent_type = <ls_update>-agent_type
                             deleted             = abap_false ] TO FIELD-SYMBOL(<ls_buffer>).
      IF sy-subrc = 0.
        <ls_buffer>-instance-short_mem_volume   = COND #( WHEN <ls_update>-control-short_mem_volume = abap_true
                                                          THEN <ls_update>-short_mem_volume
                                                          ELSE <ls_buffer>-instance-short_mem_volume ).
        <ls_buffer>-instance-discard_strategy   = COND #( WHEN <ls_update>-control-discard_strategy = abap_true
                                                          THEN <ls_update>-discard_strategy
                                                          ELSE <ls_buffer>-instance-discard_strategy ).
        <ls_buffer>-instance-summary_strategy   = COND #( WHEN <ls_update>-control-summary_strategy = abap_true
                                                          THEN <ls_update>-summary_strategy
                                                          ELSE <ls_buffer>-instance-summary_strategy ).
        <ls_buffer>-instance-max_numb_loop      = COND #( WHEN <ls_update>-control-max_numb_loop = abap_true
                                                          THEN <ls_update>-max_numb_loop
                                                          ELSE <ls_buffer>-instance-max_numb_loop ).
        <ls_buffer>-instance-created_by         = COND #( WHEN <ls_update>-control-created_by = abap_true
                                                          THEN <ls_update>-created_by
                                                          ELSE <ls_buffer>-instance-created_by ).
        <ls_buffer>-instance-created_at         = COND #( WHEN <ls_update>-control-created_at = abap_true
                                                          THEN <ls_update>-created_at
                                                          ELSE <ls_buffer>-instance-created_at ).
        <ls_buffer>-instance-changed_by         = COND #( WHEN <ls_update>-control-changed_by = abap_true
                                                          THEN <ls_update>-changed_by
                                                          ELSE <ls_buffer>-instance-changed_by ).
        <ls_buffer>-instance-last_changed       = COND #( WHEN <ls_update>-control-last_changed = abap_true
                                                          THEN <ls_update>-last_changed
                                                          ELSE <ls_buffer>-instance-last_changed ).
        <ls_buffer>-instance-local_last_changed = COND #( WHEN <ls_update>-control-local_last_changed = abap_true
                                                          THEN <ls_update>-local_last_changed
                                                          ELSE <ls_buffer>-instance-local_last_changed ).

        <ls_buffer>-changed = abap_true.
        <ls_buffer>-deleted = abap_false.

        fill_agty_admin_fields( EXPORTING iv_during_create = abap_false
                                CHANGING  cs_agty          = <ls_buffer> ).

      ELSE.
        APPEND VALUE #( agent_type = <ls_update>-agent_type
                        update     = abap_true
                        fail       = zpru_if_agent_frw=>cs_fail_cause-not_found )
               TO cs_failed-agent_type.

        APPEND VALUE #( agent_type = <ls_update>-agent_type
                        update     = abap_true
                        msg        = lo_util->new_message(
                                         iv_id       = zpru_if_agent_frw=>cs_message_class-zpru_msg_execution
                                         iv_number   = `012`
                                         iv_severity = zpru_if_agent_message=>sc_severity-error
                                         iv_v1       = <ls_update>-agent_type ) )
               TO cs_reported-agent_type.

      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_agty_service~validate.
  ENDMETHOD.

  METHOD precheck_create_agent_type.
    DATA lo_pre TYPE REF TO zpru_if_agty_precheck.

    TRY.
        lo_pre ?= zpru_cl_agent_service_mngr=>get_service(
                      iv_service = `ZPRU_IF_AGTY_PRECHECK`
                      iv_context = zpru_if_agent_frw=>cs_context-st_agent_definition ).
      CATCH zpru_cx_agent_core.
        RETURN.
    ENDTRY.

    lo_pre->precheck_create_agent_type( EXPORTING it_agty_create_imp = it_agty_create_imp
                                        IMPORTING et_entities        = et_entities
                                        CHANGING  cs_reported        = cs_reported
                                                  cs_failed          = cs_failed ).
  ENDMETHOD.

  METHOD precheck_update_agent_type.
    DATA lo_pre TYPE REF TO zpru_if_agty_precheck.

    TRY.
        lo_pre ?= zpru_cl_agent_service_mngr=>get_service(
                      iv_service = `ZPRU_IF_AGTY_PRECHECK`
                      iv_context = zpru_if_agent_frw=>cs_context-st_agent_definition ).
      CATCH zpru_cx_agent_core.
        RETURN.
    ENDTRY.

    lo_pre->precheck_update_agent_type( EXPORTING it_agty_update_imp = it_agty_update_imp
                                        IMPORTING et_entities        = et_entities
                                        CHANGING  cs_reported        = cs_reported
                                                  cs_failed          = cs_failed ).
  ENDMETHOD.

  METHOD precheck_delete_agent_type.
    DATA lo_pre TYPE REF TO zpru_if_agty_precheck.

    TRY.
        lo_pre ?= zpru_cl_agent_service_mngr=>get_service(
                      iv_service = `ZPRU_IF_AGTY_PRECHECK`
                      iv_context = zpru_if_agent_frw=>cs_context-st_agent_definition ).
      CATCH zpru_cx_agent_core.
        RETURN.
    ENDTRY.

    lo_pre->precheck_delete_agent_type( EXPORTING it_agty_delete_imp = it_agty_delete_imp
                                        IMPORTING et_entities        = et_entities
                                        CHANGING  cs_reported        = cs_reported
                                                  cs_failed          = cs_failed ).
  ENDMETHOD.

  METHOD precheck_read_agent_type.
    DATA lo_pre TYPE REF TO zpru_if_agty_precheck.

    TRY.
        lo_pre ?= zpru_cl_agent_service_mngr=>get_service(
                      iv_service = `ZPRU_IF_AGTY_PRECHECK`
                      iv_context = zpru_if_agent_frw=>cs_context-st_agent_definition ).
      CATCH zpru_cx_agent_core.
        RETURN.
    ENDTRY.

    lo_pre->precheck_read_agent_type( EXPORTING it_agty_read_k = it_agty_read_k
                                      IMPORTING et_entities    = et_entities
                                      CHANGING  cs_reported    = cs_reported
                                                cs_failed      = cs_failed ).
  ENDMETHOD.

  METHOD fill_agty_admin_fields.
    GET TIME STAMP FIELD DATA(lv_ts).

    IF iv_during_create = abap_true.
      cs_agty-instance-created_at         = lv_ts.
      cs_agty-instance-created_by         = sy-uname.
      cs_agty-instance-local_last_changed = lv_ts.
      cs_agty-instance-last_changed       = lv_ts.
      cs_agty-instance-changed_by         = sy-uname.
    ELSE.
      cs_agty-instance-local_last_changed = lv_ts.
      cs_agty-instance-last_changed       = lv_ts.
      cs_agty-instance-changed_by         = sy-uname.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
