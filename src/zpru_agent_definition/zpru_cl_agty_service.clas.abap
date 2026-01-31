CLASS zpru_cl_agty_service DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zpru_if_agent_frw.
    INTERFACES zpru_if_agty_service.

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
    DATA ls_reported TYPE zpru_if_agent_frw=>ts_agty_bndl_reported.
    DATA ls_failed   TYPE zpru_if_agent_frw=>ts_agty_bndl_failed.
    DATA lt_delete_in TYPE TABLE FOR DELETE zr_pru_agent_type\\zrpruagenttype.

    precheck_delete_agent_type( EXPORTING it_agty_delete_imp = it_agty_delete_imp
                                IMPORTING et_entities        = DATA(lt_entities)
                                CHANGING  cs_reported        = ls_reported
                                          cs_failed          = ls_failed ).

    cs_failed = CORRESPONDING #( DEEP ls_failed ).
    cs_reported = CORRESPONDING #( DEEP ls_reported ).

    IF    lt_entities          <> it_agty_delete_imp
       OR ls_failed-agent_type IS NOT INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_delete>).
       APPEND INITIAL LINE TO lt_delete_in ASSIGNING FIELD-SYMBOL(<ls_delete_in>).
       <ls_delete_in>-agenttype = <ls_delete>-agent_type.
    ENDLOOP.

    MODIFY ENTITIES OF zr_pru_agent_type
           ENTITY zrpruagenttype
           DELETE FROM lt_delete_in
           FAILED DATA(ls_del_failed)
           REPORTED DATA(ls_del_reported).

    LOOP AT ls_del_failed-zrpruagenttype ASSIGNING FIELD-SYMBOL(<ls_failed_agenttype>).
      APPEND INITIAL LINE TO cs_failed-agent_type ASSIGNING FIELD-SYMBOL(<ls_failed_target>).
      <ls_failed_target>-agent_type = <ls_failed_agenttype>-agenttype.
      <ls_failed_target>-fail       = CONV #( <ls_failed_agenttype>-%fail-cause ).
      <ls_failed_target>-delete     = <ls_failed_agenttype>-%delete.
    ENDLOOP.

    LOOP AT ls_del_reported-zrpruagenttype ASSIGNING FIELD-SYMBOL(<ls_reported_agenttype>).
      APPEND INITIAL LINE TO cs_reported-agent_type ASSIGNING FIELD-SYMBOL(<ls_reported_target>).
      <ls_reported_target>-agent_type = <ls_reported_agenttype>-agenttype.
      <ls_reported_target>-delete     = <ls_reported_agenttype>-%delete.
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
    DATA ls_reported TYPE zpru_if_agent_frw=>ts_agty_bndl_reported.
    DATA ls_failed   TYPE zpru_if_agent_frw=>ts_agty_bndl_failed.
    DATA ls_out TYPE zpru_agent_type.
    DATA lt_read_in TYPE TABLE FOR READ zr_pru_agent_type\\zrpruagenttype.

    CLEAR et_agty.

    IF it_agty_read_k IS INITIAL.
      RETURN.
    ENDIF.

    precheck_read_agent_type( EXPORTING it_agty_read_k = it_agty_read_k
                              IMPORTING et_entities    = DATA(lt_entities)
                              CHANGING  cs_reported    = ls_reported
                                        cs_failed      = ls_failed ).

    cs_failed = CORRESPONDING #( DEEP ls_failed ).
    cs_reported = CORRESPONDING #( DEEP ls_reported ).

    IF    lt_entities       <> it_agty_read_k
       OR ls_failed-agent_type IS NOT INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_req>).
      APPEND INITIAL LINE TO lt_read_in ASSIGNING FIELD-SYMBOL(<ls_read_in>).
      <ls_read_in>-agenttype = <ls_req>-agent_type.

      <ls_read_in>-%control-shortmemvolume = COND #( WHEN <ls_req>-control-short_mem_volume = abap_true
                                                    THEN if_abap_behv=>mk-on ).
      <ls_read_in>-%control-discardstrategy = COND #( WHEN <ls_req>-control-discard_strategy = abap_true
                                                     THEN if_abap_behv=>mk-on ).
      <ls_read_in>-%control-summarystrategy = COND #( WHEN <ls_req>-control-summary_strategy = abap_true
                                                     THEN if_abap_behv=>mk-on ).
      <ls_read_in>-%control-maxnumbloop = COND #( WHEN <ls_req>-control-max_numb_loop = abap_true
                                                 THEN if_abap_behv=>mk-on ).
      <ls_read_in>-%control-createdby = COND #( WHEN <ls_req>-control-created_by = abap_true
                                               THEN if_abap_behv=>mk-on ).
      <ls_read_in>-%control-createdat = COND #( WHEN <ls_req>-control-created_at = abap_true
                                               THEN if_abap_behv=>mk-on ).
      <ls_read_in>-%control-changedby = COND #( WHEN <ls_req>-control-changed_by = abap_true
                                               THEN if_abap_behv=>mk-on ).
      <ls_read_in>-%control-lastchanged = COND #( WHEN <ls_req>-control-last_changed = abap_true
                                                 THEN if_abap_behv=>mk-on ).
      <ls_read_in>-%control-locallastchanged = COND #( WHEN <ls_req>-control-local_last_changed = abap_true
                                                      THEN if_abap_behv=>mk-on ).
    ENDLOOP.

    READ ENTITIES OF zr_pru_agent_type
         ENTITY zrpruagenttype
         FROM lt_read_in
         RESULT DATA(lt_result)
         FAILED DATA(ls_rd_failed)
         REPORTED DATA(ls_rd_reported).

    LOOP AT ls_rd_failed-zrpruagenttype ASSIGNING FIELD-SYMBOL(<ls_failed_agenttype>).
      APPEND INITIAL LINE TO cs_failed-agent_type ASSIGNING FIELD-SYMBOL(<ls_failed_target>).
      <ls_failed_target>-agent_type = <ls_failed_agenttype>-agenttype.
      <ls_failed_target>-fail       = CONV #( <ls_failed_agenttype>-%fail-cause ).
    ENDLOOP.

    LOOP AT ls_rd_reported-zrpruagenttype ASSIGNING FIELD-SYMBOL(<ls_reported_agenttype>).
      APPEND INITIAL LINE TO cs_reported-agent_type ASSIGNING FIELD-SYMBOL(<ls_reported_target>).
      <ls_reported_target>-agent_type = <ls_reported_agenttype>-agenttype.
*      <ls_reported_target>-msg        =  <ls_reported_agenttype>-%msg.
    ENDLOOP.

    LOOP AT lt_result ASSIGNING FIELD-SYMBOL(<ls_res>).
        CLEAR ls_out.
        ls_out-agent_type         = ls_res-agenttype.
        ls_out-short_mem_volume   = ls_res-shortmemvolume.
        ls_out-discard_strategy   = ls_res-discardstrategy.
        ls_out-summary_strategy   = ls_res-summarystrategy.
        ls_out-max_numb_loop      = ls_res-maxnumbloop.
        ls_out-created_by         = ls_res-createdby .
        ls_out-created_at         = ls_res-createdat .
        ls_out-changed_by         = ls_res-changedby .
        ls_out-last_changed       = ls_res-lastchanged .
        ls_out-local_last_changed = ls_res-locallastchanged .
        APPEND ls_out TO et_agty.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_agty_service~update_agent_type.
    DATA ls_reported TYPE zpru_if_agent_frw=>ts_agty_bndl_reported.
    DATA ls_failed   TYPE zpru_if_agent_frw=>ts_agty_bndl_failed.
    DATA lt_update_in TYPE TABLE FOR UPDATE zr_pru_agent_type\\zrpruagenttype.

    precheck_update_agent_type( EXPORTING it_agty_update_imp = it_agty_update_imp
                                IMPORTING et_entities        = DATA(lt_entities)
                                CHANGING  cs_reported        = ls_reported
                                          cs_failed          = ls_failed ).

    cs_failed = CORRESPONDING #( DEEP ls_failed ).
    cs_reported = CORRESPONDING #( DEEP ls_reported ).

    IF    lt_entities          <> it_agty_update_imp
       OR ls_failed-agent_type IS NOT INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_update>).
      APPEND INITIAL LINE TO lt_update_in ASSIGNING FIELD-SYMBOL(<ls_update_in>).
      <ls_update_in>-agenttype = <ls_update>-agent_type.

      <ls_update_in>-shortmemvolume = COND #( WHEN <ls_update>-control-short_mem_volume = abap_true
                                              THEN <ls_update>-short_mem_volume ).
      <ls_update_in>-%control-shortmemvolume = COND #( WHEN <ls_update>-control-short_mem_volume = abap_true
                                                       THEN if_abap_behv=>mk-on ).

      <ls_update_in>-discardstrategy = COND #( WHEN <ls_update>-control-discard_strategy = abap_true
                                               THEN <ls_update>-discard_strategy ).
      <ls_update_in>-%control-discardstrategy = COND #( WHEN <ls_update>-control-discard_strategy = abap_true
                                                        THEN if_abap_behv=>mk-on ).

      <ls_update_in>-summarystrategy = COND #( WHEN <ls_update>-control-summary_strategy = abap_true
                                               THEN <ls_update>-summary_strategy ).
      <ls_update_in>-%control-summarystrategy = COND #( WHEN <ls_update>-control-summary_strategy = abap_true
                                                        THEN if_abap_behv=>mk-on ).

      <ls_update_in>-maxnumbloop = COND #( WHEN <ls_update>-control-max_numb_loop = abap_true
                                           THEN <ls_update>-max_numb_loop ).
      <ls_update_in>-%control-maxnumbloop = COND #( WHEN <ls_update>-control-max_numb_loop = abap_true
                                                    THEN if_abap_behv=>mk-on ).

      <ls_update_in>-createdby = COND #( WHEN <ls_update>-control-created_by = abap_true
                                         THEN <ls_update>-created_by ).
      <ls_update_in>-%control-createdby = COND #( WHEN <ls_update>-control-created_by = abap_true
                                                  THEN if_abap_behv=>mk-on ).

      <ls_update_in>-createdat = COND #( WHEN <ls_update>-control-created_at = abap_true
                                         THEN <ls_update>-created_at ).
      <ls_update_in>-%control-createdat = COND #( WHEN <ls_update>-control-created_at = abap_true
                                                  THEN if_abap_behv=>mk-on ).

      <ls_update_in>-changedby = COND #( WHEN <ls_update>-control-changed_by <> if_abap_behv=>mk-off
                                         THEN <ls_update>-changed_by ).
      <ls_update_in>-%control-changedby = COND #( WHEN <ls_update>-control-changed_by <> if_abap_behv=>mk-off
                                                  THEN if_abap_behv=>mk-on ).

      <ls_update_in>-lastchanged = COND #( WHEN <ls_update>-control-last_changed = abap_true
                                           THEN <ls_update>-last_changed ).
      <ls_update_in>-%control-lastchanged = COND #( WHEN <ls_update>-control-last_changed = abap_true
                                                    THEN if_abap_behv=>mk-on ).

      <ls_update_in>-locallastchanged = COND #( WHEN <ls_update>-control-local_last_changed = abap_true
                                                THEN <ls_update>-local_last_changed ).
      <ls_update_in>-%control-locallastchanged = COND #( WHEN <ls_update>-control-local_last_changed = abap_true
                                                         THEN if_abap_behv=>mk-on ).
    ENDLOOP.

    MODIFY ENTITIES OF zr_pru_agent_type
           ENTITY zrpruagenttype
           UPDATE WITH lt_update_in
           FAILED DATA(ls_up_failed)
           REPORTED DATA(ls_up_reported).

    LOOP AT ls_up_failed-zrpruagenttype ASSIGNING FIELD-SYMBOL(<ls_failed_agenttype>).
      APPEND INITIAL LINE TO cs_failed-agent_type ASSIGNING FIELD-SYMBOL(<ls_failed_target>).
      <ls_failed_target>-agent_type = <ls_failed_agenttype>-agenttype.
      <ls_failed_target>-fail       = CONV #( <ls_failed_agenttype>-%fail-cause ).
      <ls_failed_target>-update     = <ls_failed_agenttype>-%update.
    ENDLOOP.

    LOOP AT ls_up_reported-zrpruagenttype ASSIGNING FIELD-SYMBOL(<ls_reported_agenttype>).
      APPEND INITIAL LINE TO cs_reported-agent_type ASSIGNING FIELD-SYMBOL(<ls_reported_target>).
      <ls_reported_target>-agent_type = <ls_reported_agenttype>-agenttype.
      <ls_reported_target>-update     = <ls_reported_agenttype>-%update.
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

ENDCLASS.
