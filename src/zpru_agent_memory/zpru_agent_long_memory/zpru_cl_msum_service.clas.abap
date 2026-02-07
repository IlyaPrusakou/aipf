CLASS zpru_cl_msum_service DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zpru_if_agent_frw.
    INTERFACES zpru_if_msum_service.

  PROTECTED SECTION.
    METHODS precheck_create_msum
      IMPORTING it_msum_create_imp TYPE zpru_if_msum_crud=>tt_msum_create_imp
      EXPORTING et_entities        TYPE zpru_if_msum_crud=>tt_msum_create_imp
      CHANGING  cs_reported        TYPE zpru_if_agent_frw=>ts_msum_bndl_reported OPTIONAL
                cs_failed          TYPE zpru_if_agent_frw=>ts_msum_bndl_failed   OPTIONAL.

    METHODS precheck_update_msum
      IMPORTING it_msum_update_imp TYPE zpru_if_msum_crud=>tt_msum_update_imp
      EXPORTING et_entities        TYPE zpru_if_msum_crud=>tt_msum_update_imp
      CHANGING  cs_reported        TYPE zpru_if_agent_frw=>ts_msum_bndl_reported OPTIONAL
                cs_failed          TYPE zpru_if_agent_frw=>ts_msum_bndl_failed   OPTIONAL.

    METHODS precheck_delete_msum
      IMPORTING it_msum_delete_imp TYPE zpru_if_msum_crud=>tt_msum_delete_imp
      EXPORTING et_entities        TYPE zpru_if_msum_crud=>tt_msum_delete_imp
      CHANGING  cs_reported        TYPE zpru_if_agent_frw=>ts_msum_bndl_reported OPTIONAL
                cs_failed          TYPE zpru_if_agent_frw=>ts_msum_bndl_failed   OPTIONAL.

    METHODS precheck_read_msum
      IMPORTING it_msum_read_k TYPE zpru_if_msum_crud=>tt_msum_read_k
      EXPORTING et_entities    TYPE zpru_if_msum_crud=>tt_msum_read_k
      CHANGING  cs_reported    TYPE zpru_if_agent_frw=>ts_msum_bndl_reported OPTIONAL
                cs_failed      TYPE zpru_if_agent_frw=>ts_msum_bndl_failed   OPTIONAL.

ENDCLASS.


CLASS zpru_cl_msum_service IMPLEMENTATION.
  METHOD zpru_if_msum_service~clean_up.
  ENDMETHOD.

  METHOD zpru_if_msum_service~create_msum.
    DATA ls_reported  TYPE zpru_if_agent_frw=>ts_msum_bndl_reported.
    DATA ls_failed    TYPE zpru_if_agent_frw=>ts_msum_bndl_failed.
    DATA lt_create_in TYPE TABLE FOR CREATE zr_pru_mem_sum\\ZrPruMemSum.

    precheck_create_msum( EXPORTING it_msum_create_imp = it_msum_create_imp
                          IMPORTING et_entities        = DATA(lt_entities)
                          CHANGING  cs_reported        = ls_reported
                                    cs_failed          = ls_failed ).

    cs_failed = CORRESPONDING #( DEEP ls_failed ).
    cs_reported = CORRESPONDING #( DEEP ls_reported ).

    IF    lt_entities    <> it_msum_create_imp
       OR ls_failed-msum IS NOT INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_create>).
      APPEND INITIAL LINE TO lt_create_in ASSIGNING FIELD-SYMBOL(<ls_create_in>).
      <ls_create_in>-%cid        = <ls_create>-summary_uuid.
      <ls_create_in>-SummaryUUID = <ls_create>-summary_uuid.
      <ls_create_in>-Content     = COND #( WHEN <ls_create>-control-content = abap_true THEN <ls_create>-content ).
      <ls_create_in>-%control-Content = COND #( WHEN <ls_create>-control-content = abap_true THEN if_abap_behv=>mk-on ).
      <ls_create_in>-SummaryCid = COND #( WHEN <ls_create>-control-summary_cid = abap_true THEN <ls_create>-summary_cid ).
      <ls_create_in>-%control-SummaryCid = COND #( WHEN <ls_create>-control-summary_cid = abap_true
                                                   THEN if_abap_behv=>mk-on ).
      <ls_create_in>-Stage = COND #( WHEN <ls_create>-control-stage = abap_true THEN <ls_create>-stage ).
      <ls_create_in>-%control-Stage = COND #( WHEN <ls_create>-control-stage = abap_true THEN if_abap_behv=>mk-on ).
      <ls_create_in>-SubStage = COND #( WHEN <ls_create>-control-sub_stage = abap_true THEN <ls_create>-sub_stage ).
      <ls_create_in>-%control-SubStage = COND #( WHEN <ls_create>-control-sub_stage = abap_true THEN if_abap_behv=>mk-on ).
      <ls_create_in>-Namespace = COND #( WHEN <ls_create>-control-namespace = abap_true THEN <ls_create>-namespace ).
      <ls_create_in>-%control-Namespace = COND #( WHEN <ls_create>-control-namespace = abap_true
                                                  THEN if_abap_behv=>mk-on ).
      <ls_create_in>-UserName = COND #( WHEN <ls_create>-control-user_name = abap_true THEN <ls_create>-user_name ).
      <ls_create_in>-%control-UserName = COND #( WHEN <ls_create>-control-user_name = abap_true THEN if_abap_behv=>mk-on ).
      <ls_create_in>-AgentUUID = COND #( WHEN <ls_create>-control-agent_uuid = abap_true THEN <ls_create>-agent_uuid ).
      <ls_create_in>-%control-AgentUUID = COND #( WHEN <ls_create>-control-agent_uuid = abap_true
                                                  THEN if_abap_behv=>mk-on ).
      <ls_create_in>-RunUUID = COND #( WHEN <ls_create>-control-run_uuid = abap_true THEN <ls_create>-run_uuid ).
      <ls_create_in>-%control-RunUUID = COND #( WHEN <ls_create>-control-run_uuid = abap_true THEN if_abap_behv=>mk-on ).
      <ls_create_in>-QueryUUID = COND #( WHEN <ls_create>-control-query_uuid = abap_true THEN <ls_create>-query_uuid ).
      <ls_create_in>-%control-QueryUUID = COND #( WHEN <ls_create>-control-query_uuid = abap_true
                                                  THEN if_abap_behv=>mk-on ).
      <ls_create_in>-StepUUID = COND #( WHEN <ls_create>-control-step_uuid = abap_true THEN <ls_create>-step_uuid ).
      <ls_create_in>-%control-StepUUID = COND #( WHEN <ls_create>-control-step_uuid = abap_true THEN if_abap_behv=>mk-on ).
      <ls_create_in>-MessageTime = COND #( WHEN <ls_create>-control-message_time = abap_true
                                           THEN <ls_create>-message_time ).
      <ls_create_in>-%control-MessageTime = COND #( WHEN <ls_create>-control-message_time = abap_true
                                                    THEN if_abap_behv=>mk-on ).
      <ls_create_in>-CreatedBy = COND #( WHEN <ls_create>-control-created_by = abap_true THEN <ls_create>-created_by ).
      <ls_create_in>-%control-CreatedBy = COND #( WHEN <ls_create>-control-created_by = abap_true
                                                  THEN if_abap_behv=>mk-on ).
      <ls_create_in>-CreatedAt = COND #( WHEN <ls_create>-control-created_at = abap_true THEN <ls_create>-created_at ).
      <ls_create_in>-%control-CreatedAt = COND #( WHEN <ls_create>-control-created_at = abap_true
                                                  THEN if_abap_behv=>mk-on ).
      <ls_create_in>-ChangedBy = COND #( WHEN <ls_create>-control-changed_by = abap_true THEN <ls_create>-changed_by ).
      <ls_create_in>-%control-ChangedBy = COND #( WHEN <ls_create>-control-changed_by = abap_true
                                                  THEN if_abap_behv=>mk-on ).
      <ls_create_in>-ChangedAt = COND #( WHEN <ls_create>-control-changed_at = abap_true THEN <ls_create>-changed_at ).
      <ls_create_in>-%control-ChangedAt = COND #( WHEN <ls_create>-control-changed_at = abap_true
                                                  THEN if_abap_behv=>mk-on ).
    ENDLOOP.

    MODIFY ENTITIES OF zr_pru_mem_sum
           ENTITY ZrPruMemSum
           CREATE AUTO FILL CID WITH lt_create_in
           MAPPED DATA(ls_cr_mapped)
           REPORTED DATA(ls_cr_reported)
           FAILED DATA(ls_cr_failed).

    LOOP AT ls_cr_failed-zrprumemsum ASSIGNING FIELD-SYMBOL(<ls_failed_msum>).
      APPEND INITIAL LINE TO cs_failed-msum ASSIGNING FIELD-SYMBOL(<ls_failed_target>).
      <ls_failed_target>-summary_uuid = <ls_failed_msum>-SummaryUUID.
      <ls_failed_target>-fail         = CONV #( <ls_failed_msum>-%fail-cause ).
      <ls_failed_target>-create       = <ls_failed_msum>-%create.
    ENDLOOP.

    LOOP AT ls_cr_reported-zrprumemsum ASSIGNING FIELD-SYMBOL(<ls_reported_msum>).
      APPEND INITIAL LINE TO cs_reported-msum ASSIGNING FIELD-SYMBOL(<ls_reported_target>).
      <ls_reported_target>-summary_uuid = <ls_reported_msum>-SummaryUUID.
      <ls_reported_target>-create       = <ls_reported_msum>-%create.
    ENDLOOP.

    LOOP AT ls_cr_mapped-zrprumemsum ASSIGNING FIELD-SYMBOL(<ls_mapped_msum>).
      APPEND INITIAL LINE TO cs_mapped-msum ASSIGNING FIELD-SYMBOL(<ls_mapped_target>).
      <ls_mapped_target>-summary_uuid = <ls_mapped_msum>-SummaryUUID.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_msum_service~delete_msum.
    DATA ls_reported  TYPE zpru_if_agent_frw=>ts_msum_bndl_reported.
    DATA ls_failed    TYPE zpru_if_agent_frw=>ts_msum_bndl_failed.
    DATA lt_delete_in TYPE TABLE FOR DELETE zr_pru_mem_sum\\ZrPruMemSum.

    precheck_delete_msum( EXPORTING it_msum_delete_imp = it_msum_delete_imp
                          IMPORTING et_entities        = DATA(lt_entities)
                          CHANGING  cs_reported        = ls_reported
                                    cs_failed          = ls_failed ).

    cs_failed = CORRESPONDING #( DEEP ls_failed ).
    cs_reported = CORRESPONDING #( DEEP ls_reported ).

    IF    lt_entities    <> it_msum_delete_imp
       OR ls_failed-msum IS NOT INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_delete>).
      APPEND INITIAL LINE TO lt_delete_in ASSIGNING FIELD-SYMBOL(<ls_delete_in>).
      <ls_delete_in>-SummaryUUID = <ls_delete>-summary_uuid.
    ENDLOOP.

    MODIFY ENTITIES OF zr_pru_mem_sum
           ENTITY ZrPruMemSum
           DELETE FROM lt_delete_in
           FAILED DATA(ls_del_failed)
           REPORTED DATA(ls_del_reported).

    LOOP AT ls_del_failed-zrprumemsum ASSIGNING FIELD-SYMBOL(<ls_failed_msum>).
      APPEND INITIAL LINE TO cs_failed-msum ASSIGNING FIELD-SYMBOL(<ls_failed_target>).
      <ls_failed_target>-summary_uuid = <ls_failed_msum>-SummaryUUID.
      <ls_failed_target>-fail         = CONV #( <ls_failed_msum>-%fail-cause ).
      <ls_failed_target>-delete       = <ls_failed_msum>-%delete.
    ENDLOOP.

    LOOP AT ls_del_reported-zrprumemsum ASSIGNING FIELD-SYMBOL(<ls_reported_msum>).
      APPEND INITIAL LINE TO cs_reported-msum ASSIGNING FIELD-SYMBOL(<ls_reported_target>).
      <ls_reported_target>-summary_uuid = <ls_reported_msum>-SummaryUUID.
      <ls_reported_target>-delete       = <ls_reported_msum>-%delete.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_msum_service~determine.
  ENDMETHOD.

  METHOD zpru_if_msum_service~do_save.
  ENDMETHOD.

  METHOD zpru_if_msum_service~query_msum.
    CLEAR et_msum_k.

    SELECT summary_uuid FROM zpru_mem_sum
      WHERE summary_uuid IN @it_summary_uuid
        AND agent_uuid   IN @it_agent_uuid
        AND run_uuid     IN @it_run_uuid
        AND query_uuid   IN @it_query_uuid
        AND step_uuid    IN @it_step_uuid
        AND created_by   IN @it_created_by
        AND created_at   IN @it_created_at
        AND changed_by   IN @it_changed_by
        AND changed_at   IN @it_changed_at
      INTO TABLE @et_msum_k.
  ENDMETHOD.

  METHOD zpru_if_msum_service~read_msum.
    DATA ls_reported TYPE zpru_if_agent_frw=>ts_msum_bndl_reported.
    DATA ls_failed   TYPE zpru_if_agent_frw=>ts_msum_bndl_failed.
    DATA lt_read_in  TYPE TABLE FOR READ IMPORT zr_pru_mem_sum\\ZrPruMemSum.

    CLEAR et_msum.

    IF it_msum_read_k IS INITIAL.
      RETURN.
    ENDIF.

    precheck_read_msum( EXPORTING it_msum_read_k = it_msum_read_k
                        IMPORTING et_entities    = DATA(lt_entities)
                        CHANGING  cs_reported    = ls_reported
                                  cs_failed      = ls_failed ).

    cs_failed = CORRESPONDING #( DEEP ls_failed ).
    cs_reported = CORRESPONDING #( DEEP ls_reported ).

    IF    lt_entities    <> it_msum_read_k
       OR ls_failed-msum IS NOT INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_req>).
      APPEND INITIAL LINE TO lt_read_in ASSIGNING FIELD-SYMBOL(<ls_read_in>).
      <ls_read_in>-SummaryUUID = <ls_req>-summary_uuid.

      <ls_read_in>-%control-Content     = COND #( WHEN <ls_req>-control-content = abap_true THEN if_abap_behv=>mk-on ).
      <ls_read_in>-%control-SummaryCid  = COND #( WHEN <ls_req>-control-summary_cid = abap_true THEN if_abap_behv=>mk-on ).
      <ls_read_in>-%control-Stage       = COND #( WHEN <ls_req>-control-stage = abap_true THEN if_abap_behv=>mk-on ).
      <ls_read_in>-%control-SubStage    = COND #( WHEN <ls_req>-control-sub_stage = abap_true THEN if_abap_behv=>mk-on ).
      <ls_read_in>-%control-Namespace   = COND #( WHEN <ls_req>-control-namespace = abap_true THEN if_abap_behv=>mk-on ).
      <ls_read_in>-%control-UserName    = COND #( WHEN <ls_req>-control-user_name = abap_true THEN if_abap_behv=>mk-on ).
      <ls_read_in>-%control-AgentUUID   = COND #( WHEN <ls_req>-control-agent_uuid = abap_true THEN if_abap_behv=>mk-on ).
      <ls_read_in>-%control-RunUUID     = COND #( WHEN <ls_req>-control-run_uuid = abap_true THEN if_abap_behv=>mk-on ).
      <ls_read_in>-%control-QueryUUID   = COND #( WHEN <ls_req>-control-query_uuid = abap_true THEN if_abap_behv=>mk-on ).
      <ls_read_in>-%control-StepUUID    = COND #( WHEN <ls_req>-control-step_uuid = abap_true THEN if_abap_behv=>mk-on ).
      <ls_read_in>-%control-MessageTime = COND #( WHEN <ls_req>-control-message_time = abap_true
                                                  THEN if_abap_behv=>mk-on ).
      <ls_read_in>-%control-CreatedBy   = COND #( WHEN <ls_req>-control-created_by = abap_true THEN if_abap_behv=>mk-on ).
      <ls_read_in>-%control-CreatedAt   = COND #( WHEN <ls_req>-control-created_at = abap_true THEN if_abap_behv=>mk-on ).
      <ls_read_in>-%control-ChangedBy   = COND #( WHEN <ls_req>-control-changed_by = abap_true THEN if_abap_behv=>mk-on ).
      <ls_read_in>-%control-ChangedAt   = COND #( WHEN <ls_req>-control-changed_at = abap_true THEN if_abap_behv=>mk-on ).
    ENDLOOP.

    READ ENTITIES OF zr_pru_mem_sum
         ENTITY ZrPruMemSum
         FROM lt_read_in
         RESULT DATA(lt_result)
         FAILED DATA(ls_rd_failed)
         REPORTED DATA(ls_rd_reported).

    LOOP AT ls_rd_failed-zrprumemsum ASSIGNING FIELD-SYMBOL(<ls_failed_rd>).
      APPEND INITIAL LINE TO cs_failed-msum ASSIGNING FIELD-SYMBOL(<ls_failed_target_rd>).
      <ls_failed_target_rd>-summary_uuid = <ls_failed_rd>-SummaryUUID.
      <ls_failed_target_rd>-fail         = CONV #( <ls_failed_rd>-%fail-cause ).
    ENDLOOP.

    LOOP AT ls_rd_reported-zrprumemsum ASSIGNING FIELD-SYMBOL(<ls_reported_rd>).
      APPEND INITIAL LINE TO cs_reported-msum ASSIGNING FIELD-SYMBOL(<ls_reported_target_rd>).
      <ls_reported_target_rd>-summary_uuid = <ls_reported_rd>-SummaryUUID.
    ENDLOOP.

    LOOP AT lt_result ASSIGNING FIELD-SYMBOL(<ls_res>).
      APPEND INITIAL LINE TO et_msum ASSIGNING FIELD-SYMBOL(<ls_out_msum>).
      <ls_out_msum>-summary_uuid = <ls_res>-SummaryUUID.
      <ls_out_msum>-content      = <ls_res>-Content.
      <ls_out_msum>-summary_cid  = <ls_res>-SummaryCid.
      <ls_out_msum>-stage        = <ls_res>-Stage.
      <ls_out_msum>-sub_stage    = <ls_res>-SubStage.
      <ls_out_msum>-namespace    = <ls_res>-Namespace.
      <ls_out_msum>-user_name    = <ls_res>-UserName.
      <ls_out_msum>-agent_uuid   = <ls_res>-AgentUUID.
      <ls_out_msum>-run_uuid     = <ls_res>-RunUUID.
      <ls_out_msum>-query_uuid   = <ls_res>-QueryUUID.
      <ls_out_msum>-step_uuid    = <ls_res>-StepUUID.
      <ls_out_msum>-message_time = <ls_res>-MessageTime.
      <ls_out_msum>-created_by   = <ls_res>-CreatedBy.
      <ls_out_msum>-created_at   = <ls_res>-CreatedAt.
      <ls_out_msum>-changed_by   = <ls_res>-ChangedBy.
      <ls_out_msum>-changed_at   = <ls_res>-ChangedAt.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_msum_service~update_msum.
    DATA ls_reported  TYPE zpru_if_agent_frw=>ts_msum_bndl_reported.
    DATA ls_failed    TYPE zpru_if_agent_frw=>ts_msum_bndl_failed.
    DATA lt_update_in TYPE TABLE FOR UPDATE zr_pru_mem_sum\\ZrPruMemSum.

    precheck_update_msum( EXPORTING it_msum_update_imp = it_msum_update_imp
                          IMPORTING et_entities        = DATA(lt_entities)
                          CHANGING  cs_reported        = ls_reported
                                    cs_failed          = ls_failed ).

    cs_failed = CORRESPONDING #( DEEP ls_failed ).
    cs_reported = CORRESPONDING #( DEEP ls_reported ).

    IF    lt_entities    <> it_msum_update_imp
       OR ls_failed-msum IS NOT INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_update>).
      APPEND INITIAL LINE TO lt_update_in ASSIGNING FIELD-SYMBOL(<ls_update_in>).
      <ls_update_in>-SummaryUUID = <ls_update>-summary_uuid.

      <ls_update_in>-Content     = COND #( WHEN <ls_update>-control-content = abap_true THEN <ls_update>-content ).
      <ls_update_in>-%control-Content = COND #( WHEN <ls_update>-control-content = abap_true THEN if_abap_behv=>mk-on ).
      <ls_update_in>-SummaryCid = COND #( WHEN <ls_update>-control-summary_cid = abap_true THEN <ls_update>-summary_cid ).
      <ls_update_in>-%control-SummaryCid = COND #( WHEN <ls_update>-control-summary_cid = abap_true
                                                   THEN if_abap_behv=>mk-on ).
      <ls_update_in>-Stage = COND #( WHEN <ls_update>-control-stage = abap_true THEN <ls_update>-stage ).
      <ls_update_in>-%control-Stage = COND #( WHEN <ls_update>-control-stage = abap_true THEN if_abap_behv=>mk-on ).
      <ls_update_in>-SubStage = COND #( WHEN <ls_update>-control-sub_stage = abap_true THEN <ls_update>-sub_stage ).
      <ls_update_in>-%control-SubStage = COND #( WHEN <ls_update>-control-sub_stage = abap_true THEN if_abap_behv=>mk-on ).
      <ls_update_in>-Namespace = COND #( WHEN <ls_update>-control-namespace = abap_true THEN <ls_update>-namespace ).
      <ls_update_in>-%control-Namespace = COND #( WHEN <ls_update>-control-namespace = abap_true
                                                  THEN if_abap_behv=>mk-on ).
      <ls_update_in>-UserName = COND #( WHEN <ls_update>-control-user_name = abap_true THEN <ls_update>-user_name ).
      <ls_update_in>-%control-UserName = COND #( WHEN <ls_update>-control-user_name = abap_true THEN if_abap_behv=>mk-on ).
      <ls_update_in>-AgentUUID = COND #( WHEN <ls_update>-control-agent_uuid = abap_true THEN <ls_update>-agent_uuid ).
      <ls_update_in>-%control-AgentUUID = COND #( WHEN <ls_update>-control-agent_uuid = abap_true
                                                  THEN if_abap_behv=>mk-on ).
      <ls_update_in>-RunUUID = COND #( WHEN <ls_update>-control-run_uuid = abap_true THEN <ls_update>-run_uuid ).
      <ls_update_in>-%control-RunUUID = COND #( WHEN <ls_update>-control-run_uuid = abap_true THEN if_abap_behv=>mk-on ).
      <ls_update_in>-QueryUUID = COND #( WHEN <ls_update>-control-query_uuid = abap_true THEN <ls_update>-query_uuid ).
      <ls_update_in>-%control-QueryUUID = COND #( WHEN <ls_update>-control-query_uuid = abap_true
                                                  THEN if_abap_behv=>mk-on ).
      <ls_update_in>-StepUUID = COND #( WHEN <ls_update>-control-step_uuid = abap_true THEN <ls_update>-step_uuid ).
      <ls_update_in>-%control-StepUUID = COND #( WHEN <ls_update>-control-step_uuid = abap_true THEN if_abap_behv=>mk-on ).
      <ls_update_in>-MessageTime = COND #( WHEN <ls_update>-control-message_time = abap_true
                                           THEN <ls_update>-message_time ).
      <ls_update_in>-%control-MessageTime = COND #( WHEN <ls_update>-control-message_time = abap_true
                                                    THEN if_abap_behv=>mk-on ).
      <ls_update_in>-CreatedBy = COND #( WHEN <ls_update>-control-created_by = abap_true THEN <ls_update>-created_by ).
      <ls_update_in>-%control-CreatedBy = COND #( WHEN <ls_update>-control-created_by = abap_true
                                                  THEN if_abap_behv=>mk-on ).
      <ls_update_in>-CreatedAt = COND #( WHEN <ls_update>-control-created_at = abap_true THEN <ls_update>-created_at ).
      <ls_update_in>-%control-CreatedAt = COND #( WHEN <ls_update>-control-created_at = abap_true
                                                  THEN if_abap_behv=>mk-on ).
      <ls_update_in>-ChangedBy = COND #( WHEN <ls_update>-control-changed_by = abap_true THEN <ls_update>-changed_by ).
      <ls_update_in>-%control-ChangedBy = COND #( WHEN <ls_update>-control-changed_by = abap_true
                                                  THEN if_abap_behv=>mk-on ).
      <ls_update_in>-ChangedAt = COND #( WHEN <ls_update>-control-changed_at = abap_true THEN <ls_update>-changed_at ).
      <ls_update_in>-%control-ChangedAt = COND #( WHEN <ls_update>-control-changed_at = abap_true
                                                  THEN if_abap_behv=>mk-on ).
    ENDLOOP.

    MODIFY ENTITIES OF zr_pru_mem_sum
           ENTITY ZrPruMemSum
           UPDATE FROM lt_update_in
           FAILED DATA(ls_up_failed)
           REPORTED DATA(ls_up_reported).

    LOOP AT ls_up_failed-zrprumemsum ASSIGNING FIELD-SYMBOL(<ls_failed_up>).
      APPEND INITIAL LINE TO cs_failed-msum ASSIGNING FIELD-SYMBOL(<ls_failed_target_up>).
      <ls_failed_target_up>-summary_uuid = <ls_failed_up>-SummaryUUID.
      <ls_failed_target_up>-fail         = CONV #( <ls_failed_up>-%fail-cause ).
      <ls_failed_target_up>-update       = <ls_failed_up>-%update.
    ENDLOOP.

    LOOP AT ls_up_reported-zrprumemsum ASSIGNING FIELD-SYMBOL(<ls_reported_up>).
      APPEND INITIAL LINE TO cs_reported-msum ASSIGNING FIELD-SYMBOL(<ls_reported_target_up>).
      <ls_reported_target_up>-summary_uuid = <ls_reported_up>-SummaryUUID.
      <ls_reported_target_up>-update       = <ls_reported_up>-%update.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_msum_service~validate.
  ENDMETHOD.

  METHOD precheck_create_msum.
    DATA lo_pre TYPE REF TO zpru_if_msum_precheck.

    TRY.
        lo_pre ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_MSUM_PRECHECK`
                                                           iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RETURN.
    ENDTRY.

    lo_pre->precheck_create_msum( EXPORTING it_msum_create_imp = it_msum_create_imp
                                  IMPORTING et_entities        = et_entities
                                  CHANGING  cs_reported        = cs_reported
                                            cs_failed          = cs_failed ).
  ENDMETHOD.

  METHOD precheck_delete_msum.
    DATA lo_pre TYPE REF TO zpru_if_msum_precheck.

    TRY.
        lo_pre ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_MSUM_PRECHECK`
                                                           iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RETURN.
    ENDTRY.

    lo_pre->precheck_delete_msum( EXPORTING it_msum_delete_imp = it_msum_delete_imp
                                  IMPORTING et_entities        = et_entities
                                  CHANGING  cs_reported        = cs_reported
                                            cs_failed          = cs_failed ).
  ENDMETHOD.

  METHOD precheck_read_msum.
    DATA lo_pre TYPE REF TO zpru_if_msum_precheck.

    TRY.
        lo_pre ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_MSUM_PRECHECK`
                                                           iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RETURN.
    ENDTRY.

    lo_pre->precheck_read_msum( EXPORTING it_msum_read_k = it_msum_read_k
                                IMPORTING et_entities    = et_entities
                                CHANGING  cs_reported    = cs_reported
                                          cs_failed      = cs_failed ).
  ENDMETHOD.

  METHOD precheck_update_msum.
    DATA lo_pre TYPE REF TO zpru_if_msum_precheck.

    TRY.
        lo_pre ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_MSUM_PRECHECK`
                                                           iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RETURN.
    ENDTRY.

    lo_pre->precheck_update_msum( EXPORTING it_msum_update_imp = it_msum_update_imp
                                  IMPORTING et_entities        = et_entities
                                  CHANGING  cs_reported        = cs_reported
                                            cs_failed          = cs_failed ).
  ENDMETHOD.
ENDCLASS.
