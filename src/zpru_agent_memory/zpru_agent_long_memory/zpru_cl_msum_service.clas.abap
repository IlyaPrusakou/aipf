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
    DATA lt_create_in TYPE TABLE FOR CREATE zr_pru_mem_sum\\zrprumemsum.

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
      <ls_create_in>-%cid        = <ls_create>-summaryuuid.
      <ls_create_in>-aipf7summaryuuid = <ls_create>-summaryuuid.
      <ls_create_in>-aipf7content     = COND #( WHEN <ls_create>-control-content = abap_true THEN <ls_create>-content ).
      <ls_create_in>-%control-aipf7content = COND #( WHEN <ls_create>-control-content = abap_true THEN if_abap_behv=>mk-on ).
      <ls_create_in>-aipf7summarycontentid = COND #( WHEN <ls_create>-control-summarycontentid = abap_true THEN <ls_create>-summarycontentid ).
      <ls_create_in>-%control-aipf7summarycontentid = COND #( WHEN <ls_create>-control-summarycontentid = abap_true
                                                   THEN if_abap_behv=>mk-on ).
      <ls_create_in>-aipf7stage = COND #( WHEN <ls_create>-control-stage = abap_true THEN <ls_create>-stage ).
      <ls_create_in>-%control-aipf7stage = COND #( WHEN <ls_create>-control-stage = abap_true THEN if_abap_behv=>mk-on ).
      <ls_create_in>-aipf7substage = COND #( WHEN <ls_create>-control-substage = abap_true THEN <ls_create>-substage ).
      <ls_create_in>-%control-aipf7substage = COND #( WHEN <ls_create>-control-substage = abap_true THEN if_abap_behv=>mk-on ).
      <ls_create_in>-aipf7namespace = COND #( WHEN <ls_create>-control-namespace = abap_true THEN <ls_create>-namespace ).
      <ls_create_in>-%control-aipf7namespace = COND #( WHEN <ls_create>-control-namespace = abap_true
                                                  THEN if_abap_behv=>mk-on ).
      <ls_create_in>-aipf7username = COND #( WHEN <ls_create>-control-username = abap_true THEN <ls_create>-username ).
      <ls_create_in>-%control-aipf7username = COND #( WHEN <ls_create>-control-username = abap_true THEN if_abap_behv=>mk-on ).
      <ls_create_in>-aipf7agentuuid = COND #( WHEN <ls_create>-control-agentuuid = abap_true THEN <ls_create>-agentuuid ).
      <ls_create_in>-%control-aipf7agentuuid = COND #( WHEN <ls_create>-control-agentuuid = abap_true
                                                  THEN if_abap_behv=>mk-on ).
      <ls_create_in>-aipf7runuuid = COND #( WHEN <ls_create>-control-runuuid = abap_true THEN <ls_create>-runuuid ).
      <ls_create_in>-%control-aipf7runuuid = COND #( WHEN <ls_create>-control-runuuid = abap_true THEN if_abap_behv=>mk-on ).
      <ls_create_in>-aipf7queryuuid = COND #( WHEN <ls_create>-control-queryuuid = abap_true THEN <ls_create>-queryuuid ).
      <ls_create_in>-%control-aipf7queryuuid = COND #( WHEN <ls_create>-control-queryuuid = abap_true
                                                  THEN if_abap_behv=>mk-on ).
      <ls_create_in>-aipf7stepuuid = COND #( WHEN <ls_create>-control-stepuuid = abap_true THEN <ls_create>-stepuuid ).
      <ls_create_in>-%control-aipf7stepuuid = COND #( WHEN <ls_create>-control-stepuuid = abap_true THEN if_abap_behv=>mk-on ).
      <ls_create_in>-aipf7messagedatetime = COND #( WHEN <ls_create>-control-messagedatetime = abap_true
                                           THEN <ls_create>-messagedatetime ).
      <ls_create_in>-%control-aipf7messagedatetime = COND #( WHEN <ls_create>-control-messagedatetime = abap_true
                                                    THEN if_abap_behv=>mk-on ).
      <ls_create_in>-aipf7createdby = COND #( WHEN <ls_create>-control-createdby = abap_true THEN <ls_create>-createdby ).
      <ls_create_in>-%control-aipf7createdby = COND #( WHEN <ls_create>-control-createdby = abap_true
                                                  THEN if_abap_behv=>mk-on ).
      <ls_create_in>-aipf7createdat = COND #( WHEN <ls_create>-control-createdat = abap_true THEN <ls_create>-createdat ).
      <ls_create_in>-%control-aipf7createdat = COND #( WHEN <ls_create>-control-createdat = abap_true
                                                  THEN if_abap_behv=>mk-on ).
      <ls_create_in>-aipf7changedby = COND #( WHEN <ls_create>-control-changedby = abap_true THEN <ls_create>-changedby ).
      <ls_create_in>-%control-aipf7changedby = COND #( WHEN <ls_create>-control-changedby = abap_true
                                                  THEN if_abap_behv=>mk-on ).
      <ls_create_in>-aipf7changedat = COND #( WHEN <ls_create>-control-changedat = abap_true THEN <ls_create>-changedat ).
      <ls_create_in>-%control-aipf7changedat = COND #( WHEN <ls_create>-control-changedat = abap_true
                                                  THEN if_abap_behv=>mk-on ).
    ENDLOOP.

    MODIFY ENTITIES OF zr_pru_mem_sum
           ENTITY zrprumemsum
           CREATE AUTO FILL CID WITH lt_create_in
           MAPPED DATA(ls_cr_mapped)
           REPORTED DATA(ls_cr_reported)
           FAILED DATA(ls_cr_failed).

    LOOP AT ls_cr_failed-zrprumemsum ASSIGNING FIELD-SYMBOL(<ls_failed_msum>).
      APPEND INITIAL LINE TO cs_failed-msum ASSIGNING FIELD-SYMBOL(<ls_failed_target>).
      <ls_failed_target>-summaryuuid = <ls_failed_msum>-aipf7summaryuuid.
      <ls_failed_target>-fail         = CONV #( <ls_failed_msum>-%fail-cause ).
      <ls_failed_target>-create       = <ls_failed_msum>-%create.
    ENDLOOP.

    LOOP AT ls_cr_reported-zrprumemsum ASSIGNING FIELD-SYMBOL(<ls_reported_msum>).
      APPEND INITIAL LINE TO cs_reported-msum ASSIGNING FIELD-SYMBOL(<ls_reported_target>).
      <ls_reported_target>-summaryuuid = <ls_reported_msum>-aipf7summaryuuid.
      <ls_reported_target>-create       = <ls_reported_msum>-%create.
    ENDLOOP.

    LOOP AT ls_cr_mapped-zrprumemsum ASSIGNING FIELD-SYMBOL(<ls_mapped_msum>).
      APPEND INITIAL LINE TO cs_mapped-msum ASSIGNING FIELD-SYMBOL(<ls_mapped_target>).
      <ls_mapped_target>-summaryuuid = <ls_mapped_msum>-aipf7summaryuuid.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_msum_service~delete_msum.
    DATA ls_reported  TYPE zpru_if_agent_frw=>ts_msum_bndl_reported.
    DATA ls_failed    TYPE zpru_if_agent_frw=>ts_msum_bndl_failed.
    DATA lt_delete_in TYPE TABLE FOR DELETE zr_pru_mem_sum\\zrprumemsum.

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
      <ls_delete_in>-aipf7summaryuuid = <ls_delete>-summaryuuid.
    ENDLOOP.

    MODIFY ENTITIES OF zr_pru_mem_sum
           ENTITY zrprumemsum
           DELETE FROM lt_delete_in
           FAILED DATA(ls_del_failed)
           REPORTED DATA(ls_del_reported).

    LOOP AT ls_del_failed-zrprumemsum ASSIGNING FIELD-SYMBOL(<ls_failed_msum>).
      APPEND INITIAL LINE TO cs_failed-msum ASSIGNING FIELD-SYMBOL(<ls_failed_target>).
      <ls_failed_target>-summaryuuid = <ls_failed_msum>-aipf7summaryuuid.
      <ls_failed_target>-fail         = CONV #( <ls_failed_msum>-%fail-cause ).
      <ls_failed_target>-delete       = <ls_failed_msum>-%delete.
    ENDLOOP.

    LOOP AT ls_del_reported-zrprumemsum ASSIGNING FIELD-SYMBOL(<ls_reported_msum>).
      APPEND INITIAL LINE TO cs_reported-msum ASSIGNING FIELD-SYMBOL(<ls_reported_target>).
      <ls_reported_target>-summaryuuid = <ls_reported_msum>-aipf7summaryuuid.
      <ls_reported_target>-delete       = <ls_reported_msum>-%delete.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_msum_service~determine.
  ENDMETHOD.

  METHOD zpru_if_msum_service~do_save.
  ENDMETHOD.

  METHOD zpru_if_msum_service~query_msum.
    CLEAR et_msum_k.

    SELECT summaryuuid FROM zpru_mem_sum
      WHERE summaryuuid IN @it_summary_uuid
        AND agentuuid   IN @it_agent_uuid
        AND runuuid     IN @it_run_uuid
        AND queryuuid   IN @it_query_uuid
        AND stepuuid    IN @it_step_uuid
        AND createdby   IN @it_created_by
        AND createdat   IN @it_created_at
        AND changedby   IN @it_changed_by
        AND changedat   IN @it_changed_at
      INTO TABLE @et_msum_k.
  ENDMETHOD.

  METHOD zpru_if_msum_service~read_msum.
    DATA ls_reported TYPE zpru_if_agent_frw=>ts_msum_bndl_reported.
    DATA ls_failed   TYPE zpru_if_agent_frw=>ts_msum_bndl_failed.
    DATA lt_read_in  TYPE TABLE FOR READ IMPORT zr_pru_mem_sum\\zrprumemsum.

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
      <ls_read_in>-aipf7summaryuuid = <ls_req>-summaryuuid.

      <ls_read_in>-%control-aipf7content     = COND #( WHEN <ls_req>-control-content = abap_true THEN if_abap_behv=>mk-on ).
      <ls_read_in>-%control-aipf7summarycontentid  = COND #( WHEN <ls_req>-control-summarycontentid = abap_true THEN if_abap_behv=>mk-on ).
      <ls_read_in>-%control-aipf7stage       = COND #( WHEN <ls_req>-control-stage = abap_true THEN if_abap_behv=>mk-on ).
      <ls_read_in>-%control-aipf7substage    = COND #( WHEN <ls_req>-control-substage = abap_true THEN if_abap_behv=>mk-on ).
      <ls_read_in>-%control-aipf7namespace   = COND #( WHEN <ls_req>-control-namespace = abap_true THEN if_abap_behv=>mk-on ).
      <ls_read_in>-%control-aipf7username    = COND #( WHEN <ls_req>-control-username = abap_true THEN if_abap_behv=>mk-on ).
      <ls_read_in>-%control-aipf7agentuuid   = COND #( WHEN <ls_req>-control-agentuuid = abap_true THEN if_abap_behv=>mk-on ).
      <ls_read_in>-%control-aipf7runuuid     = COND #( WHEN <ls_req>-control-runuuid = abap_true THEN if_abap_behv=>mk-on ).
      <ls_read_in>-%control-aipf7queryuuid   = COND #( WHEN <ls_req>-control-queryuuid = abap_true THEN if_abap_behv=>mk-on ).
      <ls_read_in>-%control-aipf7stepuuid    = COND #( WHEN <ls_req>-control-stepuuid = abap_true THEN if_abap_behv=>mk-on ).
      <ls_read_in>-%control-aipf7messagedatetime = COND #( WHEN <ls_req>-control-messagedatetime = abap_true
                                                  THEN if_abap_behv=>mk-on ).
      <ls_read_in>-%control-aipf7createdby   = COND #( WHEN <ls_req>-control-createdby = abap_true THEN if_abap_behv=>mk-on ).
      <ls_read_in>-%control-aipf7createdat   = COND #( WHEN <ls_req>-control-createdat = abap_true THEN if_abap_behv=>mk-on ).
      <ls_read_in>-%control-aipf7changedby   = COND #( WHEN <ls_req>-control-changedby = abap_true THEN if_abap_behv=>mk-on ).
      <ls_read_in>-%control-aipf7changedat   = COND #( WHEN <ls_req>-control-changedat = abap_true THEN if_abap_behv=>mk-on ).
    ENDLOOP.

    READ ENTITIES OF zr_pru_mem_sum
         ENTITY zrprumemsum
         FROM lt_read_in
         RESULT DATA(lt_result)
         FAILED DATA(ls_rd_failed)
         REPORTED DATA(ls_rd_reported).

    LOOP AT ls_rd_failed-zrprumemsum ASSIGNING FIELD-SYMBOL(<ls_failed_rd>).
      APPEND INITIAL LINE TO cs_failed-msum ASSIGNING FIELD-SYMBOL(<ls_failed_target_rd>).
      <ls_failed_target_rd>-summaryuuid = <ls_failed_rd>-aipf7summaryuuid.
      <ls_failed_target_rd>-fail         = CONV #( <ls_failed_rd>-%fail-cause ).
    ENDLOOP.

    LOOP AT ls_rd_reported-zrprumemsum ASSIGNING FIELD-SYMBOL(<ls_reported_rd>).
      APPEND INITIAL LINE TO cs_reported-msum ASSIGNING FIELD-SYMBOL(<ls_reported_target_rd>).
      <ls_reported_target_rd>-summaryuuid = <ls_reported_rd>-aipf7summaryuuid.
    ENDLOOP.

    LOOP AT lt_result ASSIGNING FIELD-SYMBOL(<ls_res>).
      APPEND INITIAL LINE TO et_msum ASSIGNING FIELD-SYMBOL(<ls_out_msum>).
      <ls_out_msum>-summaryuuid = <ls_res>-aipf7summaryuuid.
      <ls_out_msum>-content      = <ls_res>-aipf7content.
      <ls_out_msum>-summarycontentid  = <ls_res>-aipf7summarycontentid.
      <ls_out_msum>-stage        = <ls_res>-aipf7stage.
      <ls_out_msum>-substage    = <ls_res>-aipf7substage.
      <ls_out_msum>-namespace    = <ls_res>-aipf7namespace.
      <ls_out_msum>-username    = <ls_res>-aipf7username.
      <ls_out_msum>-agentuuid   = <ls_res>-aipf7agentuuid.
      <ls_out_msum>-runuuid     = <ls_res>-aipf7runuuid.
      <ls_out_msum>-queryuuid   = <ls_res>-aipf7queryuuid.
      <ls_out_msum>-stepuuid    = <ls_res>-aipf7stepuuid.
      <ls_out_msum>-messagedatetime = <ls_res>-aipf7messagedatetime.
      <ls_out_msum>-createdby   = <ls_res>-aipf7createdby.
      <ls_out_msum>-createdat   = <ls_res>-aipf7createdat.
      <ls_out_msum>-changedby   = <ls_res>-aipf7changedby.
      <ls_out_msum>-changedat   = <ls_res>-aipf7changedat.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_msum_service~update_msum.
    DATA ls_reported  TYPE zpru_if_agent_frw=>ts_msum_bndl_reported.
    DATA ls_failed    TYPE zpru_if_agent_frw=>ts_msum_bndl_failed.
    DATA lt_update_in TYPE TABLE FOR UPDATE zr_pru_mem_sum\\zrprumemsum.

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
      <ls_update_in>-aipf7summaryuuid = <ls_update>-summaryuuid.

      <ls_update_in>-aipf7content     = COND #( WHEN <ls_update>-control-content = abap_true THEN <ls_update>-content ).
      <ls_update_in>-%control-aipf7content = COND #( WHEN <ls_update>-control-content = abap_true THEN if_abap_behv=>mk-on ).
      <ls_update_in>-aipf7summarycontentid = COND #( WHEN <ls_update>-control-summarycontentid = abap_true THEN <ls_update>-summarycontentid ).
      <ls_update_in>-%control-aipf7summarycontentid = COND #( WHEN <ls_update>-control-summarycontentid = abap_true
                                                   THEN if_abap_behv=>mk-on ).
      <ls_update_in>-aipf7stage = COND #( WHEN <ls_update>-control-stage = abap_true THEN <ls_update>-stage ).
      <ls_update_in>-%control-aipf7stage = COND #( WHEN <ls_update>-control-stage = abap_true THEN if_abap_behv=>mk-on ).
      <ls_update_in>-aipf7substage = COND #( WHEN <ls_update>-control-substage = abap_true THEN <ls_update>-substage ).
      <ls_update_in>-%control-aipf7substage = COND #( WHEN <ls_update>-control-substage = abap_true THEN if_abap_behv=>mk-on ).
      <ls_update_in>-aipf7namespace = COND #( WHEN <ls_update>-control-namespace = abap_true THEN <ls_update>-namespace ).
      <ls_update_in>-%control-aipf7namespace = COND #( WHEN <ls_update>-control-namespace = abap_true
                                                  THEN if_abap_behv=>mk-on ).
      <ls_update_in>-aipf7username = COND #( WHEN <ls_update>-control-username = abap_true THEN <ls_update>-username ).
      <ls_update_in>-%control-aipf7username = COND #( WHEN <ls_update>-control-username = abap_true THEN if_abap_behv=>mk-on ).
      <ls_update_in>-aipf7agentuuid = COND #( WHEN <ls_update>-control-agentuuid = abap_true THEN <ls_update>-agentuuid ).
      <ls_update_in>-%control-aipf7agentuuid = COND #( WHEN <ls_update>-control-agentuuid = abap_true
                                                  THEN if_abap_behv=>mk-on ).
      <ls_update_in>-aipf7runuuid = COND #( WHEN <ls_update>-control-runuuid = abap_true THEN <ls_update>-runuuid ).
      <ls_update_in>-%control-aipf7runuuid = COND #( WHEN <ls_update>-control-runuuid = abap_true THEN if_abap_behv=>mk-on ).
      <ls_update_in>-aipf7queryuuid = COND #( WHEN <ls_update>-control-queryuuid = abap_true THEN <ls_update>-queryuuid ).
      <ls_update_in>-%control-aipf7queryuuid = COND #( WHEN <ls_update>-control-queryuuid = abap_true
                                                  THEN if_abap_behv=>mk-on ).
      <ls_update_in>-aipf7stepuuid = COND #( WHEN <ls_update>-control-stepuuid = abap_true THEN <ls_update>-stepuuid ).
      <ls_update_in>-%control-aipf7stepuuid = COND #( WHEN <ls_update>-control-stepuuid = abap_true THEN if_abap_behv=>mk-on ).
      <ls_update_in>-aipf7messagedatetime = COND #( WHEN <ls_update>-control-messagedatetime = abap_true
                                           THEN <ls_update>-messagedatetime ).
      <ls_update_in>-%control-aipf7messagedatetime = COND #( WHEN <ls_update>-control-messagedatetime = abap_true
                                                    THEN if_abap_behv=>mk-on ).
      <ls_update_in>-aipf7createdby = COND #( WHEN <ls_update>-control-createdby = abap_true THEN <ls_update>-createdby ).
      <ls_update_in>-%control-aipf7createdby = COND #( WHEN <ls_update>-control-createdby = abap_true
                                                  THEN if_abap_behv=>mk-on ).
      <ls_update_in>-aipf7createdat = COND #( WHEN <ls_update>-control-createdat = abap_true THEN <ls_update>-createdat ).
      <ls_update_in>-%control-aipf7createdat = COND #( WHEN <ls_update>-control-createdat = abap_true
                                                  THEN if_abap_behv=>mk-on ).
      <ls_update_in>-aipf7changedby = COND #( WHEN <ls_update>-control-changedby = abap_true THEN <ls_update>-changedby ).
      <ls_update_in>-%control-aipf7changedby = COND #( WHEN <ls_update>-control-changedby = abap_true
                                                  THEN if_abap_behv=>mk-on ).
      <ls_update_in>-aipf7changedat = COND #( WHEN <ls_update>-control-changedat = abap_true THEN <ls_update>-changedat ).
      <ls_update_in>-%control-aipf7changedat = COND #( WHEN <ls_update>-control-changedat = abap_true
                                                  THEN if_abap_behv=>mk-on ).
    ENDLOOP.

    MODIFY ENTITIES OF zr_pru_mem_sum
           ENTITY zrprumemsum
           UPDATE FROM lt_update_in
           FAILED DATA(ls_up_failed)
           REPORTED DATA(ls_up_reported).

    LOOP AT ls_up_failed-zrprumemsum ASSIGNING FIELD-SYMBOL(<ls_failed_up>).
      APPEND INITIAL LINE TO cs_failed-msum ASSIGNING FIELD-SYMBOL(<ls_failed_target_up>).
      <ls_failed_target_up>-summaryuuid = <ls_failed_up>-aipf7summaryuuid.
      <ls_failed_target_up>-fail         = CONV #( <ls_failed_up>-%fail-cause ).
      <ls_failed_target_up>-update       = <ls_failed_up>-%update.
    ENDLOOP.

    LOOP AT ls_up_reported-zrprumemsum ASSIGNING FIELD-SYMBOL(<ls_reported_up>).
      APPEND INITIAL LINE TO cs_reported-msum ASSIGNING FIELD-SYMBOL(<ls_reported_target_up>).
      <ls_reported_target_up>-summaryuuid = <ls_reported_up>-aipf7summaryuuid.
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
