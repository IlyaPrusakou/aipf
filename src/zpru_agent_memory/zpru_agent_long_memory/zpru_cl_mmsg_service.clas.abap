CLASS zpru_cl_mmsg_service DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zpru_if_agent_frw.
    INTERFACES zpru_if_mmsg_service.

  PROTECTED SECTION.
    METHODS precheck_create_mmsg
      IMPORTING it_mmsg_create_imp TYPE zpru_if_mmsg_crud=>tt_mmsg_create_imp
      EXPORTING et_entities        TYPE zpru_if_mmsg_crud=>tt_mmsg_create_imp
      CHANGING  cs_reported        TYPE zpru_if_agent_frw=>ts_mmsg_bndl_reported OPTIONAL
                cs_failed          TYPE zpru_if_agent_frw=>ts_mmsg_bndl_failed   OPTIONAL.

    METHODS precheck_update_mmsg
      IMPORTING it_mmsg_update_imp TYPE zpru_if_mmsg_crud=>tt_mmsg_update_imp
      EXPORTING et_entities        TYPE zpru_if_mmsg_crud=>tt_mmsg_update_imp
      CHANGING  cs_reported        TYPE zpru_if_agent_frw=>ts_mmsg_bndl_reported OPTIONAL
                cs_failed          TYPE zpru_if_agent_frw=>ts_mmsg_bndl_failed   OPTIONAL.

    METHODS precheck_delete_mmsg
      IMPORTING it_mmsg_delete_imp TYPE zpru_if_mmsg_crud=>tt_mmsg_delete_imp
      EXPORTING et_entities        TYPE zpru_if_mmsg_crud=>tt_mmsg_delete_imp
      CHANGING  cs_reported        TYPE zpru_if_agent_frw=>ts_mmsg_bndl_reported OPTIONAL
                cs_failed          TYPE zpru_if_agent_frw=>ts_mmsg_bndl_failed   OPTIONAL.

    METHODS precheck_read_mmsg
      IMPORTING it_mmsg_read_k TYPE zpru_if_mmsg_crud=>tt_mmsg_read_k
      EXPORTING et_entities    TYPE zpru_if_mmsg_crud=>tt_mmsg_read_k
      CHANGING  cs_reported    TYPE zpru_if_agent_frw=>ts_mmsg_bndl_reported OPTIONAL
                cs_failed      TYPE zpru_if_agent_frw=>ts_mmsg_bndl_failed   OPTIONAL.

ENDCLASS.


CLASS zpru_cl_mmsg_service IMPLEMENTATION.
  METHOD zpru_if_mmsg_service~clean_up.
  ENDMETHOD.

  METHOD zpru_if_mmsg_service~create_mmsg.
    DATA ls_reported  TYPE zpru_if_agent_frw=>ts_mmsg_bndl_reported.
    DATA ls_failed    TYPE zpru_if_agent_frw=>ts_mmsg_bndl_failed.
    DATA lt_create_in TYPE TABLE FOR CREATE zr_pru_mem_msg\\ZrPruMemMsg.

    precheck_create_mmsg( EXPORTING it_mmsg_create_imp = it_mmsg_create_imp
                          IMPORTING et_entities        = DATA(lt_entities)
                          CHANGING  cs_reported        = ls_reported
                                    cs_failed          = ls_failed ).

    cs_failed = CORRESPONDING #( DEEP ls_failed ).
    cs_reported = CORRESPONDING #( DEEP ls_reported ).

    IF    lt_entities    <> it_mmsg_create_imp
       OR ls_failed-mmsg IS NOT INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_create>).
      APPEND INITIAL LINE TO lt_create_in ASSIGNING FIELD-SYMBOL(<ls_create_in>).
      <ls_create_in>-%cid        = <ls_create>-messageuuid.
      <ls_create_in>-MessageUUID = <ls_create>-messageuuid.
      <ls_create_in>-Content     = COND #( WHEN <ls_create>-control-content = abap_true THEN <ls_create>-content ).
      <ls_create_in>-%control-Content = COND #( WHEN <ls_create>-control-content = abap_true THEN if_abap_behv=>mk-on ).
      <ls_create_in>-MessageType = COND #( WHEN <ls_create>-control-messagetype = abap_true
                                           THEN <ls_create>-messagetype ).
      <ls_create_in>-%control-MessageType = COND #( WHEN <ls_create>-control-messagetype = abap_true
                                                    THEN if_abap_behv=>mk-on ).
      <ls_create_in>-MessageCid = COND #( WHEN <ls_create>-control-messagecid = abap_true THEN <ls_create>-messagecid ).
      <ls_create_in>-%control-MessageCid = COND #( WHEN <ls_create>-control-messagecid = abap_true
                                                   THEN if_abap_behv=>mk-on ).
      <ls_create_in>-Stage = COND #( WHEN <ls_create>-control-stage = abap_true THEN <ls_create>-stage ).
      <ls_create_in>-%control-Stage = COND #( WHEN <ls_create>-control-stage = abap_true THEN if_abap_behv=>mk-on ).
      <ls_create_in>-SubStage = COND #( WHEN <ls_create>-control-substage = abap_true THEN <ls_create>-substage ).
      <ls_create_in>-%control-SubStage = COND #( WHEN <ls_create>-control-substage = abap_true THEN if_abap_behv=>mk-on ).
      <ls_create_in>-Namespace = COND #( WHEN <ls_create>-control-namespace = abap_true THEN <ls_create>-namespace ).
      <ls_create_in>-%control-Namespace = COND #( WHEN <ls_create>-control-namespace = abap_true
                                                  THEN if_abap_behv=>mk-on ).
      <ls_create_in>-UserName = COND #( WHEN <ls_create>-control-username = abap_true THEN <ls_create>-username ).
      <ls_create_in>-%control-UserName = COND #( WHEN <ls_create>-control-username = abap_true THEN if_abap_behv=>mk-on ).
      <ls_create_in>-AgentUUID = COND #( WHEN <ls_create>-control-agentuuid = abap_true THEN <ls_create>-agentuuid ).
      <ls_create_in>-%control-AgentUUID = COND #( WHEN <ls_create>-control-agentuuid = abap_true
                                                  THEN if_abap_behv=>mk-on ).
      <ls_create_in>-RunUUID = COND #( WHEN <ls_create>-control-runuuid = abap_true THEN <ls_create>-runuuid ).
      <ls_create_in>-%control-RunUUID = COND #( WHEN <ls_create>-control-runuuid = abap_true THEN if_abap_behv=>mk-on ).
      <ls_create_in>-QueryUUID = COND #( WHEN <ls_create>-control-queryuuid = abap_true THEN <ls_create>-queryuuid ).
      <ls_create_in>-%control-QueryUUID = COND #( WHEN <ls_create>-control-queryuuid = abap_true
                                                  THEN if_abap_behv=>mk-on ).
      <ls_create_in>-StepUUID = COND #( WHEN <ls_create>-control-stepuuid = abap_true THEN <ls_create>-stepuuid ).
      <ls_create_in>-%control-StepUUID = COND #( WHEN <ls_create>-control-stepuuid = abap_true THEN if_abap_behv=>mk-on ).
      <ls_create_in>-MessageTime = COND #( WHEN <ls_create>-control-messagetime = abap_true
                                           THEN <ls_create>-messagetime ).
      <ls_create_in>-%control-MessageTime = COND #( WHEN <ls_create>-control-messagetime = abap_true
                                                    THEN if_abap_behv=>mk-on ).
      <ls_create_in>-CreatedBy = COND #( WHEN <ls_create>-control-createdby = abap_true THEN <ls_create>-createdby ).
      <ls_create_in>-%control-CreatedBy = COND #( WHEN <ls_create>-control-createdby = abap_true
                                                  THEN if_abap_behv=>mk-on ).
      <ls_create_in>-CreatedAt = COND #( WHEN <ls_create>-control-createdat = abap_true THEN <ls_create>-createdat ).
      <ls_create_in>-%control-CreatedAt = COND #( WHEN <ls_create>-control-createdat = abap_true
                                                  THEN if_abap_behv=>mk-on ).
      <ls_create_in>-ChangedBy = COND #( WHEN <ls_create>-control-changedby = abap_true THEN <ls_create>-changedby ).
      <ls_create_in>-%control-ChangedBy = COND #( WHEN <ls_create>-control-changedby = abap_true
                                                  THEN if_abap_behv=>mk-on ).
      <ls_create_in>-ChangedAt = COND #( WHEN <ls_create>-control-changedat = abap_true THEN <ls_create>-changedat ).
      <ls_create_in>-%control-ChangedAt = COND #( WHEN <ls_create>-control-changedat = abap_true
                                                  THEN if_abap_behv=>mk-on ).
    ENDLOOP.

    MODIFY ENTITIES OF zr_pru_mem_msg
           ENTITY ZrPruMemMsg
           CREATE AUTO FILL CID WITH lt_create_in
           MAPPED DATA(ls_cr_mapped)
           REPORTED DATA(ls_cr_reported)
           FAILED DATA(ls_cr_failed).

    LOOP AT ls_cr_failed-zrprumemmsg ASSIGNING FIELD-SYMBOL(<ls_failed_mmsg>).
      APPEND INITIAL LINE TO cs_failed-mmsg ASSIGNING FIELD-SYMBOL(<ls_failed_target>).
      <ls_failed_target>-messageuuid = <ls_failed_mmsg>-MessageUUID.
      <ls_failed_target>-fail        = CONV #( <ls_failed_mmsg>-%fail-cause ).
      <ls_failed_target>-create      = <ls_failed_mmsg>-%create.
    ENDLOOP.

    LOOP AT ls_cr_reported-zrprumemmsg ASSIGNING FIELD-SYMBOL(<ls_reported_mmsg>).
      APPEND INITIAL LINE TO cs_reported-mmsg ASSIGNING FIELD-SYMBOL(<ls_reported_target>).
      <ls_reported_target>-messageuuid = <ls_reported_mmsg>-MessageUUID.
      <ls_reported_target>-create      = <ls_reported_mmsg>-%create.
    ENDLOOP.

    LOOP AT ls_cr_mapped-zrprumemmsg ASSIGNING FIELD-SYMBOL(<ls_mapped_mmsg>).
      APPEND INITIAL LINE TO cs_mapped-mmsg ASSIGNING FIELD-SYMBOL(<ls_mapped_target>).
      <ls_mapped_target>-MessageUUID = <ls_mapped_mmsg>-MessageUUID.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_mmsg_service~delete_mmsg.
    DATA ls_reported  TYPE zpru_if_agent_frw=>ts_mmsg_bndl_reported.
    DATA ls_failed    TYPE zpru_if_agent_frw=>ts_mmsg_bndl_failed.
    DATA lt_delete_in TYPE TABLE FOR DELETE zr_pru_mem_msg\\ZrPruMemMsg.

    precheck_delete_mmsg( EXPORTING it_mmsg_delete_imp = it_mmsg_delete_imp
                          IMPORTING et_entities        = DATA(lt_entities)
                          CHANGING  cs_reported        = ls_reported
                                    cs_failed          = ls_failed ).

    cs_failed = CORRESPONDING #( DEEP ls_failed ).
    cs_reported = CORRESPONDING #( DEEP ls_reported ).

    IF    lt_entities    <> it_mmsg_delete_imp
       OR ls_failed-mmsg IS NOT INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_delete>).
      APPEND INITIAL LINE TO lt_delete_in ASSIGNING FIELD-SYMBOL(<ls_delete_in>).
      <ls_delete_in>-MessageUUID = <ls_delete>-MessageUUID.
    ENDLOOP.

    MODIFY ENTITIES OF zr_pru_mem_msg
           ENTITY ZrPruMemMsg
           DELETE FROM lt_delete_in
           FAILED DATA(ls_del_failed)
           REPORTED DATA(ls_del_reported).

    LOOP AT ls_del_failed-zrprumemmsg ASSIGNING FIELD-SYMBOL(<ls_failed_mmsg>).
      APPEND INITIAL LINE TO cs_failed-mmsg ASSIGNING FIELD-SYMBOL(<ls_failed_target>).
      <ls_failed_target>-messageuuid = <ls_failed_mmsg>-MessageUUID.
      <ls_failed_target>-fail        = CONV #( <ls_failed_mmsg>-%fail-cause ).
      <ls_failed_target>-delete      = <ls_failed_mmsg>-%delete.
    ENDLOOP.

    LOOP AT ls_del_reported-zrprumemmsg ASSIGNING FIELD-SYMBOL(<ls_reported_mmsg>).
      APPEND INITIAL LINE TO cs_reported-mmsg ASSIGNING FIELD-SYMBOL(<ls_reported_target>).
      <ls_reported_target>-messageuuid = <ls_reported_mmsg>-MessageUUID.
      <ls_reported_target>-delete      = <ls_reported_mmsg>-%delete.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_mmsg_service~determine.
  ENDMETHOD.

  METHOD zpru_if_mmsg_service~do_save.
  ENDMETHOD.

  METHOD zpru_if_mmsg_service~query_mmsg.
    CLEAR et_mmsg_k.

    SELECT messageuuid FROM zpru_mem_msg
      WHERE messageuuid IN @it_message_uuid
        AND agentuuid   IN @it_agent_uuid
        AND runuuid     IN @it_run_uuid
        AND queryuuid   IN @it_query_uuid
        AND stepuuid    IN @it_step_uuid
        AND messagetype IN @it_message_type
        AND createdby   IN @it_created_by
        AND createdat   IN @it_created_at
        AND changedby   IN @it_changed_by
        AND changedat   IN @it_changed_at
      INTO TABLE @et_mmsg_k.
  ENDMETHOD.

  METHOD zpru_if_mmsg_service~read_mmsg.
    DATA ls_reported TYPE zpru_if_agent_frw=>ts_mmsg_bndl_reported.
    DATA ls_failed   TYPE zpru_if_agent_frw=>ts_mmsg_bndl_failed.
    DATA lt_read_in  TYPE TABLE FOR READ IMPORT zr_pru_mem_msg\\ZrPruMemMsg.

    CLEAR et_mmsg.

    IF it_mmsg_read_k IS INITIAL.
      RETURN.
    ENDIF.

    precheck_read_mmsg( EXPORTING it_mmsg_read_k = it_mmsg_read_k
                        IMPORTING et_entities    = DATA(lt_entities)
                        CHANGING  cs_reported    = ls_reported
                                  cs_failed      = ls_failed ).

    cs_failed = CORRESPONDING #( DEEP ls_failed ).
    cs_reported = CORRESPONDING #( DEEP ls_reported ).

    IF    lt_entities    <> it_mmsg_read_k
       OR ls_failed-mmsg IS NOT INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_req>).
      APPEND INITIAL LINE TO lt_read_in ASSIGNING FIELD-SYMBOL(<ls_read_in>).
      <ls_read_in>-MessageUUID = <ls_req>-MessageUUID.

      <ls_read_in>-%control-Content     = COND #( WHEN <ls_req>-Control-content = abap_true THEN if_abap_behv=>mk-on ).
      <ls_read_in>-%control-MessageType = COND #( WHEN <ls_req>-Control-messagetype = abap_true
                                                  THEN if_abap_behv=>mk-on ).
      <ls_read_in>-%control-MessageCid  = COND #( WHEN <ls_req>-Control-messagecid = abap_true THEN if_abap_behv=>mk-on ).
      <ls_read_in>-%control-Stage       = COND #( WHEN <ls_req>-Control-stage = abap_true THEN if_abap_behv=>mk-on ).
      <ls_read_in>-%control-SubStage    = COND #( WHEN <ls_req>-Control-substage = abap_true THEN if_abap_behv=>mk-on ).
      <ls_read_in>-%control-Namespace   = COND #( WHEN <ls_req>-Control-namespace = abap_true THEN if_abap_behv=>mk-on ).
      <ls_read_in>-%control-UserName    = COND #( WHEN <ls_req>-Control-username = abap_true THEN if_abap_behv=>mk-on ).
      <ls_read_in>-%control-AgentUUID   = COND #( WHEN <ls_req>-Control-agentuuid = abap_true THEN if_abap_behv=>mk-on ).
      <ls_read_in>-%control-RunUUID     = COND #( WHEN <ls_req>-Control-runuuid = abap_true THEN if_abap_behv=>mk-on ).
      <ls_read_in>-%control-QueryUUID   = COND #( WHEN <ls_req>-Control-queryuuid = abap_true THEN if_abap_behv=>mk-on ).
      <ls_read_in>-%control-StepUUID    = COND #( WHEN <ls_req>-Control-stepuuid = abap_true THEN if_abap_behv=>mk-on ).
      <ls_read_in>-%control-MessageTime = COND #( WHEN <ls_req>-Control-messagetime = abap_true
                                                  THEN if_abap_behv=>mk-on ).
      <ls_read_in>-%control-CreatedBy   = COND #( WHEN <ls_req>-Control-createdby = abap_true THEN if_abap_behv=>mk-on ).
      <ls_read_in>-%control-CreatedAt   = COND #( WHEN <ls_req>-Control-createdat = abap_true THEN if_abap_behv=>mk-on ).
      <ls_read_in>-%control-ChangedBy   = COND #( WHEN <ls_req>-Control-changedby = abap_true THEN if_abap_behv=>mk-on ).
      <ls_read_in>-%control-ChangedAt   = COND #( WHEN <ls_req>-Control-changedat = abap_true THEN if_abap_behv=>mk-on ).
    ENDLOOP.

    READ ENTITIES OF zr_pru_mem_msg
         ENTITY ZrPruMemMsg
         FROM lt_read_in
         RESULT DATA(lt_result)
         FAILED DATA(ls_rd_failed)
         REPORTED DATA(ls_rd_reported).

    LOOP AT ls_rd_failed-zrprumemmsg ASSIGNING FIELD-SYMBOL(<ls_failed_rd>).
      APPEND INITIAL LINE TO cs_failed-mmsg ASSIGNING FIELD-SYMBOL(<ls_failed_target_rd>).
      <ls_failed_target_rd>-messageuuid = <ls_failed_rd>-MessageUUID.
      <ls_failed_target_rd>-fail        = CONV #( <ls_failed_rd>-%fail-cause ).
    ENDLOOP.

    LOOP AT ls_rd_reported-zrprumemmsg ASSIGNING FIELD-SYMBOL(<ls_reported_rd>).
      APPEND INITIAL LINE TO cs_reported-mmsg ASSIGNING FIELD-SYMBOL(<ls_reported_target_rd>).
      <ls_reported_target_rd>-MessageUUID = <ls_reported_rd>-MessageUUID.
    ENDLOOP.

    LOOP AT lt_result ASSIGNING FIELD-SYMBOL(<ls_res>).
      APPEND INITIAL LINE TO et_mmsg ASSIGNING FIELD-SYMBOL(<ls_out_mmsg>).
      <ls_out_mmsg>-messageuuid = <ls_res>-MessageUUID.
      <ls_out_mmsg>-content     = <ls_res>-Content.
      <ls_out_mmsg>-messagetype = <ls_res>-MessageType.
      <ls_out_mmsg>-messagecid  = <ls_res>-MessageCid.
      <ls_out_mmsg>-stage       = <ls_res>-Stage.
      <ls_out_mmsg>-substage    = <ls_res>-SubStage.
      <ls_out_mmsg>-namespace   = <ls_res>-Namespace.
      <ls_out_mmsg>-username    = <ls_res>-UserName.
      <ls_out_mmsg>-agentuuid   = <ls_res>-AgentUUID.
      <ls_out_mmsg>-runuuid     = <ls_res>-RunUUID.
      <ls_out_mmsg>-queryuuid   = <ls_res>-QueryUUID.
      <ls_out_mmsg>-stepuuid    = <ls_res>-StepUUID.
      <ls_out_mmsg>-messagetime = <ls_res>-MessageTime.
      <ls_out_mmsg>-createdby   = <ls_res>-CreatedBy.
      <ls_out_mmsg>-createdat   = <ls_res>-CreatedAt.
      <ls_out_mmsg>-changedby   = <ls_res>-ChangedBy.
      <ls_out_mmsg>-changedat   = <ls_res>-ChangedAt.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_mmsg_service~update_mmsg.
    DATA ls_reported  TYPE zpru_if_agent_frw=>ts_mmsg_bndl_reported.
    DATA ls_failed    TYPE zpru_if_agent_frw=>ts_mmsg_bndl_failed.
    DATA lt_update_in TYPE TABLE FOR UPDATE zr_pru_mem_msg\\ZrPruMemMsg.

    precheck_update_mmsg( EXPORTING it_mmsg_update_imp = it_mmsg_update_imp
                          IMPORTING et_entities        = DATA(lt_entities)
                          CHANGING  cs_reported        = ls_reported
                                    cs_failed          = ls_failed ).

    cs_failed = CORRESPONDING #( DEEP ls_failed ).
    cs_reported = CORRESPONDING #( DEEP ls_reported ).

    IF    lt_entities    <> it_mmsg_update_imp
       OR ls_failed-mmsg IS NOT INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_update>).
      APPEND INITIAL LINE TO lt_update_in ASSIGNING FIELD-SYMBOL(<ls_update_in>).
      <ls_update_in>-MessageUUID = <ls_update>-messageuuid.

      <ls_update_in>-Content     = COND #( WHEN <ls_update>-control-content = abap_true THEN <ls_update>-content ).
      <ls_update_in>-%control-Content = COND #( WHEN <ls_update>-control-content = abap_true THEN if_abap_behv=>mk-on ).
      <ls_update_in>-MessageType = COND #( WHEN <ls_update>-control-messagetype = abap_true
                                           THEN <ls_update>-messagetype ).
      <ls_update_in>-%control-MessageType = COND #( WHEN <ls_update>-control-messagetype = abap_true
                                                    THEN if_abap_behv=>mk-on ).
      <ls_update_in>-MessageCid = COND #( WHEN <ls_update>-control-messagecid = abap_true THEN <ls_update>-messagecid ).
      <ls_update_in>-%control-MessageCid = COND #( WHEN <ls_update>-control-messagecid = abap_true
                                                   THEN if_abap_behv=>mk-on ).
      <ls_update_in>-Stage = COND #( WHEN <ls_update>-control-stage = abap_true THEN <ls_update>-stage ).
      <ls_update_in>-%control-Stage = COND #( WHEN <ls_update>-control-stage = abap_true THEN if_abap_behv=>mk-on ).
      <ls_update_in>-SubStage = COND #( WHEN <ls_update>-control-substage = abap_true THEN <ls_update>-substage ).
      <ls_update_in>-%control-SubStage = COND #( WHEN <ls_update>-control-substage = abap_true THEN if_abap_behv=>mk-on ).
      <ls_update_in>-Namespace = COND #( WHEN <ls_update>-control-namespace = abap_true THEN <ls_update>-namespace ).
      <ls_update_in>-%control-Namespace = COND #( WHEN <ls_update>-control-namespace = abap_true
                                                  THEN if_abap_behv=>mk-on ).
      <ls_update_in>-UserName = COND #( WHEN <ls_update>-control-username = abap_true THEN <ls_update>-username ).
      <ls_update_in>-%control-UserName = COND #( WHEN <ls_update>-control-username = abap_true THEN if_abap_behv=>mk-on ).
      <ls_update_in>-AgentUUID = COND #( WHEN <ls_update>-control-agentuuid = abap_true THEN <ls_update>-agentuuid ).
      <ls_update_in>-%control-AgentUUID = COND #( WHEN <ls_update>-control-agentuuid = abap_true
                                                  THEN if_abap_behv=>mk-on ).
      <ls_update_in>-RunUUID = COND #( WHEN <ls_update>-control-runuuid = abap_true THEN <ls_update>-runuuid ).
      <ls_update_in>-%control-RunUUID = COND #( WHEN <ls_update>-control-runuuid = abap_true THEN if_abap_behv=>mk-on ).
      <ls_update_in>-QueryUUID = COND #( WHEN <ls_update>-control-queryuuid = abap_true THEN <ls_update>-queryuuid ).
      <ls_update_in>-%control-QueryUUID = COND #( WHEN <ls_update>-control-queryuuid = abap_true
                                                  THEN if_abap_behv=>mk-on ).
      <ls_update_in>-StepUUID = COND #( WHEN <ls_update>-control-stepuuid = abap_true THEN <ls_update>-stepuuid ).
      <ls_update_in>-%control-StepUUID = COND #( WHEN <ls_update>-control-stepuuid = abap_true THEN if_abap_behv=>mk-on ).
      <ls_update_in>-MessageTime = COND #( WHEN <ls_update>-control-messagetime = abap_true
                                           THEN <ls_update>-messagetime ).
      <ls_update_in>-%control-MessageTime = COND #( WHEN <ls_update>-control-messagetime = abap_true
                                                    THEN if_abap_behv=>mk-on ).
      <ls_update_in>-CreatedBy = COND #( WHEN <ls_update>-control-createdby = abap_true THEN <ls_update>-createdby ).
      <ls_update_in>-%control-CreatedBy = COND #( WHEN <ls_update>-control-createdby = abap_true
                                                  THEN if_abap_behv=>mk-on ).
      <ls_update_in>-CreatedAt = COND #( WHEN <ls_update>-control-createdat = abap_true THEN <ls_update>-createdat ).
      <ls_update_in>-%control-CreatedAt = COND #( WHEN <ls_update>-control-createdat = abap_true
                                                  THEN if_abap_behv=>mk-on ).
      <ls_update_in>-ChangedBy = COND #( WHEN <ls_update>-control-changedby = abap_true THEN <ls_update>-changedby ).
      <ls_update_in>-%control-ChangedBy = COND #( WHEN <ls_update>-control-changedby = abap_true
                                                  THEN if_abap_behv=>mk-on ).
      <ls_update_in>-ChangedAt = COND #( WHEN <ls_update>-control-changedat = abap_true THEN <ls_update>-changedat ).
      <ls_update_in>-%control-ChangedAt = COND #( WHEN <ls_update>-control-changedat = abap_true
                                                  THEN if_abap_behv=>mk-on ).
    ENDLOOP.

    MODIFY ENTITIES OF zr_pru_mem_msg
           ENTITY ZrPruMemMsg
           UPDATE FROM lt_update_in
           FAILED DATA(ls_up_failed)
           REPORTED DATA(ls_up_reported).

    LOOP AT ls_up_failed-zrprumemmsg ASSIGNING FIELD-SYMBOL(<ls_failed_up>).
      APPEND INITIAL LINE TO cs_failed-mmsg ASSIGNING FIELD-SYMBOL(<ls_failed_target_up>).
      <ls_failed_target_up>-messageuuid = <ls_failed_up>-MessageUUID.
      <ls_failed_target_up>-fail        = CONV #( <ls_failed_up>-%fail-cause ).
      <ls_failed_target_up>-update      = <ls_failed_up>-%update.
    ENDLOOP.

    LOOP AT ls_up_reported-zrprumemmsg ASSIGNING FIELD-SYMBOL(<ls_reported_up>).
      APPEND INITIAL LINE TO cs_reported-mmsg ASSIGNING FIELD-SYMBOL(<ls_reported_target_up>).
      <ls_reported_target_up>-messageuuid = <ls_reported_up>-MessageUUID.
      <ls_reported_target_up>-update      = <ls_reported_up>-%update.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_mmsg_service~validate.
  ENDMETHOD.

  METHOD precheck_create_mmsg.
    DATA lo_pre TYPE REF TO zpru_if_mmsg_precheck.

    TRY.
        lo_pre ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_MMSG_PRECHECK`
                                                           iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RETURN.
    ENDTRY.

    lo_pre->precheck_create_mmsg( EXPORTING it_mmsg_create_imp = it_mmsg_create_imp
                                  IMPORTING et_entities        = et_entities
                                  CHANGING  cs_reported        = cs_reported
                                            cs_failed          = cs_failed ).
  ENDMETHOD.

  METHOD precheck_delete_mmsg.
    DATA lo_pre TYPE REF TO zpru_if_mmsg_precheck.

    TRY.
        lo_pre ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_MMSG_PRECHECK`
                                                           iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RETURN.
    ENDTRY.

    lo_pre->precheck_delete_mmsg( EXPORTING it_mmsg_delete_imp = it_mmsg_delete_imp
                                  IMPORTING et_entities        = et_entities
                                  CHANGING  cs_reported        = cs_reported
                                            cs_failed          = cs_failed ).
  ENDMETHOD.

  METHOD precheck_read_mmsg.
    DATA lo_pre TYPE REF TO zpru_if_mmsg_precheck.

    TRY.
        lo_pre ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_MMSG_PRECHECK`
                                                           iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RETURN.
    ENDTRY.

    lo_pre->precheck_read_mmsg( EXPORTING it_mmsg_read_k = it_mmsg_read_k
                                IMPORTING et_entities    = et_entities
                                CHANGING  cs_reported    = cs_reported
                                          cs_failed      = cs_failed ).
  ENDMETHOD.

  METHOD precheck_update_mmsg.
    DATA lo_pre TYPE REF TO zpru_if_mmsg_precheck.

    TRY.
        lo_pre ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_MMSG_PRECHECK`
                                                           iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RETURN.
    ENDTRY.

    lo_pre->precheck_update_mmsg( EXPORTING it_mmsg_update_imp = it_mmsg_update_imp
                                  IMPORTING et_entities        = et_entities
                                  CHANGING  cs_reported        = cs_reported
                                            cs_failed          = cs_failed ).
  ENDMETHOD.
ENDCLASS.
