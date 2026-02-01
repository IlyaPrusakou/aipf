CLASS zpru_cl_agsrv_service DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zpru_if_agent_frw.
    INTERFACES zpru_if_agsrv_service.

  PROTECTED SECTION.
    METHODS precheck_create_agent_service
      IMPORTING it_agsrv_create_imp TYPE zpru_if_agsrv_crud=>tt_agsrv_create_imp
      EXPORTING et_entities         TYPE zpru_if_agsrv_crud=>tt_agsrv_create_imp
      CHANGING  cs_reported         TYPE zpru_if_agent_frw=>ts_agsrv_bndl_reported OPTIONAL
                cs_failed           TYPE zpru_if_agent_frw=>ts_agsrv_bndl_failed   OPTIONAL.

    METHODS precheck_update_agent_service
      IMPORTING it_agsrv_update_imp TYPE zpru_if_agsrv_crud=>tt_agsrv_update_imp
      EXPORTING et_entities         TYPE zpru_if_agsrv_crud=>tt_agsrv_update_imp
      CHANGING  cs_reported         TYPE zpru_if_agent_frw=>ts_agsrv_bndl_reported OPTIONAL
                cs_failed           TYPE zpru_if_agent_frw=>ts_agsrv_bndl_failed   OPTIONAL.

    METHODS precheck_delete_agent_service
      IMPORTING it_agsrv_delete_imp TYPE zpru_if_agsrv_crud=>tt_agsrv_delete_imp
      EXPORTING et_entities         TYPE zpru_if_agsrv_crud=>tt_agsrv_delete_imp
      CHANGING  cs_reported         TYPE zpru_if_agent_frw=>ts_agsrv_bndl_reported OPTIONAL
                cs_failed           TYPE zpru_if_agent_frw=>ts_agsrv_bndl_failed   OPTIONAL.

    METHODS precheck_read_agent_service
      IMPORTING it_agsrv_read_k TYPE zpru_if_agsrv_crud=>tt_agsrv_read_k
      EXPORTING et_entities     TYPE zpru_if_agsrv_crud=>tt_agsrv_read_k
      CHANGING  cs_reported     TYPE zpru_if_agent_frw=>ts_agsrv_bndl_reported OPTIONAL
                cs_failed       TYPE zpru_if_agent_frw=>ts_agsrv_bndl_failed   OPTIONAL.
ENDCLASS.


CLASS zpru_cl_agsrv_service IMPLEMENTATION.
  METHOD zpru_if_agsrv_service~query_agent_service.
    CLEAR et_agsrv_k.

    SELECT service, context FROM zpru_agent_serv
      WHERE service            IN @it_service
        AND context            IN @it_context
        AND class              IN @it_class
        AND created_by         IN @it_created_by
        AND created_at         IN @it_created_at
        AND changed_by         IN @it_changed_by
        AND last_changed       IN @it_last_changed
        AND local_last_changed IN @it_local_last_changed
      INTO TABLE @et_agsrv_k.
  ENDMETHOD.

  METHOD zpru_if_agsrv_service~read_agent_service.
    DATA ls_reported TYPE zpru_if_agent_frw=>ts_agsrv_bndl_reported.
    DATA ls_failed   TYPE zpru_if_agent_frw=>ts_agsrv_bndl_failed.
    DATA ls_out      TYPE zpru_agent_serv.
    DATA lt_read_in  TYPE TABLE FOR READ IMPORT zr_pru_agent_serv\\zrpruagentserv.

    CLEAR et_agsrv.

    IF it_agsrv_read_k IS INITIAL.
      RETURN.
    ENDIF.

    precheck_read_agent_service( EXPORTING it_agsrv_read_k = it_agsrv_read_k
                                 IMPORTING et_entities     = DATA(lt_entities)
                                 CHANGING  cs_reported     = ls_reported
                                           cs_failed       = ls_failed ).

    cs_failed = CORRESPONDING #( DEEP ls_failed ).
    cs_reported = CORRESPONDING #( DEEP ls_reported ).

    IF    lt_entities     <> it_agsrv_read_k
       OR ls_failed-agsrv IS NOT INITIAL.
      RETURN.
    ENDIF.

    lt_read_in = VALUE #(
        FOR <ls_req> IN lt_entities
        ( service  = <ls_req>-service
          context  = <ls_req>-context
          %control = VALUE #(
              Class            = COND #( WHEN <ls_req>-control-class = abap_true THEN if_abap_behv=>mk-on )
              CreatedBy        = COND #( WHEN <ls_req>-control-created_by = abap_true THEN if_abap_behv=>mk-on )
              CreatedAt        = COND #( WHEN <ls_req>-control-created_at = abap_true THEN if_abap_behv=>mk-on )
              ChangedBy        = COND #( WHEN <ls_req>-control-changed_by = abap_true THEN if_abap_behv=>mk-on )
              LastChanged      = COND #( WHEN <ls_req>-control-last_changed = abap_true THEN if_abap_behv=>mk-on )
              LocalLastChanged = COND #( WHEN <ls_req>-control-local_last_changed = abap_true THEN if_abap_behv=>mk-on ) ) ) ).

    READ ENTITIES OF zr_pru_agent_serv
         ENTITY zrpruagentserv
         FROM lt_read_in
         RESULT DATA(lt_result)
         FAILED DATA(ls_rd_failed)
         REPORTED DATA(ls_rd_reported).

    LOOP AT ls_rd_failed-zrpruagentserv ASSIGNING FIELD-SYMBOL(<ls_failed_res>).
      APPEND INITIAL LINE TO cs_failed-agsrv ASSIGNING FIELD-SYMBOL(<ls_failed_target>).
      <ls_failed_target>-service = <ls_failed_res>-service.
      <ls_failed_target>-context = <ls_failed_res>-context.
      <ls_failed_target>-fail    = CONV #( <ls_failed_res>-%fail-cause ).
    ENDLOOP.

    LOOP AT ls_rd_reported-zrpruagentserv ASSIGNING FIELD-SYMBOL(<ls_reported_res>).
      APPEND INITIAL LINE TO cs_reported-agsrv ASSIGNING FIELD-SYMBOL(<ls_reported_target>).
      <ls_reported_target>-service = <ls_reported_res>-service.
      <ls_reported_target>-context = <ls_reported_res>-context.
    ENDLOOP.

    LOOP AT lt_result ASSIGNING FIELD-SYMBOL(<ls_res>).
      CLEAR ls_out.
      ls_out-service            = <ls_res>-Service.
      ls_out-context            = <ls_res>-Context.
      ls_out-class              = <ls_res>-Class.
      ls_out-created_by         = <ls_res>-CreatedBy.
      ls_out-created_at         = <ls_res>-CreatedAt.
      ls_out-changed_by         = <ls_res>-ChangedBy.
      ls_out-last_changed       = <ls_res>-LastChanged.
      ls_out-local_last_changed = <ls_res>-LocalLastChanged.
      APPEND ls_out TO et_agsrv.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_agsrv_service~create_agent_service.
    DATA ls_reported  TYPE zpru_if_agent_frw=>ts_agsrv_bndl_reported.
    DATA ls_failed    TYPE zpru_if_agent_frw=>ts_agsrv_bndl_failed.
    DATA lt_create_in TYPE TABLE FOR CREATE zr_pru_agent_serv\\zrpruagentserv.

    precheck_create_agent_service( EXPORTING it_agsrv_create_imp = it_agsrv_create_imp
                                   IMPORTING et_entities         = DATA(lt_entities)
                                   CHANGING  cs_reported         = ls_reported
                                             cs_failed           = ls_failed ).

    cs_failed = CORRESPONDING #( DEEP ls_failed ).
    cs_reported = CORRESPONDING #( DEEP ls_reported ).

    IF    lt_entities     <> it_agsrv_create_imp
       OR ls_failed-agsrv IS NOT INITIAL.
      RETURN.
    ENDIF.

    lt_create_in = VALUE #(
        FOR <ls_create> IN lt_entities
        ( Service          = <ls_create>-service
          Context          = <ls_create>-context
          Class            = <ls_create>-class
          CreatedBy        = <ls_create>-created_by
          CreatedAt        = <ls_create>-created_at
          ChangedBy        = <ls_create>-changed_by
          LastChanged      = <ls_create>-last_changed
          LocalLastChanged = <ls_create>-local_last_changed
          %control         = VALUE #(
              Service          = if_abap_behv=>mk-on
              Context          = if_abap_behv=>mk-on
              Class            = COND #( WHEN <ls_create>-control-class = abap_true THEN if_abap_behv=>mk-on )
              CreatedBy        = COND #( WHEN <ls_create>-control-created_by = abap_true THEN if_abap_behv=>mk-on )
              CreatedAt        = COND #( WHEN <ls_create>-control-created_at = abap_true THEN if_abap_behv=>mk-on )
              ChangedBy        = COND #( WHEN <ls_create>-control-changed_by = abap_true THEN if_abap_behv=>mk-on )
              LastChanged      = COND #( WHEN <ls_create>-control-last_changed = abap_true THEN if_abap_behv=>mk-on )
              LocalLastChanged = COND #( WHEN <ls_create>-control-local_last_changed = abap_true
                                         THEN if_abap_behv=>mk-on ) ) ) ).

    MODIFY ENTITIES OF zr_pru_agent_serv
           ENTITY zrpruagentserv
           CREATE AUTO FILL CID WITH lt_create_in
           MAPPED DATA(ls_mapped_res)
           REPORTED DATA(ls_reported_res)
           FAILED DATA(ls_failed_res).

    LOOP AT ls_failed_res-zrpruagentserv ASSIGNING FIELD-SYMBOL(<ls_failed_item>).
      APPEND INITIAL LINE TO cs_failed-agsrv ASSIGNING FIELD-SYMBOL(<ls_failed_out>).
      <ls_failed_out>-service = <ls_failed_item>-service.
      <ls_failed_out>-context = <ls_failed_item>-context.
      <ls_failed_out>-fail    = CONV #( <ls_failed_item>-%fail-cause ).
      <ls_failed_out>-create  = <ls_failed_item>-%create.
    ENDLOOP.

    LOOP AT ls_reported_res-zrpruagentserv ASSIGNING FIELD-SYMBOL(<ls_reported_item>).
      APPEND INITIAL LINE TO cs_reported-agsrv ASSIGNING FIELD-SYMBOL(<ls_reported_out>).
      <ls_reported_out>-service = <ls_reported_item>-service.
      <ls_reported_out>-context = <ls_reported_item>-context.
      <ls_reported_out>-create  = <ls_reported_item>-%create.
    ENDLOOP.

    LOOP AT ls_mapped_res-zrpruagentserv ASSIGNING FIELD-SYMBOL(<ls_mapped_item>).
      APPEND INITIAL LINE TO cs_mapped-agsrv ASSIGNING FIELD-SYMBOL(<ls_mapped_out>).
      <ls_mapped_out>-service = <ls_mapped_item>-service.
      <ls_mapped_out>-context = <ls_mapped_item>-context.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_agsrv_service~update_agent_service.
    DATA ls_reported  TYPE zpru_if_agent_frw=>ts_agsrv_bndl_reported.
    DATA ls_failed    TYPE zpru_if_agent_frw=>ts_agsrv_bndl_failed.
    DATA lt_update_in TYPE TABLE FOR UPDATE zr_pru_agent_serv\\zrpruagentserv.

    precheck_update_agent_service( EXPORTING it_agsrv_update_imp = it_agsrv_update_imp
                                   IMPORTING et_entities         = DATA(lt_entities)
                                   CHANGING  cs_reported         = ls_reported
                                             cs_failed           = ls_failed ).

    cs_failed = CORRESPONDING #( DEEP ls_failed ).
    cs_reported = CORRESPONDING #( DEEP ls_reported ).

    IF    lt_entities     <> it_agsrv_update_imp
       OR ls_failed-agsrv IS NOT INITIAL.
      RETURN.
    ENDIF.

    lt_update_in = VALUE #(
        FOR <ls_update> IN lt_entities
        ( Service          = <ls_update>-service
          Context          = <ls_update>-context
          Class            = <ls_update>-class
          CreatedBy        = <ls_update>-created_by
          CreatedAt        = <ls_update>-created_at
          ChangedBy        = <ls_update>-changed_by
          LastChanged      = <ls_update>-last_changed
          LocalLastChanged = <ls_update>-local_last_changed
          %control         = VALUE #(
              Class            = COND #( WHEN <ls_update>-control-class = abap_true THEN if_abap_behv=>mk-on )
              CreatedBy        = COND #( WHEN <ls_update>-control-created_by = abap_true THEN if_abap_behv=>mk-on )
              CreatedAt        = COND #( WHEN <ls_update>-control-created_at = abap_true THEN if_abap_behv=>mk-on )
              ChangedBy        = COND #( WHEN <ls_update>-control-changed_by = abap_true THEN if_abap_behv=>mk-on )
              LastChanged      = COND #( WHEN <ls_update>-control-last_changed = abap_true THEN if_abap_behv=>mk-on )
              LocalLastChanged = COND #( WHEN <ls_update>-control-local_last_changed = abap_true
                                         THEN if_abap_behv=>mk-on ) ) ) ).

    MODIFY ENTITIES OF zr_pru_agent_serv
           ENTITY zrpruagentserv
           UPDATE FROM lt_update_in
           REPORTED DATA(ls_reported_res)
           FAILED DATA(ls_failed_res).

    LOOP AT ls_failed_res-zrpruagentserv ASSIGNING FIELD-SYMBOL(<ls_failed_item>).
      APPEND INITIAL LINE TO cs_failed-agsrv ASSIGNING FIELD-SYMBOL(<ls_failed_out>).
      <ls_failed_out>-service = <ls_failed_item>-service.
      <ls_failed_out>-context = <ls_failed_item>-context.
      <ls_failed_out>-fail    = CONV #( <ls_failed_item>-%fail-cause ).
      <ls_failed_out>-update  = <ls_failed_item>-%update.
    ENDLOOP.

    LOOP AT ls_reported_res-zrpruagentserv ASSIGNING FIELD-SYMBOL(<ls_reported_item>).
      APPEND INITIAL LINE TO cs_reported-agsrv ASSIGNING FIELD-SYMBOL(<ls_reported_out>).
      <ls_reported_out>-service = <ls_reported_item>-service.
      <ls_reported_out>-context = <ls_reported_item>-context.
      <ls_reported_out>-update  = <ls_reported_item>-%update.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_agsrv_service~delete_agent_service.
    DATA ls_reported  TYPE zpru_if_agent_frw=>ts_agsrv_bndl_reported.
    DATA ls_failed    TYPE zpru_if_agent_frw=>ts_agsrv_bndl_failed.
    DATA lt_delete_in TYPE TABLE FOR DELETE zr_pru_agent_serv\\zrpruagentserv.

    precheck_delete_agent_service( EXPORTING it_agsrv_delete_imp = it_agsrv_delete_imp
                                   IMPORTING et_entities         = DATA(lt_entities)
                                   CHANGING  cs_reported         = ls_reported
                                             cs_failed           = ls_failed ).

    cs_failed = CORRESPONDING #( DEEP ls_failed ).
    cs_reported = CORRESPONDING #( DEEP ls_reported ).

    IF    lt_entities     <> it_agsrv_delete_imp
       OR ls_failed-agsrv IS NOT INITIAL.
      RETURN.
    ENDIF.

    lt_delete_in = VALUE #( FOR <ls_delete> IN lt_entities
                            ( service = <ls_delete>-service
                              context = <ls_delete>-context ) ).

    MODIFY ENTITIES OF zr_pru_agent_serv
           ENTITY zrpruagentserv
           DELETE FROM lt_delete_in
           REPORTED DATA(ls_reported_res)
           FAILED DATA(ls_failed_res).

    LOOP AT ls_failed_res-zrpruagentserv ASSIGNING FIELD-SYMBOL(<ls_failed_item>).
      APPEND INITIAL LINE TO cs_failed-agsrv ASSIGNING FIELD-SYMBOL(<ls_failed_out>).
      <ls_failed_out>-service = <ls_failed_item>-service.
      <ls_failed_out>-context = <ls_failed_item>-context.
      <ls_failed_out>-fail    = CONV #( <ls_failed_item>-%fail-cause ).
      <ls_failed_out>-delete  = <ls_failed_item>-%delete.
    ENDLOOP.

    LOOP AT ls_reported_res-zrpruagentserv ASSIGNING FIELD-SYMBOL(<ls_reported_item>).
      APPEND INITIAL LINE TO cs_reported-agsrv ASSIGNING FIELD-SYMBOL(<ls_reported_out>).
      <ls_reported_out>-service = <ls_reported_item>-service.
      <ls_reported_out>-context = <ls_reported_item>-context.
      <ls_reported_out>-delete  = <ls_reported_item>-%delete.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_agsrv_service~determine.
  ENDMETHOD.

  METHOD zpru_if_agsrv_service~validate.
  ENDMETHOD.

  METHOD zpru_if_agsrv_service~clean_up.
  ENDMETHOD.

  METHOD zpru_if_agsrv_service~do_save.
  ENDMETHOD.

  METHOD precheck_create_agent_service.
    DATA lo_pre TYPE REF TO zpru_if_agsrv_precheck.

    TRY.
        lo_pre ?= zpru_cl_agent_service_mngr=>get_service(
                      iv_service = `ZPRU_IF_AGSRV_PRECHECK`
                      iv_context = zpru_if_agent_frw=>cs_context-st_agent_definition ).
      CATCH zpru_cx_agent_core.
        RETURN.
    ENDTRY.

    lo_pre->precheck_create_agent_service( EXPORTING it_agsrv_create_imp = it_agsrv_create_imp
                                           IMPORTING et_entities         = et_entities
                                           CHANGING  cs_reported         = cs_reported
                                                     cs_failed           = cs_failed ).
  ENDMETHOD.

  METHOD precheck_update_agent_service.
    DATA lo_pre TYPE REF TO zpru_if_agsrv_precheck.

    TRY.
        lo_pre ?= zpru_cl_agent_service_mngr=>get_service(
                      iv_service = `ZPRU_IF_AGSRV_PRECHECK`
                      iv_context = zpru_if_agent_frw=>cs_context-st_agent_definition ).
      CATCH zpru_cx_agent_core.
        RETURN.
    ENDTRY.

    lo_pre->precheck_update_agent_service( EXPORTING it_agsrv_update_imp = it_agsrv_update_imp
                                           IMPORTING et_entities         = et_entities
                                           CHANGING  cs_reported         = cs_reported
                                                     cs_failed           = cs_failed ).
  ENDMETHOD.

  METHOD precheck_delete_agent_service.
    DATA lo_pre TYPE REF TO zpru_if_agsrv_precheck.

    TRY.
        lo_pre ?= zpru_cl_agent_service_mngr=>get_service(
                      iv_service = `ZPRU_IF_AGSRV_PRECHECK`
                      iv_context = zpru_if_agent_frw=>cs_context-st_agent_definition ).
      CATCH zpru_cx_agent_core.
        RETURN.
    ENDTRY.

    lo_pre->precheck_delete_agent_service( EXPORTING it_agsrv_delete_imp = it_agsrv_delete_imp
                                           IMPORTING et_entities         = et_entities
                                           CHANGING  cs_reported         = cs_reported
                                                     cs_failed           = cs_failed ).
  ENDMETHOD.

  METHOD precheck_read_agent_service.
    DATA lo_pre TYPE REF TO zpru_if_agsrv_precheck.

    TRY.
        lo_pre ?= zpru_cl_agent_service_mngr=>get_service(
                      iv_service = `ZPRU_IF_AGSRV_PRECHECK`
                      iv_context = zpru_if_agent_frw=>cs_context-st_agent_definition ).
      CATCH zpru_cx_agent_core.
        RETURN.
    ENDTRY.

    lo_pre->precheck_read_agent_service( EXPORTING it_agsrv_read_k = it_agsrv_read_k
                                         IMPORTING et_entities     = et_entities
                                         CHANGING  cs_reported     = cs_reported
                                                   cs_failed       = cs_failed ).
  ENDMETHOD.
ENDCLASS.
