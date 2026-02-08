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

    SELECT aipf7Service as Service, aipf7Context as Context FROM zi_pru_agent_serv
      WHERE aipf7Service          IN @it_service
        AND aipf7Context          IN @it_context
        AND aipf7Class            IN @it_class
        AND aipf7CreatedBy        IN @it_created_by
        AND aipf7CreatedAt        IN @it_created_at
        AND aipf7ChangedBy        IN @it_changed_by
        AND aipf7LastChanged      IN @it_last_changed
        AND aipf7LocalLastChanged IN @it_local_last_changed
      INTO TABLE @et_agsrv_k.
  ENDMETHOD.

  METHOD zpru_if_agsrv_service~read_agent_service.
    DATA ls_reported TYPE zpru_if_agent_frw=>ts_agsrv_bndl_reported.
    DATA ls_failed   TYPE zpru_if_agent_frw=>ts_agsrv_bndl_failed.
    DATA ls_out      TYPE zpru_s_agent_serv.
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
        ( aipf7service  = <ls_req>-service
          aipf7context  = <ls_req>-context
          %control = VALUE #(
              aipf7Class            = COND #( WHEN <ls_req>-control-Class = abap_true THEN if_abap_behv=>mk-on )
              aipf7CreatedBy        = COND #( WHEN <ls_req>-control-CreatedBy = abap_true THEN if_abap_behv=>mk-on )
              aipf7CreatedAt        = COND #( WHEN <ls_req>-control-CreatedAt = abap_true THEN if_abap_behv=>mk-on )
              aipf7ChangedBy        = COND #( WHEN <ls_req>-control-ChangedBy = abap_true THEN if_abap_behv=>mk-on )
              aipf7LastChanged      = COND #( WHEN <ls_req>-control-LastChanged = abap_true THEN if_abap_behv=>mk-on )
              aipf7LocalLastChanged = COND #( WHEN <ls_req>-control-LocalLastChanged = abap_true THEN if_abap_behv=>mk-on ) ) ) ).

    READ ENTITIES OF zr_pru_agent_serv
         ENTITY zrpruagentserv
         FROM lt_read_in
         RESULT DATA(lt_result)
         FAILED DATA(ls_rd_failed)
         REPORTED DATA(ls_rd_reported).

    LOOP AT ls_rd_failed-zrpruagentserv ASSIGNING FIELD-SYMBOL(<ls_failed_res>).
      APPEND INITIAL LINE TO cs_failed-agsrv ASSIGNING FIELD-SYMBOL(<ls_failed_target>).
      <ls_failed_target>-service = <ls_failed_res>-aipf7service.
      <ls_failed_target>-context = <ls_failed_res>-aipf7context.
      <ls_failed_target>-fail    = CONV #( <ls_failed_res>-%fail-cause ).
    ENDLOOP.

    LOOP AT ls_rd_reported-zrpruagentserv ASSIGNING FIELD-SYMBOL(<ls_reported_res>).
      APPEND INITIAL LINE TO cs_reported-agsrv ASSIGNING FIELD-SYMBOL(<ls_reported_target>).
      <ls_reported_target>-service = <ls_reported_res>-aipf7service.
      <ls_reported_target>-context = <ls_reported_res>-aipf7context.
    ENDLOOP.

    LOOP AT lt_result ASSIGNING FIELD-SYMBOL(<ls_res>).
      CLEAR ls_out.
      ls_out-Service          = <ls_res>-aipf7Service.
      ls_out-Context          = <ls_res>-aipf7Context.
      ls_out-Class            = <ls_res>-aipf7Class.
      ls_out-CreatedBy        = <ls_res>-aipf7CreatedBy.
      ls_out-CreatedAt        = <ls_res>-aipf7CreatedAt.
      ls_out-ChangedBy        = <ls_res>-aipf7ChangedBy.
      ls_out-LastChanged      = <ls_res>-aipf7LastChanged.
      ls_out-LocalLastChanged = <ls_res>-aipf7LocalLastChanged.
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
        ( aipf7Service          = <ls_create>-Service
          aipf7Context          = <ls_create>-Context
          aipf7Class            = <ls_create>-Class
          aipf7CreatedBy        = <ls_create>-CreatedBy
          aipf7CreatedAt        = <ls_create>-CreatedAt
          aipf7ChangedBy        = <ls_create>-ChangedBy
          aipf7LastChanged      = <ls_create>-LastChanged
          aipf7LocalLastChanged = <ls_create>-LocalLastChanged
          %control         = VALUE #(
              aipf7Service          = if_abap_behv=>mk-on
              aipf7Context          = if_abap_behv=>mk-on
              aipf7Class            = COND #( WHEN <ls_create>-Control-Class = abap_true THEN if_abap_behv=>mk-on )
              aipf7CreatedBy        = COND #( WHEN <ls_create>-Control-CreatedBy = abap_true THEN if_abap_behv=>mk-on )
              aipf7CreatedAt        = COND #( WHEN <ls_create>-Control-CreatedAt = abap_true THEN if_abap_behv=>mk-on )
              aipf7ChangedBy        = COND #( WHEN <ls_create>-Control-ChangedBy = abap_true THEN if_abap_behv=>mk-on )
              aipf7LastChanged      = COND #( WHEN <ls_create>-Control-LastChanged = abap_true THEN if_abap_behv=>mk-on )
              aipf7LocalLastChanged = COND #( WHEN <ls_create>-Control-LocalLastChanged = abap_true
                                         THEN if_abap_behv=>mk-on ) ) ) ).

    MODIFY ENTITIES OF zr_pru_agent_serv
           ENTITY zrpruagentserv
           CREATE AUTO FILL CID WITH lt_create_in
           MAPPED DATA(ls_mapped_res)
           REPORTED DATA(ls_reported_res)
           FAILED DATA(ls_failed_res).

    LOOP AT ls_failed_res-zrpruagentserv ASSIGNING FIELD-SYMBOL(<ls_failed_item>).
      APPEND INITIAL LINE TO cs_failed-agsrv ASSIGNING FIELD-SYMBOL(<ls_failed_out>).
      <ls_failed_out>-service = <ls_failed_item>-aipf7service.
      <ls_failed_out>-context = <ls_failed_item>-aipf7context.
      <ls_failed_out>-fail    = CONV #( <ls_failed_item>-%fail-cause ).
      <ls_failed_out>-create  = <ls_failed_item>-%create.
    ENDLOOP.

    LOOP AT ls_reported_res-zrpruagentserv ASSIGNING FIELD-SYMBOL(<ls_reported_item>).
      APPEND INITIAL LINE TO cs_reported-agsrv ASSIGNING FIELD-SYMBOL(<ls_reported_out>).
      <ls_reported_out>-service = <ls_reported_item>-aipf7service.
      <ls_reported_out>-context = <ls_reported_item>-aipf7context.
      <ls_reported_out>-create  = <ls_reported_item>-%create.
    ENDLOOP.

    LOOP AT ls_mapped_res-zrpruagentserv ASSIGNING FIELD-SYMBOL(<ls_mapped_item>).
      APPEND INITIAL LINE TO cs_mapped-agsrv ASSIGNING FIELD-SYMBOL(<ls_mapped_out>).
      <ls_mapped_out>-service = <ls_mapped_item>-aipf7service.
      <ls_mapped_out>-context = <ls_mapped_item>-aipf7context.
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
        ( aipf7Service          = <ls_update>-Service
          aipf7Context          = <ls_update>-Context
          aipf7Class            = <ls_update>-Class
          aipf7CreatedBy        = <ls_update>-CreatedBy
          aipf7CreatedAt        = <ls_update>-CreatedAt
          aipf7ChangedBy        = <ls_update>-ChangedBy
          aipf7LastChanged      = <ls_update>-LastChanged
          aipf7LocalLastChanged = <ls_update>-LocalLastChanged
          %control         = VALUE #(
              aipf7Class            = COND #( WHEN <ls_update>-Control-Class = abap_true THEN if_abap_behv=>mk-on )
              aipf7CreatedBy        = COND #( WHEN <ls_update>-Control-CreatedBy = abap_true THEN if_abap_behv=>mk-on )
              aipf7CreatedAt        = COND #( WHEN <ls_update>-Control-CreatedAt = abap_true THEN if_abap_behv=>mk-on )
              aipf7ChangedBy        = COND #( WHEN <ls_update>-Control-ChangedBy = abap_true THEN if_abap_behv=>mk-on )
              aipf7LastChanged      = COND #( WHEN <ls_update>-Control-LastChanged = abap_true THEN if_abap_behv=>mk-on )
              aipf7LocalLastChanged = COND #( WHEN <ls_update>-Control-LocalLastChanged = abap_true
                                         THEN if_abap_behv=>mk-on ) ) ) ).

    MODIFY ENTITIES OF zr_pru_agent_serv
           ENTITY zrpruagentserv
           UPDATE FROM lt_update_in
           REPORTED DATA(ls_reported_res)
           FAILED DATA(ls_failed_res).

    LOOP AT ls_failed_res-zrpruagentserv ASSIGNING FIELD-SYMBOL(<ls_failed_item>).
      APPEND INITIAL LINE TO cs_failed-agsrv ASSIGNING FIELD-SYMBOL(<ls_failed_out>).
      <ls_failed_out>-service = <ls_failed_item>-aipf7service.
      <ls_failed_out>-context = <ls_failed_item>-aipf7context.
      <ls_failed_out>-fail    = CONV #( <ls_failed_item>-%fail-cause ).
      <ls_failed_out>-update  = <ls_failed_item>-%update.
    ENDLOOP.

    LOOP AT ls_reported_res-zrpruagentserv ASSIGNING FIELD-SYMBOL(<ls_reported_item>).
      APPEND INITIAL LINE TO cs_reported-agsrv ASSIGNING FIELD-SYMBOL(<ls_reported_out>).
      <ls_reported_out>-service = <ls_reported_item>-aipf7service.
      <ls_reported_out>-context = <ls_reported_item>-aipf7context.
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
                            ( aipf7service = <ls_delete>-service
                              aipf7context = <ls_delete>-context ) ).

    MODIFY ENTITIES OF zr_pru_agent_serv
           ENTITY zrpruagentserv
           DELETE FROM lt_delete_in
           REPORTED DATA(ls_reported_res)
           FAILED DATA(ls_failed_res).

    LOOP AT ls_failed_res-zrpruagentserv ASSIGNING FIELD-SYMBOL(<ls_failed_item>).
      APPEND INITIAL LINE TO cs_failed-agsrv ASSIGNING FIELD-SYMBOL(<ls_failed_out>).
      <ls_failed_out>-service = <ls_failed_item>-aipf7service.
      <ls_failed_out>-context = <ls_failed_item>-aipf7context.
      <ls_failed_out>-fail    = CONV #( <ls_failed_item>-%fail-cause ).
      <ls_failed_out>-delete  = <ls_failed_item>-%delete.
    ENDLOOP.

    LOOP AT ls_reported_res-zrpruagentserv ASSIGNING FIELD-SYMBOL(<ls_reported_item>).
      APPEND INITIAL LINE TO cs_reported-agsrv ASSIGNING FIELD-SYMBOL(<ls_reported_out>).
      <ls_reported_out>-service = <ls_reported_item>-aipf7service.
      <ls_reported_out>-context = <ls_reported_item>-aipf7context.
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
