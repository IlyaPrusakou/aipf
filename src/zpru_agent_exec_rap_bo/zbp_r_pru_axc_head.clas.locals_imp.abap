CLASS lcl_buffer DEFINITION CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES: BEGIN OF ts_run_count,
             numberrangelevel TYPE zpru_de_run_id,
           END OF ts_run_count.

    TYPES tt_run_count TYPE STANDARD TABLE OF ts_run_count WITH EMPTY KEY.

    CLASS-DATA st_run_count TYPE tt_run_count.

    TYPES: BEGIN OF ts_query_count,
             AIPF7RunUuid     TYPE sysuuid_x16,
             AIPF7QueryUuid   TYPE sysuuid_x16,
             AIPF7QueryNumber TYPE zpru_de_query_number,
           END OF ts_query_count.

    TYPES tt_query_count TYPE STANDARD TABLE OF ts_query_count WITH EMPTY KEY.

    CLASS-DATA st_query_count TYPE tt_query_count.

    TYPES: BEGIN OF ts_step_count,
             AIPF7RunUuid    TYPE sysuuid_x16,
             AIPF7QueryUuid  TYPE sysuuid_x16,
             AIPF7stepuuid   TYPE sysuuid_x16,
             AIPF7stepnumber TYPE zpru_de_step_number,
           END OF ts_step_count.

    TYPES tt_step_count TYPE STANDARD TABLE OF ts_step_count WITH EMPTY KEY.

    CLASS-DATA st_step_count TYPE tt_step_count.

ENDCLASS.


CLASS lcl_buffer IMPLEMENTATION.
ENDCLASS.


CLASS lhc_zr_pru_axc_head DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.
    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING
      REQUEST requested_authorizations FOR executionHeader
      RESULT result.
    METHODS GenerateRunID FOR DETERMINE ON MODIFY
      IMPORTING keys FOR executionHeader~GenerateRunID.
ENDCLASS.


CLASS lhc_zr_pru_axc_head IMPLEMENTATION.
  METHOD get_global_authorizations.
  ENDMETHOD.

  METHOD GenerateRunID.
    DATA lv_number      TYPE cl_numberrange_runtime=>nr_number.
    DATA lv_number_buffered      TYPE cl_numberrange_runtime=>nr_number.
    DATA lt_update_root TYPE TABLE FOR UPDATE zr_pru_axc_head\\executionHeader.

    TRY.
        cl_numberrange_runtime=>number_status(
          EXPORTING
            nr_range_nr = '01'
            object      = 'ZPRU_AXCHD'
          IMPORTING
            number      = lv_number ).

        IF lv_number IS NOT INITIAL.
          SORT lcl_buffer=>st_run_count BY numberrangelevel DESCENDING.
          lv_number_buffered = VALUE #( lcl_buffer=>st_run_count[ 1 ]-numberrangelevel OPTIONAL ).
          IF lv_number_buffered < lv_number.
            APPEND INITIAL LINE TO lcl_buffer=>st_run_count ASSIGNING FIELD-SYMBOL(<ls_query_count>).
            <ls_query_count>-numberrangelevel = lv_number_buffered.
          ENDIF.
        ENDIF.

      CATCH cx_nr_object_not_found
           cx_number_ranges.
        ASSERT 1 = 2.
    ENDTRY.

    IF  lcl_buffer=>st_run_count IS NOT INITIAL.
      SORT  lcl_buffer=>st_run_count BY numberrangelevel DESCENDING.
      lv_number = VALUE #( lcl_buffer=>st_run_count[ 1 ]-numberrangelevel OPTIONAL ).
    ELSE.
      lv_number = '00000000000000000000'.
    ENDIF.

    LOOP AT keys ASSIGNING FIELD-SYMBOL(<ls_key>).

      lv_number = lv_number + 1.

      APPEND INITIAL LINE TO lt_update_root ASSIGNING FIELD-SYMBOL(<ls_update_root>).
      <ls_update_root>-%tky       = <ls_key>-%tky.
      <ls_update_root>-AIPF7RunID = lv_number.
      <ls_update_root>-%control-AIPF7RunID = if_abap_behv=>mk-on.

      APPEND INITIAL LINE TO lcl_buffer=>st_run_count ASSIGNING FIELD-SYMBOL(<ls_run_count>).
      <ls_run_count>-numberrangelevel = lv_number.

    ENDLOOP.

    IF lt_update_root IS NOT INITIAL.
      MODIFY ENTITIES OF zr_pru_axc_head
             IN LOCAL MODE
             ENTITY executionHeader
             UPDATE FROM lt_update_root
             REPORTED DATA(ls_upd_reported).

      reported = CORRESPONDING #( DEEP ls_upd_reported ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.


CLASS lhc_executionquery DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.
    METHODS GenerateQueryID FOR DETERMINE ON MODIFY
      IMPORTING keys FOR executionQuery~GenerateQueryID.

ENDCLASS.


CLASS lhc_executionquery IMPLEMENTATION.
  METHOD GenerateQueryID.
    DATA lv_number          TYPE zpru_de_query_number.
    DATA lv_number_buffered TYPE zpru_de_query_number.
    DATA lt_update_query    TYPE TABLE FOR UPDATE zr_pru_axc_head\\executionQuery.

    READ ENTITIES OF zr_pru_axc_head
         IN LOCAL MODE
         ENTITY executionQuery
         FIELDS ( AIPF7RunUuid ) WITH CORRESPONDING #( keys )
         RESULT DATA(lt_query).

    IF lt_query IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lt_query ASSIGNING FIELD-SYMBOL(<ls_query_group>)
         GROUP BY <ls_query_group>-AIPF7RunUuid
         ASSIGNING FIELD-SYMBOL(<lv_query_key>).

      CLEAR: lv_number,
             lv_number_buffered.

      SELECT AIPF7RunUuid,
             AIPF7QueryUuid,
             AIPF7QueryNumber
        FROM zi_pru_axc_query
        WHERE AIPF7RunUuid = @<lv_query_key>
        ORDER BY AIPF7QueryNumber DESCENDING
        INTO TABLE @DATA(lt_last_id)
        UP TO 1 ROWS.
      IF sy-subrc = 0.
        ASSIGN lt_last_id[ 1 ] TO FIELD-SYMBOL(<ls_number_db>).
        IF sy-subrc = 0.
          DATA(lt_buffer_copy) = lcl_buffer=>st_query_count.
          DELETE lt_buffer_copy WHERE AIPF7RunUuid <> <lv_query_key>.
          SORT lt_buffer_copy BY AIPF7querynumber DESCENDING.

          lv_number_buffered = VALUE #( lt_buffer_copy[ 1 ]-aipf7querynumber OPTIONAL ).

          IF lv_number_buffered < <ls_number_db>-AIPF7QueryNumber.
            APPEND INITIAL LINE TO lcl_buffer=>st_query_count ASSIGNING FIELD-SYMBOL(<ls_query_count>).
            <ls_query_count>-AIPF7RunUuid     = <ls_number_db>-AIPF7RunUuid.
            <ls_query_count>-aipf7queryuuid   = <ls_number_db>-AIPF7QueryUuid.
            <ls_query_count>-aipf7querynumber = <ls_number_db>-AIPF7QueryNumber.
          ENDIF.
        ENDIF.
      ENDIF.

      lt_buffer_copy = lcl_buffer=>st_query_count.
      DELETE lt_buffer_copy WHERE AIPF7RunUuid <> <lv_query_key>.

      IF lt_buffer_copy IS NOT INITIAL.
        SORT lt_buffer_copy BY AIPF7querynumber DESCENDING.
        lv_number = VALUE #( lt_buffer_copy[ 1 ]-aipf7querynumber OPTIONAL ).
      ELSE.
        lv_number = '0000000000'.
      ENDIF.

      LOOP AT GROUP <lv_query_key> ASSIGNING FIELD-SYMBOL(<ls_member>).

        lv_number += 1.

        APPEND INITIAL LINE TO lt_update_query ASSIGNING FIELD-SYMBOL(<ls_update>).
        <ls_update>-%tky             = <ls_member>-%tky.
        <ls_update>-AIPF7QueryNumber = lv_number.
        <ls_update>-%control-AIPF7QueryNumber = if_abap_behv=>mk-on.

        APPEND INITIAL LINE TO lcl_buffer=>st_query_count ASSIGNING <ls_query_count>.
        <ls_query_count>-aipf7runuuid     = <ls_member>-AIPF7RunUuid.
        <ls_query_count>-aipf7queryuuid   = <ls_member>-aipf7queryuuid.
        <ls_query_count>-aipf7querynumber = lv_number.
      ENDLOOP.

      CLEAR lt_last_id.

    ENDLOOP.

    IF lt_update_query IS NOT INITIAL.
      MODIFY ENTITIES OF zr_pru_axc_head
             IN LOCAL MODE
             ENTITY executionQuery
             UPDATE FROM lt_update_query
             REPORTED DATA(ls_upd_reported).

      reported = CORRESPONDING #( DEEP ls_upd_reported ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.


CLASS lhc_executionstep DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.
    METHODS GenerateStepID FOR DETERMINE ON MODIFY
      IMPORTING keys FOR executionStep~GenerateStepID.

ENDCLASS.


CLASS lhc_executionstep IMPLEMENTATION.
  METHOD GenerateStepID.
    DATA lv_number          TYPE zpru_de_step_number.
    DATA lv_number_buffered TYPE zpru_de_step_number.
    DATA lt_update_step     TYPE TABLE FOR UPDATE zr_pru_axc_head\\executionStep.

    READ ENTITIES OF zr_pru_axc_head
         IN LOCAL MODE
         ENTITY executionStep
         FIELDS ( AIPF7RunUuid AIPF7QueryUuid ) WITH CORRESPONDING #( keys )
         RESULT DATA(lt_steps).

    IF lt_steps IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lt_steps ASSIGNING FIELD-SYMBOL(<ls_step_group>)
         GROUP BY ( AIPF7RunUuid   = <ls_step_group>-AIPF7RunUuid
                    AIPF7QueryUuid = <ls_step_group>-AIPF7QueryUuid )
         ASSIGNING FIELD-SYMBOL(<ls_step_key>).

      CLEAR: lv_number,
             lv_number_buffered.

      SELECT AIPF7RunUuid,
             AIPF7QueryUuid,
             AIPF7StepUuid,
             AIPF7StepNumber
        FROM zi_pru_axc_step
        WHERE AIPF7RunUuid   = @<ls_step_key>-AIPF7RunUuid
          AND AIPF7QueryUuid = @<ls_step_key>-AIPF7QueryUuid
        ORDER BY AIPF7StepNumber DESCENDING
        INTO TABLE @DATA(lt_last_id)
        UP TO 1 ROWS.
      IF sy-subrc = 0.
        ASSIGN lt_last_id[ 1 ] TO FIELD-SYMBOL(<ls_number_db>).
        IF sy-subrc = 0.
          DATA(lt_buffer_copy) = lcl_buffer=>st_step_count.
          DELETE lt_buffer_copy WHERE    AIPF7RunUuid   <> <ls_step_key>-AIPF7RunUuid
                                      OR AIPF7QueryUuid <> <ls_step_key>-aipf7queryuuid.
          SORT lt_buffer_copy BY AIPF7StepNumber DESCENDING.

          lv_number_buffered = VALUE #( lt_buffer_copy[ 1 ]-AIPF7StepNumber OPTIONAL ).

          IF lv_number_buffered < <ls_number_db>-AIPF7StepNumber.
            APPEND INITIAL LINE TO lcl_buffer=>st_step_count ASSIGNING FIELD-SYMBOL(<ls_step_count>).
            <ls_step_count>-AIPF7RunUuid    = <ls_number_db>-AIPF7RunUuid.
            <ls_step_count>-aipf7queryuuid  = <ls_number_db>-AIPF7QueryUuid.
            <ls_step_count>-AIPF7StepUuid   = <ls_number_db>-AIPF7StepUuid.
            <ls_step_count>-AIPF7StepNumber = <ls_number_db>-AIPF7StepNumber.
          ENDIF.
        ENDIF.
      ENDIF.

      lt_buffer_copy = lcl_buffer=>st_step_count.
      DELETE lt_buffer_copy WHERE    AIPF7RunUuid   <> <ls_step_key>-AIPF7RunUuid
                                  OR AIPF7QueryUuid <> <ls_step_key>-aipf7queryuuid.

      IF lt_buffer_copy IS NOT INITIAL.
        SORT lt_buffer_copy BY AIPF7StepNumber DESCENDING.
        lv_number = VALUE #( lt_buffer_copy[ 1 ]-AIPF7StepNumber OPTIONAL ).
      ELSE.
        lv_number = '0000000000'.
      ENDIF.

      LOOP AT GROUP <ls_step_key> ASSIGNING FIELD-SYMBOL(<ls_member>).

        lv_number += 1.

        APPEND INITIAL LINE TO lt_update_step ASSIGNING FIELD-SYMBOL(<ls_update>).
        <ls_update>-%tky            = <ls_member>-%tky.
        <ls_update>-AIPF7StepNumber = lv_number.
        <ls_update>-%control-AIPF7StepNumber = if_abap_behv=>mk-on.

        APPEND INITIAL LINE TO lcl_buffer=>st_step_count ASSIGNING <ls_step_count>.
        <ls_step_count>-aipf7runuuid    = <ls_member>-AIPF7RunUuid.
        <ls_step_count>-aipf7queryuuid  = <ls_member>-aipf7queryuuid.
        <ls_step_count>-aipf7stepuuid   = <ls_member>-aipf7stepuuid.
        <ls_step_count>-AIPF7StepNumber = lv_number.
      ENDLOOP.

      CLEAR lt_last_id.

    ENDLOOP.

    IF lt_update_step IS NOT INITIAL.
      MODIFY ENTITIES OF zr_pru_axc_head
             IN LOCAL MODE
             ENTITY executionStep
             UPDATE FROM lt_update_step
             REPORTED DATA(ls_upd_reported).

      reported = CORRESPONDING #( DEEP ls_upd_reported ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.


CLASS lsc_zr_pru_axc_head DEFINITION INHERITING FROM cl_abap_behavior_saver.

  PROTECTED SECTION.
    METHODS save_modified REDEFINITION.

    METHODS cleanup       REDEFINITION.

ENDCLASS.


CLASS lsc_zr_pru_axc_head IMPLEMENTATION.
  METHOD save_modified.

    TYPES: BEGIN OF ts_head_key,
             runuuid TYPE sysuuid_x16,
           END OF ts_head_key.

    DATA lt_head_del_key TYPE STANDARD TABLE OF ts_head_key WITH EMPTY KEY.
    DATA lt_head_mod_tab       TYPE TABLE OF zpru_axc_head WITH EMPTY KEY.
    DATA lt_head_buffer       TYPE TABLE OF zpru_axc_head WITH EMPTY KEY.
    DATA lo_head_struct TYPE REF TO cl_abap_structdescr.
*    DATA lt_del_tab       TYPE lcl_buffer=>tt_root_db_keys.
*    DATA lt_mod_child_tab TYPE TABLE OF zpru_po_item WITH EMPTY KEY.
*    DATA lt_del_child_tab TYPE lcl_buffer=>tt_child_db_keys.


    lt_head_mod_tab = CORRESPONDING #( create-executionheader MAPPING FROM ENTITY ).

    SELECT *
    FROM zpru_axc_head
    FOR ALL ENTRIES IN @update-executionheader
    WHERE runuuid = @update-executionheader-aipf7runuuid
    INTO TABLE @DATA(lt_head_db).


    lo_head_struct ?= cl_abap_structdescr=>describe_by_name( p_name = `ZPRU_AXC_HEAD` ).
    DATA(lt_head_symbols) = lo_head_struct->get_symbols( ).

    lt_head_buffer = CORRESPONDING #( update-executionheader MAPPING FROM ENTITY ).

    LOOP AT lt_head_buffer  ASSIGNING FIELD-SYMBOL(<ls_head_buffer>).

      ASSIGN lt_head_db[ runuuid = <ls_head_buffer>-runuuid ] TO FIELD-SYMBOL(<ls_head_db>).
      IF sy-subrc <> 0.
        ASSERT 1 = 2.
      ENDIF.

      ASSIGN update-executionheader[ KEY id
                                     COMPONENTS
                                     aipf7runuuid = <ls_head_buffer>-runuuid ] TO FIELD-SYMBOL(<ls_head_control>).
      IF sy-subrc <> 0.
        ASSERT 1 = 2.
      ENDIF.

      APPEND INITIAL LINE TO lt_head_mod_tab ASSIGNING FIELD-SYMBOL(<ls_head_mod>).
      <ls_head_mod> = <ls_head_db>.

      LOOP AT lt_head_symbols ASSIGNING FIELD-SYMBOL(<ls_head_symbol>).

        data(lv_aipf7_field) = |AIPF7{ <ls_head_symbol>-name }|.

        ASSIGN COMPONENT lv_aipf7_field OF STRUCTURE <ls_head_control>-%control TO FIELD-SYMBOL(<lv_control>).
        IF sy-subrc <> 0.
          ASSERT 1 = 2.
        ENDIF.

        IF <lv_control> <> if_abap_behv=>mk-on.
          CONTINUE.
        ENDIF.

        ASSIGN COMPONENT <ls_head_symbol>-name OF STRUCTURE <ls_head_mod> TO FIELD-SYMBOL(<lv_head_target>).
        IF sy-subrc <> 0.
          ASSERT 1 = 2.
        ENDIF.

        ASSIGN COMPONENT <ls_head_symbol>-name OF STRUCTURE <ls_head_buffer> TO FIELD-SYMBOL(<lv_head_source>).
        IF sy-subrc <> 0.
          ASSERT 1 = 2.
        ENDIF.

        IF <lv_head_target> <> <lv_head_source>.
          <lv_head_target> = <lv_head_source>.
        ENDIF.
      ENDLOOP.
    ENDLOOP.




*    MODIFY zpru_axc_head FROM TABLE @( CORRESPONDING #( lt_mod_tab ) ).
*
*
*    IF line_exists( lcl_buffer=>root_buffer[ deleted = abap_true ] ).
*      LOOP AT lcl_buffer=>root_buffer ASSIGNING FIELD-SYMBOL(<ls_del>) WHERE     deleted  = abap_true
*                                                                             AND is_draft = if_abap_behv=>mk-off.
*        APPEND VALUE #( purchase_order_id = <ls_del>-instance-purchaseorderid ) TO lt_del_tab.
*      ENDLOOP.
*      DELETE zpru_purc_order FROM TABLE @( CORRESPONDING #( lt_del_tab ) ). " qqq use on your  database tables
*
*    ENDIF.
*
*    IF line_exists( lcl_buffer=>child_buffer[ changed = abap_true ] ).
*      LOOP AT lcl_buffer=>child_buffer ASSIGNING FIELD-SYMBOL(<ls_mod_child>) WHERE     changed  = abap_true
*                                                                                    AND is_draft = if_abap_behv=>mk-off.
*        APPEND CORRESPONDING #( <ls_mod_child>-instance MAPPING FROM ENTITY ) TO lt_mod_child_tab.
*      ENDLOOP.
*
*      MODIFY zpru_po_item FROM TABLE @( CORRESPONDING #( lt_mod_child_tab ) ). " qqq use on your  database tables
*    ENDIF.
*
*    IF line_exists( lcl_buffer=>child_buffer[ deleted = abap_true ] ).
*      LOOP AT lcl_buffer=>child_buffer ASSIGNING FIELD-SYMBOL(<ls_del_child>) WHERE     deleted  = abap_true
*                                                                                    AND is_draft = if_abap_behv=>mk-off.
*        APPEND VALUE #( purchase_order_id = <ls_del_child>-instance-purchaseorderid
*                        item_id           = <ls_del_child>-instance-itemid  ) TO lt_del_child_tab.
*      ENDLOOP.
*      DELETE zpru_po_item FROM TABLE @( CORRESPONDING #( lt_del_child_tab ) ). " qqq use on your  database tables
*    ENDIF.
*
*
*


  ENDMETHOD.

  METHOD cleanup.
    CLEAR lcl_buffer=>st_run_count.
    CLEAR lcl_buffer=>st_query_count.
    CLEAR lcl_buffer=>st_step_count.
  ENDMETHOD.
ENDCLASS.
