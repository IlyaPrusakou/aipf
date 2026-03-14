CLASS lhc_zr_pru_axc_head DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.
    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
              IMPORTING
              REQUEST requested_authorizations FOR executionHeader
              RESULT result.
    METHODS GenerateID FOR DETERMINE ON MODIFY
              IMPORTING keys FOR executionHeader~GenerateID.
ENDCLASS.


CLASS lhc_zr_pru_axc_head IMPLEMENTATION.
  METHOD get_global_authorizations.
  ENDMETHOD.

  METHOD GenerateID.
    DATA lv_number      TYPE cl_numberrange_runtime=>nr_number.
    DATA lt_update_root TYPE TABLE FOR UPDATE zr_pru_axc_head\\executionHeader.

    LOOP AT keys ASSIGNING FIELD-SYMBOL(<ls_key>).

      TRY.
          cl_numberrange_runtime=>number_get( EXPORTING nr_range_nr = '01'
                                                        object      = 'ZPRU_AXCHD'
                                              IMPORTING number      = lv_number ).
        CATCH cx_nr_object_not_found
              cx_number_ranges.
          ASSERT 1 = 2.
      ENDTRY.

      APPEND INITIAL LINE TO lt_update_root ASSIGNING FIELD-SYMBOL(<ls_update_root>).
      <ls_update_root>-%tky       = <ls_key>-%tky.
      <ls_update_root>-AIPF7RunID = lv_number.
      <ls_update_root>-%control-AIPF7RunID = if_abap_behv=>mk-on.
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
