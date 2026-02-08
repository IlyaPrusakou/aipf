CLASS zpru_cl_summarize_simple DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zpru_if_agent_frw.
    INTERFACES zpru_if_summarization.
ENDCLASS.


CLASS zpru_cl_summarize_simple IMPLEMENTATION.
  METHOD zpru_if_summarization~summarize.
    DATA lt_message       TYPE zpru_if_long_mem_persistence=>tt_message.
    DATA lt_summarization TYPE zpru_if_long_mem_persistence=>tt_summarization.
    DATA lv_string        TYPE zpru_if_agent_frw=>ts_json.
    DATA lo_utility       TYPE REF TO zpru_if_agent_util.

    IF io_input IS NOT BOUND.
      RETURN.
    ENDIF.

    lt_message = io_input->get_data( )->*.

    TRY.
        lo_utility ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_AGENT_UTIL`
                                                               iv_context = `STANDARD` ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    lo_utility->convert_to_string( EXPORTING ir_abap   = REF #(  lt_message )
                                   CHANGING  cr_string = lv_string ).

    SORT lt_message BY messagetime DESCENDING.
    DATA(ls_last_message) = VALUE #( lt_message[ 1 ] OPTIONAL ).

    APPEND INITIAL LINE TO lt_summarization ASSIGNING FIELD-SYMBOL(<ls_sum>).
    <ls_sum> = CORRESPONDING #( ls_last_message EXCEPT content ).
    <ls_sum>-summarycid = ls_last_message-messagecid.
    <ls_sum>-content     = lv_string.

    IF lt_summarization IS NOT INITIAL.
      IF eo_output IS BOUND.
        eo_output->set_data( NEW zpru_if_long_mem_persistence=>tt_summarization( lt_summarization ) ).
      ELSE.

        TRY.
            eo_output ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_PAYLOAD`
                                                                  iv_context = `STANDARD` ).
          CATCH zpru_cx_agent_core.
            RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
        ENDTRY.

        eo_output->set_data( NEW zpru_if_long_mem_persistence=>tt_summarization( lt_summarization ) ).
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
