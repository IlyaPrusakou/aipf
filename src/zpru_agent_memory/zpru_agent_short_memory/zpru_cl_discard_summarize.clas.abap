CLASS zpru_cl_discard_summarize DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zpru_if_agent_frw.
    INTERFACES zpru_if_discard_strategy.
ENDCLASS.


CLASS zpru_cl_discard_summarize IMPLEMENTATION.
  METHOD zpru_if_discard_strategy~discard.
    DATA lo_summarized_data TYPE REF TO zpru_if_payload.

    IF io_long_memory IS NOT BOUND OR io_input IS NOT BOUND.
      RETURN.
    ENDIF.

    TRY.
        lo_summarized_data ?= zpru_cl_agent_service_mngr=>get_service(
                                  iv_service = `ZPRU_IF_PAYLOAD`
                                  iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    io_long_memory->summarize_conversation( EXPORTING io_input  = io_input
                                            IMPORTING eo_output = lo_summarized_data
                                                      ev_error  = DATA(lv_error) ).

    IF lv_error = abap_true.
      RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDIF.

    io_long_memory->save_summary( EXPORTING io_input  = lo_summarized_data
                                  IMPORTING eo_output = eo_output
                                            ev_error  = lv_error ).

    IF lv_error = abap_true.
      RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
