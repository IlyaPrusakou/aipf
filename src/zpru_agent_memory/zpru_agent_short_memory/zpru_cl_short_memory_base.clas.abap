CLASS zpru_cl_short_memory_base DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zpru_if_agent_frw.
    INTERFACES zpru_if_short_memory_provider.

  PROTECTED SECTION.
    DATA mt_agent_message        TYPE zpru_if_short_memory_provider=>tt_message.
    DATA mo_discard_strategy     TYPE REF TO zpru_if_discard_strategy.
    DATA mo_long_memory_provider TYPE REF TO zpru_if_long_memory_provider.
    DATA mv_short_memory_size    TYPE zpru_de_mem_volume VALUE 20.

    METHODS discard_messages
      IMPORTING io_input  TYPE REF TO zpru_if_payload
      EXPORTING eo_output TYPE REF TO zpru_if_payload
      RAISING   zpru_cx_agent_core.

  PRIVATE SECTION.
ENDCLASS.


CLASS zpru_cl_short_memory_base IMPLEMENTATION.
  METHOD zpru_if_short_memory_provider~clear_history.
    CLEAR mt_agent_message.
  ENDMETHOD.

  METHOD zpru_if_short_memory_provider~get_history.
    rt_history = mt_agent_message.
  ENDMETHOD.

  METHOD zpru_if_short_memory_provider~save_message.
    DATA lt_message_2_discard LIKE mt_agent_message.
    DATA lo_discard_input     TYPE REF TO zpru_if_payload.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA lo_discard_output    TYPE REF TO zpru_if_payload.
    DATA lr_sort_number_r     TYPE RANGE OF i.

    IF it_message IS INITIAL.
      RETURN.
    ENDIF.

    GET TIME STAMP FIELD DATA(lv_now).

    DATA(lv_count) = 0.
    LOOP AT mt_agent_message ASSIGNING FIELD-SYMBOL(<ls_search_count>).
      IF lv_count < <ls_search_count>-sortnumber.
        lv_count = <ls_search_count>-sortnumber.
      ENDIF.
    ENDLOOP.

    lv_count += 1.

    LOOP AT it_message ASSIGNING FIELD-SYMBOL(<ls_message>).

      IF <ls_message>-messagetime IS INITIAL.
        <ls_message>-messagetime = lv_now.
      ENDIF.

      IF <ls_message>-messagetype IS INITIAL.
        <ls_message>-messagetype = zpru_if_short_memory_provider=>cs_msg_type-info.
      ENDIF.

      APPEND INITIAL LINE TO mt_agent_message ASSIGNING FIELD-SYMBOL(<ls_target>).
      <ls_target> = <ls_message>.
      <ls_target>-sortnumber = lv_count.

      lv_count += 1.

    ENDLOOP.

    SORT mt_agent_message BY sortnumber DESCENDING.

    IF lines( mt_agent_message ) > mv_short_memory_size.
      LOOP AT mt_agent_message FROM mv_short_memory_size + 1 ASSIGNING FIELD-SYMBOL(<ls_message_to_discard>).
        APPEND INITIAL LINE TO lt_message_2_discard ASSIGNING FIELD-SYMBOL(<ls_discard>).
        <ls_discard> = <ls_message_to_discard>.

        APPEND INITIAL LINE TO lr_sort_number_r ASSIGNING FIELD-SYMBOL(<ls_sort_number_r>).
        <ls_sort_number_r>-sign   = 'I'.
        <ls_sort_number_r>-option = 'EQ'.
        <ls_sort_number_r>-low    = <ls_discard>-sortnumber.
      ENDLOOP.

      IF lt_message_2_discard IS NOT INITIAL.

        TRY.
            lo_discard_input ?= zpru_cl_agent_service_mngr=>get_service(
                                    iv_service = `ZPRU_IF_PAYLOAD`
                                    iv_context = zpru_if_agent_frw=>cs_context-standard ).
          CATCH zpru_cx_agent_core.
            RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
        ENDTRY.

        lo_discard_input->set_data( ir_data = REF #( lt_message_2_discard ) ).

        TRY.
            lo_discard_output ?= zpru_cl_agent_service_mngr=>get_service(
                                     iv_service = `ZPRU_IF_PAYLOAD`
                                     iv_context = zpru_if_agent_frw=>cs_context-standard ).
          CATCH zpru_cx_agent_core.
            RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
        ENDTRY.

        discard_messages( EXPORTING io_input  = lo_discard_input
                          IMPORTING eo_output = lo_discard_output ).

        eo_output = lo_discard_output.

        IF lr_sort_number_r IS NOT INITIAL.
          DELETE mt_agent_message WHERE sortnumber IN lr_sort_number_r.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD discard_messages.
    IF io_input IS NOT BOUND.
      RETURN.
    ENDIF.

    zpru_if_short_memory_provider~get_discard_strategy( )->discard(
      EXPORTING io_long_memory = zpru_if_short_memory_provider~get_long_memory( )
                io_input       = io_input
      IMPORTING eo_output      = eo_output ).
  ENDMETHOD.

  METHOD zpru_if_short_memory_provider~get_discard_strategy.
    IF mo_discard_strategy IS NOT BOUND.

      TRY.
          mo_discard_strategy ?= zpru_cl_agent_service_mngr=>get_service(
                                     iv_service = `ZPRU_IF_DISCARD_STRATEGY`
                                     iv_context = zpru_if_agent_frw=>cs_context-st_discard_strategy_delete ).
        CATCH zpru_cx_agent_core.
          RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
      ENDTRY.

    ENDIF.

    ro_discard_strategy = mo_discard_strategy.
  ENDMETHOD.

  METHOD zpru_if_short_memory_provider~set_discard_strategy.
    mo_discard_strategy = io_discard_strategy.
  ENDMETHOD.

  METHOD zpru_if_short_memory_provider~get_long_memory.
    IF mo_long_memory_provider IS NOT BOUND.

      TRY.
          mo_long_memory_provider ?= zpru_cl_agent_service_mngr=>get_service(
                                         iv_service = `ZPRU_IF_LONG_MEMORY_PROVIDER`
                                         iv_context = zpru_if_agent_frw=>cs_context-standard ).
        CATCH zpru_cx_agent_core.
          RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
      ENDTRY.

    ENDIF.
    ro_long_memory = mo_long_memory_provider.
  ENDMETHOD.

  METHOD zpru_if_short_memory_provider~set_long_memory.
    mo_long_memory_provider = io_long_memory.
  ENDMETHOD.

  METHOD zpru_if_short_memory_provider~get_mem_volume.
    rv_mem_volume = mv_short_memory_size.
  ENDMETHOD.

  METHOD zpru_if_short_memory_provider~set_mem_volume.
    mv_short_memory_size = iv_mem_volume.
  ENDMETHOD.
ENDCLASS.
