CLASS zpru_cl_long_memory_base DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zpru_if_agent_frw.
    INTERFACES zpru_if_long_memory_provider.

  PROTECTED SECTION.
    DATA mo_msg_persistence TYPE REF TO zpru_if_long_mem_persistence.
    DATA mo_sum_persistence TYPE REF TO zpru_if_long_mem_persistence.
    DATA mo_summarize       TYPE REF TO zpru_if_summarization.

    METHODS prepare_db_msg
      IMPORTING io_input  TYPE REF TO zpru_if_payload
      EXPORTING eo_output TYPE REF TO zpru_if_payload.

    METHODS prepare_db_sum
      IMPORTING io_input  TYPE REF TO zpru_if_payload
      EXPORTING eo_output TYPE REF TO zpru_if_payload.

ENDCLASS.


CLASS zpru_cl_long_memory_base IMPLEMENTATION.
  METHOD zpru_if_long_memory_provider~retrieve_summary.

    DATA lo_msg_service TYPE REF TO zpru_if_msum_service.
    DATA lo_util        TYPE REF TO zpru_if_agent_util.

    TRY.
        lo_msg_service ?= zpru_cl_agent_service_mngr=>get_service(
                              iv_service = `ZPRU_IF_MSUM_SERVICE`
                              iv_context = zpru_if_agent_frw=>cs_context-st_persistence_message ).

        lo_util ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_AGENT_UTIL`
                                                            iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    lo_msg_service->read_msum( EXPORTING it_msum_read_k = it_msum_read_k
                               IMPORTING et_msum        = DATA(lt_messages) ).

    LOOP AT lt_messages ASSIGNING FIELD-SYMBOL(<ls_db_messages>).
      APPEND INITIAL LINE TO et_mem_sum ASSIGNING FIELD-SYMBOL(<ls_target>).
      <ls_target>-summaryuuid = <ls_db_messages>-summaryuuid.
      <ls_target>-content     = lo_util->deserialize_xstring_2_json( <ls_db_messages>-content ).
      <ls_target>-summarycid  = <ls_db_messages>-summarycid.
      <ls_target>-stage       = <ls_db_messages>-stage.
      <ls_target>-substage    = <ls_db_messages>-substage.
      <ls_target>-namespace   = <ls_db_messages>-namespace.
      <ls_target>-username    = <ls_db_messages>-username.
      <ls_target>-agentuuid   = <ls_db_messages>-agentuuid.
      <ls_target>-runuuid     = <ls_db_messages>-runuuid.
      <ls_target>-queryuuid   = <ls_db_messages>-queryuuid.
      <ls_target>-stepuuid    = <ls_db_messages>-stepuuid.
      <ls_target>-messagetime = <ls_db_messages>-messagetime.
      <ls_target>-createdby   = <ls_db_messages>-createdby.
      <ls_target>-createdat   = <ls_db_messages>-createdat.
      <ls_target>-changedby   = <ls_db_messages>-changedby.
      <ls_target>-changedat   = <ls_db_messages>-changedat.

    ENDLOOP.

  ENDMETHOD.

  METHOD zpru_if_long_memory_provider~retrieve_message.
    DATA lo_msg_service TYPE REF TO zpru_if_mmsg_service.
    DATA lo_util        TYPE REF TO zpru_if_agent_util.

    TRY.
        lo_msg_service ?= zpru_cl_agent_service_mngr=>get_service(
                              iv_service = `ZPRU_IF_MMSG_SERVICE`
                              iv_context = zpru_if_agent_frw=>cs_context-st_persistence_message ).

        lo_util ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_AGENT_UTIL`
                                                            iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    lo_msg_service->read_mmsg( EXPORTING it_mmsg_read_k = it_mmsg_read_k
                               IMPORTING et_mmsg        = DATA(lt_messages) ).

    LOOP AT lt_messages ASSIGNING FIELD-SYMBOL(<ls_db_messages>).
      APPEND INITIAL LINE TO et_mem_msg ASSIGNING FIELD-SYMBOL(<ls_target>).
      <ls_target>-messageuuid = <ls_db_messages>-messageuuid.
      <ls_target>-content     = lo_util->deserialize_xstring_2_json( <ls_db_messages>-content ).
      <ls_target>-messagetype = <ls_db_messages>-messagetype.
      <ls_target>-messagecid  = <ls_db_messages>-messagecid.
      <ls_target>-stage       = <ls_db_messages>-stage.
      <ls_target>-substage    = <ls_db_messages>-substage.
      <ls_target>-namespace   = <ls_db_messages>-namespace.
      <ls_target>-username    = <ls_db_messages>-username.
      <ls_target>-agentuuid   = <ls_db_messages>-agentuuid.
      <ls_target>-runuuid     = <ls_db_messages>-runuuid.
      <ls_target>-queryuuid   = <ls_db_messages>-queryuuid.
      <ls_target>-stepuuid    = <ls_db_messages>-stepuuid.
      <ls_target>-messagetime = <ls_db_messages>-messagetime.
      <ls_target>-createdby   = <ls_db_messages>-createdby.
      <ls_target>-createdat   = <ls_db_messages>-createdat.
      <ls_target>-changedby   = <ls_db_messages>-changedby.
      <ls_target>-changedat   = <ls_db_messages>-changedat.

    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_long_memory_provider~save_summary.
    DATA lo_prepar_response TYPE REF TO zpru_if_payload.
    DATA lo_persist_request TYPE REF TO zpru_if_payload.

    IF io_input IS NOT BOUND.
      RETURN.
    ENDIF.

    TRY.
        lo_prepar_response ?= zpru_cl_agent_service_mngr=>get_service(
                                  iv_service = `ZPRU_IF_PAYLOAD`
                                  iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    prepare_db_sum( EXPORTING io_input  = io_input
                    IMPORTING eo_output = lo_prepar_response ).

    lo_persist_request = lo_prepar_response.

    DATA(lo_sum_prst) = zpru_if_long_memory_provider~get_sum_persistence( ).
    lo_sum_prst->persist( EXPORTING io_input  = lo_persist_request
                          IMPORTING eo_output = eo_output ).
  ENDMETHOD.

  METHOD zpru_if_long_memory_provider~save_messages.
    DATA lo_prepar_response TYPE REF TO zpru_if_payload.
    DATA lo_persist_request TYPE REF TO zpru_if_payload.

    IF io_input IS NOT BOUND.
      RETURN.
    ENDIF.

    TRY.
        lo_prepar_response ?= zpru_cl_agent_service_mngr=>get_service(
                                  iv_service = `ZPRU_IF_PAYLOAD`
                                  iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    prepare_db_msg( EXPORTING io_input  = io_input
                    IMPORTING eo_output = lo_prepar_response ).

    lo_persist_request = lo_prepar_response.

    DATA(lo_msg_prst) = zpru_if_long_memory_provider~get_msg_persistence( ).
    lo_msg_prst->persist( EXPORTING io_input  = lo_persist_request
                          IMPORTING eo_output = eo_output ).
  ENDMETHOD.

  METHOD zpru_if_long_memory_provider~summarize_conversation.
    IF io_input IS NOT BOUND.
      RETURN.
    ENDIF.

    DATA(lo_summarization) = zpru_if_long_memory_provider~get_summarization( ).

    lo_summarization->summarize( EXPORTING io_input  = io_input
                                 IMPORTING eo_output = eo_output ).
  ENDMETHOD.

  METHOD zpru_if_long_memory_provider~get_msg_persistence.
    IF mo_msg_persistence IS NOT BOUND.

      TRY.
          mo_msg_persistence ?= zpru_cl_agent_service_mngr=>get_service(
                                    iv_service = `ZPRU_IF_LONG_MEM_PERSISTENCE`
                                    iv_context = zpru_if_agent_frw=>cs_context-st_persistence_message ).
        CATCH zpru_cx_agent_core.
          RETURN.
      ENDTRY.

    ENDIF.

    ro_msg_persistence = mo_msg_persistence.
  ENDMETHOD.

  METHOD zpru_if_long_memory_provider~get_sum_persistence.
    IF mo_sum_persistence IS NOT BOUND.
      TRY.
          mo_sum_persistence ?= zpru_cl_agent_service_mngr=>get_service(
                                    iv_service = `ZPRU_IF_LONG_MEM_PERSISTENCE`
                                    iv_context = zpru_if_agent_frw=>cs_context-st_persistence_summarize ).
        CATCH zpru_cx_agent_core.
          RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
      ENDTRY.
    ENDIF.

    ro_sum_persistence = mo_sum_persistence.
  ENDMETHOD.

  METHOD zpru_if_long_memory_provider~set_msg_persistence.
    mo_msg_persistence = io_msg_persistence.
  ENDMETHOD.

  METHOD zpru_if_long_memory_provider~set_sum_persistence.
    mo_sum_persistence = io_sum_persistence.
  ENDMETHOD.

  METHOD prepare_db_msg.
    DATA lt_message_db TYPE zpru_if_long_mem_persistence=>tt_message_db.
    DATA lo_util       TYPE REF TO zpru_if_agent_util.

    FIELD-SYMBOLS <lt_message> TYPE zpru_if_long_mem_persistence=>tt_message.

    IF io_input IS NOT BOUND.
      RETURN.
    ENDIF.

    DATA(lr_data) = io_input->get_data( ).

    ASSIGN lr_data->* TO <lt_message>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    GET TIME STAMP FIELD DATA(lv_now).

    TRY.
        lo_util ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_AGENT_UTIL`
                                                            iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    DATA(lv_count) = 1.
    LOOP AT <lt_message> ASSIGNING FIELD-SYMBOL(<ls_message>).

      APPEND INITIAL LINE TO lt_message_db ASSIGNING FIELD-SYMBOL(<ls_message_db>).
      <ls_message_db> = CORRESPONDING #( <ls_message> EXCEPT content ).

      TRY.
          <ls_message_db>-messageuuid = cl_system_uuid=>create_uuid_x16_static( ).
        CATCH cx_uuid_error.
          ASSERT 1 = 2.
      ENDTRY.

      IF <ls_message_db>-messagecid IS INITIAL.
        <ls_message_db>-messagecid = |{ sy-uname }-{ lv_now }-{ lv_count }|.
      ENDIF.

      IF <ls_message_db>-username IS INITIAL.
        <ls_message_db>-username = sy-uname.
      ENDIF.

      IF <ls_message_db>-messagetype IS INITIAL.
        <ls_message_db>-messagetype = zpru_if_short_memory_provider=>cs_msg_type-info.
      ENDIF.

      <ls_message_db>-content    = lo_util->serialize_json_2_xstring( <ls_message>-content ).

      <ls_message_db>-createdby = sy-uname.
      <ls_message_db>-createdat = lv_now.
      <ls_message_db>-changedby = sy-uname.
      <ls_message_db>-changedat = lv_now.

      lv_count += 1.
    ENDLOOP.

    IF lt_message_db IS NOT INITIAL.
      IF eo_output IS BOUND.
        eo_output->set_data( ir_data = NEW zpru_if_long_mem_persistence=>tt_message_db( lt_message_db ) ).
      ELSE.
        TRY.
            eo_output ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_PAYLOAD`
                                                                  iv_context = zpru_if_agent_frw=>cs_context-standard ).
          CATCH zpru_cx_agent_core.
            RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
        ENDTRY.
        eo_output->set_data( ir_data = NEW zpru_if_long_mem_persistence=>tt_message_db( lt_message_db ) ).
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD prepare_db_sum.
    DATA lt_summarization_db TYPE zpru_if_long_mem_persistence=>tt_summarization_db.
    DATA lo_util             TYPE REF TO zpru_if_agent_util.

    FIELD-SYMBOLS <lt_summarization> TYPE zpru_if_long_mem_persistence=>tt_summarization.

    IF io_input IS NOT BOUND.
      RETURN.
    ENDIF.

    DATA(lr_data) = io_input->get_data( ).

    ASSIGN lr_data->* TO <lt_summarization>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    GET TIME STAMP FIELD DATA(lv_now).

    TRY.
        lo_util ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_AGENT_UTIL`
                                                            iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    DATA(lv_count) = 1.
    LOOP AT <lt_summarization> ASSIGNING FIELD-SYMBOL(<ls_summarization>).

      APPEND INITIAL LINE TO lt_summarization_db ASSIGNING FIELD-SYMBOL(<ls_summarization_db>).
      <ls_summarization_db> = CORRESPONDING #( <ls_summarization> EXCEPT content ).

      TRY.
          <ls_summarization_db>-summaryuuid = cl_system_uuid=>create_uuid_x16_static( ).
        CATCH cx_uuid_error.
          ASSERT 1 = 2.
      ENDTRY.

      IF <ls_summarization_db>-summarycid IS INITIAL.
        <ls_summarization_db>-summarycid = |{ sy-uname }-{ lv_now }-{ lv_count }|.
      ENDIF.

      IF <ls_summarization_db>-username IS INITIAL.
        <ls_summarization_db>-username = sy-uname.
      ENDIF.

      <ls_summarization_db>-content    = lo_util->serialize_json_2_xstring( <ls_summarization>-content ).

      <ls_summarization_db>-createdby = sy-uname.
      <ls_summarization_db>-createdat = lv_now.
      <ls_summarization_db>-changedby = sy-uname.
      <ls_summarization_db>-changedat = lv_now.

      lv_count += 1.
    ENDLOOP.

    IF lt_summarization_db IS NOT INITIAL.
      IF eo_output IS BOUND.
        eo_output->set_data( ir_data = NEW zpru_if_long_mem_persistence=>tt_summarization_db( lt_summarization_db ) ).
      ELSE.

        TRY.
            eo_output ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_PAYLOAD`
                                                                  iv_context = zpru_if_agent_frw=>cs_context-standard ).
          CATCH zpru_cx_agent_core.
            RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
        ENDTRY.

        eo_output->set_data( ir_data = NEW zpru_if_long_mem_persistence=>tt_summarization_db( lt_summarization_db ) ).
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD zpru_if_long_memory_provider~get_summarization.
    IF mo_summarize IS NOT BOUND.
      TRY.
          mo_summarize ?= zpru_cl_agent_service_mngr=>get_service(
                              iv_service = `ZPRU_IF_SUMMARIZATION`
                              iv_context = zpru_if_agent_frw=>cs_context-st_summarize ).
        CATCH zpru_cx_agent_core.
          RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
      ENDTRY.
    ENDIF.
    ro_summarization = mo_summarize.
  ENDMETHOD.

  METHOD zpru_if_long_memory_provider~set_summarization.
    mo_summarize = io_summarization.
  ENDMETHOD.
ENDCLASS.
