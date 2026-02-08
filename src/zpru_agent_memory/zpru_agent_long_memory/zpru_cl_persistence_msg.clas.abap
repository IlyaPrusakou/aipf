CLASS zpru_cl_persistence_msg DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zpru_if_agent_frw.
    INTERFACES zpru_if_long_mem_persistence.
ENDCLASS.


CLASS zpru_cl_persistence_msg IMPLEMENTATION.
  METHOD zpru_if_long_mem_persistence~persist.
    DATA lt_message_db TYPE zpru_if_long_mem_persistence=>tt_message_db.
    DATA lo_mmsg_service TYPE REF TO zpru_if_mmsg_service.
    DATA ls_failed TYPE zpru_if_agent_frw=>ts_mmsg_bndl_failed.

    IF io_input IS NOT BOUND.
      RETURN.
    ENDIF.

    ev_error = abap_false.

    lt_message_db = io_input->get_data( )->*.

    TRY.
        lo_mmsg_service ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_MMSG_SERVICE`
                                                              iv_context = zpru_if_agent_frw=>cs_context-st_persistence_message ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    lo_mmsg_service->create_mmsg( EXPORTING it_mmsg_create_imp = VALUE #( FOR <ls_in> IN lt_message_db
                                                                          ( messageuuid         = <ls_in>-messageuuid
                                                                            content             = <ls_in>-content
                                                                            messagetype         = <ls_in>-messagetype
                                                                            messagecid          = <ls_in>-messagecid
                                                                            stage               = <ls_in>-stage
                                                                            substage            = <ls_in>-substage
                                                                            namespace           = <ls_in>-namespace
                                                                            username            = <ls_in>-username
                                                                            agentuuid           = <ls_in>-agentuuid
                                                                            runuuid             = <ls_in>-runuuid
                                                                            queryuuid           = <ls_in>-queryuuid
                                                                            stepuuid            = <ls_in>-stepuuid
                                                                            messagetime         = <ls_in>-messagetime
                                                                            createdby           = <ls_in>-createdby
                                                                            createdat           = <ls_in>-createdat
                                                                            changedby           = <ls_in>-changedby
                                                                            changedat           = <ls_in>-changedat
                                                                            control-messageuuid = abap_true
                                                                            control-content     = abap_true
                                                                            control-messagetype = abap_true
                                                                            control-messagecid  = abap_true
                                                                            control-stage       = abap_true
                                                                            control-substage    = abap_true
                                                                            control-namespace   = abap_true
                                                                            control-username    = abap_true
                                                                            control-agentuuid   = abap_true
                                                                            control-runuuid     = abap_true
                                                                            control-queryuuid   = abap_true
                                                                            control-stepuuid    = abap_true
                                                                            control-messagetime = abap_true
                                                                            control-createdby   = abap_true
                                                                            control-createdat   = abap_true
                                                                            control-changedby   = abap_true
                                                                            control-changedat   = abap_true ) )
                                  CHANGING  cs_failed          = ls_failed ).

    IF ls_failed-mmsg IS NOT INITIAL.
      ev_error = abap_true.
    ELSE.
      ev_error = abap_false.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
