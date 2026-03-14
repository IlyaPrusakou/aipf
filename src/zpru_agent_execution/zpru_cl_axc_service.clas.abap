CLASS zpru_cl_axc_service DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zpru_if_agent_frw.
    INTERFACES zpru_if_axc_service.

  PROTECTED SECTION.
    METHODS precheck_create_header
      IMPORTING it_head_create_imp TYPE zpru_if_axc_type_and_constant=>tt_head_create_imp
      EXPORTING et_entities        TYPE zpru_if_axc_type_and_constant=>tt_head_create_imp
      CHANGING  cs_reported        TYPE zpru_if_agent_frw=>ts_axc_reported
                cs_failed          TYPE zpru_if_agent_frw=>ts_axc_failed.

    METHODS precheck_update_header
      IMPORTING it_head_update_imp TYPE zpru_if_axc_type_and_constant=>tt_head_update_imp
      EXPORTING et_entities        TYPE zpru_if_axc_type_and_constant=>tt_head_update_imp
      CHANGING  cs_reported        TYPE zpru_if_agent_frw=>ts_axc_reported
                cs_failed          TYPE zpru_if_agent_frw=>ts_axc_failed.

    METHODS precheck_delete_header
      IMPORTING it_head_delete_imp TYPE zpru_if_axc_type_and_constant=>tt_header_delete_imp
      EXPORTING et_entities        TYPE zpru_if_axc_type_and_constant=>tt_header_delete_imp
      CHANGING  cs_reported        TYPE zpru_if_agent_frw=>ts_axc_reported
                cs_failed          TYPE zpru_if_agent_frw=>ts_axc_failed.

    METHODS precheck_cba_query
      IMPORTING it_axc_query_imp TYPE zpru_if_axc_type_and_constant=>tt_query_create_imp
      EXPORTING et_entities      TYPE zpru_if_axc_type_and_constant=>tt_query_create_imp
      CHANGING  cs_reported      TYPE zpru_if_agent_frw=>ts_axc_reported
                cs_failed        TYPE zpru_if_agent_frw=>ts_axc_failed.

    METHODS precheck_read_header
      IMPORTING it_head_read_k TYPE zpru_if_axc_type_and_constant=>tt_head_read_k
      EXPORTING et_entities    TYPE zpru_if_axc_type_and_constant=>tt_head_read_k
      CHANGING  cs_reported    TYPE zpru_if_agent_frw=>ts_axc_reported
                cs_failed      TYPE zpru_if_agent_frw=>ts_axc_failed.

    METHODS precheck_read_query
      IMPORTING it_query_read_k TYPE zpru_if_axc_type_and_constant=>tt_query_read_k
      EXPORTING et_entities     TYPE zpru_if_axc_type_and_constant=>tt_query_read_k
      CHANGING  cs_reported     TYPE zpru_if_agent_frw=>ts_axc_reported
                cs_failed       TYPE zpru_if_agent_frw=>ts_axc_failed.

    METHODS precheck_update_query
      IMPORTING it_query_update_imp TYPE zpru_if_axc_type_and_constant=>tt_query_update_imp
      EXPORTING et_entities         TYPE zpru_if_axc_type_and_constant=>tt_query_update_imp
      CHANGING  cs_reported         TYPE zpru_if_agent_frw=>ts_axc_reported
                cs_failed           TYPE zpru_if_agent_frw=>ts_axc_failed.

    METHODS precheck_delete_query
      IMPORTING it_query_delete_imp TYPE zpru_if_axc_type_and_constant=>tt_query_delete_imp
      EXPORTING et_entities         TYPE zpru_if_axc_type_and_constant=>tt_query_delete_imp
      CHANGING  cs_reported         TYPE zpru_if_agent_frw=>ts_axc_reported
                cs_failed           TYPE zpru_if_agent_frw=>ts_axc_failed.

    METHODS precheck_rba_query
      IMPORTING it_rba_query_k TYPE zpru_if_axc_type_and_constant=>tt_rba_query_k
      EXPORTING et_entities    TYPE zpru_if_axc_type_and_constant=>tt_rba_query_k
      CHANGING  cs_reported    TYPE zpru_if_agent_frw=>ts_axc_reported
                cs_failed      TYPE zpru_if_agent_frw=>ts_axc_failed.

    METHODS precheck_cba_step
      IMPORTING it_axc_step_imp TYPE zpru_if_axc_type_and_constant=>tt_step_create_imp
      EXPORTING et_entities     TYPE zpru_if_axc_type_and_constant=>tt_step_create_imp
      CHANGING  cs_reported     TYPE zpru_if_agent_frw=>ts_axc_reported
                cs_failed       TYPE zpru_if_agent_frw=>ts_axc_failed.

    METHODS precheck_rba_step
      IMPORTING it_rba_step_k TYPE zpru_if_axc_type_and_constant=>tt_rba_step_k
      EXPORTING et_entities   TYPE zpru_if_axc_type_and_constant=>tt_rba_step_k
      CHANGING  cs_reported   TYPE zpru_if_agent_frw=>ts_axc_reported
                cs_failed     TYPE zpru_if_agent_frw=>ts_axc_failed.

    METHODS precheck_read_step
      IMPORTING it_step_read_k TYPE zpru_if_axc_type_and_constant=>tt_step_read_k
      EXPORTING et_entities    TYPE zpru_if_axc_type_and_constant=>tt_step_read_k
      CHANGING  cs_reported    TYPE zpru_if_agent_frw=>ts_axc_reported
                cs_failed      TYPE zpru_if_agent_frw=>ts_axc_failed.

    METHODS precheck_update_step
      IMPORTING it_step_update_imp TYPE zpru_if_axc_type_and_constant=>tt_step_update_imp
      EXPORTING et_entities        TYPE zpru_if_axc_type_and_constant=>tt_step_update_imp
      CHANGING  cs_reported        TYPE zpru_if_agent_frw=>ts_axc_reported
                cs_failed          TYPE zpru_if_agent_frw=>ts_axc_failed.

    METHODS precheck_delete_step
      IMPORTING it_step_delete_imp TYPE zpru_if_axc_type_and_constant=>tt_step_delete_imp
      EXPORTING et_entities        TYPE zpru_if_axc_type_and_constant=>tt_step_delete_imp
      CHANGING  cs_reported        TYPE zpru_if_agent_frw=>ts_axc_reported
                cs_failed          TYPE zpru_if_agent_frw=>ts_axc_failed.

    METHODS db_modify
      IMPORTING iv_do_commit    TYPE abap_boolean
      CHANGING  cs_reported     TYPE zpru_if_agent_frw=>ts_axc_reported
                cs_failed       TYPE zpru_if_agent_frw=>ts_axc_failed
                cs_mapped       TYPE zpru_if_agent_frw=>ts_axc_mapped
      RETURNING VALUE(rv_error) TYPE abap_boolean.

ENDCLASS.


CLASS zpru_cl_axc_service IMPLEMENTATION.
  METHOD db_modify.
    " TODO: parameter IV_DO_COMMIT is never used (ABAP cleaner)
    " TODO: parameter CS_REPORTED is never used or assigned (ABAP cleaner)
    " TODO: parameter CS_FAILED is never used or assigned (ABAP cleaner)
    " TODO: parameter CS_MAPPED is never used or assigned (ABAP cleaner)

    " db_modify is kept for compatibility with do_save, but logic is now in CRUD methods via EML.
    rv_error = abap_false.
  ENDMETHOD.

  METHOD zpru_if_axc_service~cba_step.
    DATA ls_reported TYPE zpru_if_agent_frw=>ts_axc_reported.
    DATA ls_failed   TYPE zpru_if_agent_frw=>ts_axc_failed.
    DATA lt_cba_in   TYPE TABLE FOR CREATE zr_pru_axc_head\\executionQuery\_executionstep.

    IF it_axc_step_imp IS INITIAL.
      RETURN.
    ENDIF.

    precheck_cba_step( EXPORTING it_axc_step_imp = it_axc_step_imp
                       IMPORTING et_entities     = DATA(lt_entities)
                       CHANGING  cs_reported     = ls_reported
                                 cs_failed       = ls_failed ).

    cs_failed = CORRESPONDING #( DEEP ls_failed ).
    cs_reported = CORRESPONDING #( DEEP ls_reported ).

    IF    lt_entities    <> it_axc_step_imp
       OR ls_failed-step IS NOT INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_create>).
      APPEND INITIAL LINE TO lt_cba_in ASSIGNING FIELD-SYMBOL(<ls_cba_in>).
      <ls_cba_in>-aipf7queryuuid = <ls_create>-queryuuid.
      APPEND INITIAL LINE TO <ls_cba_in>-%target ASSIGNING FIELD-SYMBOL(<ls_target>).
      <ls_target>-%cid                    = |CID_{ sy-index }_{ <ls_create>-queryuuid }|.
      <ls_target>-aipf7queryuuid          = <ls_create>-queryuuid.
      <ls_target>-aipf7runuuid            = <ls_create>-runuuid.
      <ls_target>-aipf7tooluuid           = COND #( WHEN <ls_create>-control-tooluuid = abap_true
                                                    THEN <ls_create>-tooluuid ).
      <ls_target>-aipf7stepsequence       = COND #( WHEN <ls_create>-control-stepsequence = abap_true
                                                    THEN <ls_create>-stepsequence ).
      <ls_target>-aipf7stepstatus         = COND #( WHEN <ls_create>-control-stepstatus = abap_true
                                                    THEN <ls_create>-stepstatus ).
      <ls_target>-aipf7stepstartdatetime  = COND #( WHEN <ls_create>-control-stepstartdatetime = abap_true
                                                    THEN <ls_create>-stepstartdatetime ).
      <ls_target>-aipf7stependdatetime    = COND #( WHEN <ls_create>-control-stependdatetime = abap_true
                                                    THEN <ls_create>-stependdatetime ).
      <ls_target>-aipf7stepinputprompt    = COND #( WHEN <ls_create>-control-stepinputprompt = abap_true
                                                    THEN <ls_create>-stepinputprompt ).
      <ls_target>-aipf7stepoutputresponse = COND #( WHEN <ls_create>-control-stepoutputresponse = abap_true
                                                    THEN <ls_create>-stepoutputresponse ).

      <ls_target>-%control-aipf7tooluuid           = COND #( WHEN <ls_create>-control-tooluuid = abap_true
                                                             THEN if_abap_behv=>mk-on ).
      <ls_target>-%control-aipf7stepsequence       = COND #( WHEN <ls_create>-control-stepsequence = abap_true
                                                             THEN if_abap_behv=>mk-on ).
      <ls_target>-%control-aipf7stepstatus         = COND #( WHEN <ls_create>-control-stepstatus = abap_true
                                                             THEN if_abap_behv=>mk-on ).
      <ls_target>-%control-aipf7stepstartdatetime  = COND #( WHEN <ls_create>-control-stepstartdatetime = abap_true
                                                             THEN if_abap_behv=>mk-on ).
      <ls_target>-%control-aipf7stependdatetime    = COND #( WHEN <ls_create>-control-stependdatetime = abap_true
                                                             THEN if_abap_behv=>mk-on ).
      <ls_target>-%control-aipf7stepinputprompt    = COND #( WHEN <ls_create>-control-stepinputprompt = abap_true
                                                             THEN if_abap_behv=>mk-on ).
      <ls_target>-%control-aipf7stepoutputresponse = COND #( WHEN <ls_create>-control-stepoutputresponse = abap_true
                                                             THEN if_abap_behv=>mk-on ).
    ENDLOOP.

    MODIFY ENTITIES OF zr_pru_axc_head
           ENTITY executionQuery
           CREATE BY \_executionstep FROM lt_cba_in
           MAPPED DATA(ls_mapped_eml)
           FAILED DATA(ls_failed_eml)
           REPORTED DATA(ls_reported_eml).

    LOOP AT ls_failed_eml-executionstep ASSIGNING FIELD-SYMBOL(<ls_failed_step>).
      APPEND INITIAL LINE TO cs_failed-step ASSIGNING FIELD-SYMBOL(<ls_failed_step_target>).
      <ls_failed_step_target>-stepuuid = <ls_failed_step>-aipf7stepuuid.
      <ls_failed_step_target>-fail     = CONV #( <ls_failed_step>-%fail-cause ).
    ENDLOOP.

    LOOP AT ls_reported_eml-executionstep ASSIGNING FIELD-SYMBOL(<ls_reported_step>).
      APPEND INITIAL LINE TO cs_reported-step ASSIGNING FIELD-SYMBOL(<ls_reported_step_target>).
      <ls_reported_step_target>-stepuuid = <ls_reported_step>-aipf7stepuuid.
*      <ls_reported_step_target>-msg       = <ls_reported_step>-%msg.
    ENDLOOP.

    LOOP AT ls_mapped_eml-executionstep ASSIGNING FIELD-SYMBOL(<ls_mapped_step>).
      APPEND INITIAL LINE TO cs_mapped-step ASSIGNING FIELD-SYMBOL(<ls_mapped_step_target>).
      <ls_mapped_step_target>-stepuuid = <ls_mapped_step>-aipf7stepuuid.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_axc_service~rba_step.
    DATA lt_rba_in TYPE TABLE FOR READ IMPORT zr_pru_axc_head\\executionQuery\_executionstep.

    IF it_rba_step_k IS INITIAL.
      RETURN.
    ENDIF.

    precheck_rba_step( EXPORTING it_rba_step_k = it_rba_step_k
                       IMPORTING et_entities   = DATA(lt_entities)
                       CHANGING  cs_reported   = cs_reported
                                 cs_failed     = cs_failed ).

    IF lt_entities IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_read>).
      APPEND INITIAL LINE TO lt_rba_in ASSIGNING FIELD-SYMBOL(<ls_rba_in>).
      <ls_rba_in>-aipf7queryuuid             = <ls_read>-queryuuid.
      <ls_rba_in>-%control-aipf7stepnumber         = <ls_read>-control-stepnumber.
      <ls_rba_in>-%control-aipf7stepuuid           = <ls_read>-control-stepuuid.
      <ls_rba_in>-%control-aipf7queryuuid          = <ls_read>-control-queryuuid.
      <ls_rba_in>-%control-aipf7runuuid            = <ls_read>-control-runuuid.
      <ls_rba_in>-%control-aipf7tooluuid           = <ls_read>-control-tooluuid.
      <ls_rba_in>-%control-aipf7stepsequence       = <ls_read>-control-stepsequence.
      <ls_rba_in>-%control-aipf7stepstatus         = <ls_read>-control-stepstatus.
      <ls_rba_in>-%control-aipf7stepstartdatetime  = <ls_read>-control-stepstartdatetime.
      <ls_rba_in>-%control-aipf7stependdatetime    = <ls_read>-control-stependdatetime.
      <ls_rba_in>-%control-aipf7stepinputprompt    = <ls_read>-control-stepinputprompt.
      <ls_rba_in>-%control-aipf7stepoutputresponse = <ls_read>-control-stepoutputresponse.
    ENDLOOP.

    READ ENTITIES OF zr_pru_axc_head
         ENTITY executionQuery
         BY \_executionstep
         FROM lt_rba_in
         RESULT DATA(lt_result)
         FAILED DATA(ls_failed_eml)
         REPORTED DATA(ls_reported_eml).

    LOOP AT ls_failed_eml-executionquery ASSIGNING FIELD-SYMBOL(<ls_failed_query>).
      APPEND INITIAL LINE TO cs_failed-query ASSIGNING FIELD-SYMBOL(<ls_failed_query_target>).
      <ls_failed_query_target>-queryuuid = <ls_failed_query>-aipf7queryuuid.
      <ls_failed_query_target>-fail      = CONV #( <ls_failed_query>-%fail-cause ).
    ENDLOOP.

    LOOP AT ls_reported_eml-executionquery ASSIGNING FIELD-SYMBOL(<ls_reported_query>).
      APPEND INITIAL LINE TO cs_reported-query ASSIGNING FIELD-SYMBOL(<ls_reported_query_target>).
      <ls_reported_query_target>-queryuuid = <ls_reported_query>-aipf7queryuuid.
*      <ls_reported_query_target>-msg        = <ls_reported_query>-%msg.
    ENDLOOP.

    LOOP AT lt_result ASSIGNING FIELD-SYMBOL(<ls_res>).
      APPEND INITIAL LINE TO et_axc_step ASSIGNING FIELD-SYMBOL(<ls_out>).
      <ls_out>-stepuuid           = <ls_res>-aipf7stepuuid.
      <ls_out>-queryuuid          = <ls_res>-aipf7queryuuid.
      <ls_out>-runuuid            = <ls_res>-aipf7runuuid.
      <ls_out>-tooluuid           = <ls_res>-aipf7tooluuid.
      <ls_out>-stepnumber         = <ls_res>-aipf7stepnumber.
      <ls_out>-stepsequence       = <ls_res>-aipf7stepsequence.
      <ls_out>-stepstartdatetime  = <ls_res>-aipf7stepstartdatetime.
      <ls_out>-stependdatetime    = <ls_res>-aipf7stependdatetime.
      <ls_out>-stepstatus         = <ls_res>-aipf7stepstatus.
      <ls_out>-stepinputprompt    = <ls_res>-aipf7stepinputprompt.
      <ls_out>-stepoutputresponse = <ls_res>-aipf7stepoutputresponse.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_axc_service~read_step.
    DATA lt_read_in TYPE TABLE FOR READ IMPORT zr_pru_axc_step.

    IF it_step_read_k IS INITIAL.
      RETURN.
    ENDIF.

    precheck_read_step( EXPORTING it_step_read_k = it_step_read_k
                        IMPORTING et_entities    = DATA(lt_entities)
                        CHANGING  cs_reported    = cs_reported
                                  cs_failed      = cs_failed ).

    IF lt_entities IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_read>).
      APPEND INITIAL LINE TO lt_read_in ASSIGNING FIELD-SYMBOL(<ls_read_in>).
      <ls_read_in>-aipf7stepuuid = <ls_read>-stepuuid.
      <ls_read_in>-%control-aipf7runuuid            = <ls_read>-control-runuuid.
      <ls_read_in>-%control-aipf7queryuuid          = <ls_read>-control-queryuuid.
      <ls_read_in>-%control-aipf7stepnumber         = <ls_read>-control-stepnumber.
      <ls_read_in>-%control-aipf7tooluuid           = <ls_read>-control-tooluuid.
      <ls_read_in>-%control-aipf7stepsequence       = <ls_read>-control-stepsequence.
      <ls_read_in>-%control-aipf7stepstatus         = <ls_read>-control-stepstatus.
      <ls_read_in>-%control-aipf7stepstartdatetime  = <ls_read>-control-stepstartdatetime.
      <ls_read_in>-%control-aipf7stependdatetime    = <ls_read>-control-stependdatetime.
      <ls_read_in>-%control-aipf7stepinputprompt    = <ls_read>-control-stepinputprompt.
      <ls_read_in>-%control-aipf7stepoutputresponse = <ls_read>-control-stepoutputresponse.
    ENDLOOP.

    READ ENTITIES OF zr_pru_axc_head
         ENTITY executionstep
         FROM lt_read_in
         RESULT DATA(lt_result)
         FAILED DATA(ls_failed_eml)
         REPORTED DATA(ls_reported_eml).

    LOOP AT ls_failed_eml-executionstep ASSIGNING FIELD-SYMBOL(<ls_failed_step>).
      APPEND INITIAL LINE TO cs_failed-step ASSIGNING FIELD-SYMBOL(<ls_failed_step_target>).
      <ls_failed_step_target>-stepuuid = <ls_failed_step>-aipf7stepuuid.
      <ls_failed_step_target>-fail     = CONV #( <ls_failed_step>-%fail-cause ).
    ENDLOOP.

    LOOP AT ls_reported_eml-executionstep ASSIGNING FIELD-SYMBOL(<ls_reported_step>).
      APPEND INITIAL LINE TO cs_reported-step ASSIGNING FIELD-SYMBOL(<ls_reported_step_target>).
      <ls_reported_step_target>-stepuuid = <ls_reported_step>-aipf7stepuuid.
*      <ls_reported_step_target>-msg       = <ls_reported_step>-%msg.
    ENDLOOP.

    LOOP AT lt_result ASSIGNING FIELD-SYMBOL(<ls_res>).
      APPEND INITIAL LINE TO et_axc_step ASSIGNING FIELD-SYMBOL(<ls_out>).
      <ls_out>-stepuuid           = <ls_res>-aipf7stepuuid.
      <ls_out>-queryuuid          = <ls_res>-aipf7queryuuid.
      <ls_out>-runuuid            = <ls_res>-aipf7runuuid.
      <ls_out>-tooluuid           = <ls_res>-aipf7tooluuid.
      <ls_out>-stepnumber         = <ls_res>-aipf7stepnumber.
      <ls_out>-stepsequence       = <ls_res>-aipf7stepsequence.
      <ls_out>-stepstartdatetime  = <ls_res>-aipf7stepstartdatetime.
      <ls_out>-stependdatetime    = <ls_res>-aipf7stependdatetime.
      <ls_out>-stepstatus         = <ls_res>-aipf7stepstatus.
      <ls_out>-stepinputprompt    = <ls_res>-aipf7stepinputprompt.
      <ls_out>-stepoutputresponse = <ls_res>-aipf7stepoutputresponse.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_axc_service~update_step.
    DATA ls_reported TYPE zpru_if_agent_frw=>ts_axc_reported.
    DATA ls_failed   TYPE zpru_if_agent_frw=>ts_axc_failed.
    DATA lt_update_in TYPE TABLE FOR UPDATE zr_pru_axc_head\\executionStep.

    IF it_step_update_imp IS INITIAL.
      RETURN.
    ENDIF.

    precheck_update_step( EXPORTING it_step_update_imp = it_step_update_imp
                          IMPORTING et_entities        = DATA(lt_entities)
                          CHANGING  cs_reported        = ls_reported
                                    cs_failed          = ls_failed ).

    cs_failed = CORRESPONDING #( DEEP ls_failed ).
    cs_reported = CORRESPONDING #( DEEP ls_reported ).

    IF    lt_entities    <> it_step_update_imp
       OR ls_failed-step IS NOT INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_update>).
      APPEND INITIAL LINE TO lt_update_in ASSIGNING FIELD-SYMBOL(<ls_update_in>).
      <ls_update_in>-aipf7stepuuid        = <ls_update>-stepuuid.
      <ls_update_in>-aipf7tooluuid        = COND #( WHEN <ls_update>-control-tooluuid       = abap_true THEN <ls_update>-tooluuid ).
      <ls_update_in>-aipf7stepsequence    = COND #( WHEN <ls_update>-control-stepsequence     = abap_true THEN <ls_update>-stepsequence  ).
      <ls_update_in>-aipf7stepstatus      = COND #( WHEN <ls_update>-control-stepstatus      = abap_true THEN <ls_update>-stepstatus ).
      <ls_update_in>-aipf7stepstartdatetime  = COND #( WHEN <ls_update>-control-stepstartdatetime  = abap_true THEN <ls_update>-stepstartdatetime ).
      <ls_update_in>-aipf7stependdatetime    = COND #( WHEN <ls_update>-control-stependdatetime    = abap_true THEN <ls_update>-stependdatetime ).
      <ls_update_in>-aipf7stepinputprompt     = COND #( WHEN <ls_update>-control-stepinputprompt     = abap_true THEN <ls_update>-stepinputprompt ).
      <ls_update_in>-aipf7stepoutputresponse    = COND #( WHEN <ls_update>-control-stepoutputresponse    = abap_true THEN <ls_update>-stepoutputresponse ).

      <ls_update_in>-%control-aipf7tooluuid        = COND #( WHEN <ls_update>-control-tooluuid       = abap_true THEN if_abap_behv=>mk-on ).
      <ls_update_in>-%control-aipf7stepsequence    = COND #( WHEN <ls_update>-control-stepsequence    = abap_true THEN if_abap_behv=>mk-on ).
      <ls_update_in>-%control-aipf7stepstatus      = COND #( WHEN <ls_update>-control-stepstatus      = abap_true THEN if_abap_behv=>mk-on ).
      <ls_update_in>-%control-aipf7stepstartdatetime  = COND #( WHEN <ls_update>-control-stepstartdatetime  = abap_true THEN if_abap_behv=>mk-on ).
      <ls_update_in>-%control-aipf7stependdatetime    = COND #( WHEN <ls_update>-control-stependdatetime    = abap_true THEN if_abap_behv=>mk-on ).
      <ls_update_in>-%control-aipf7stepinputprompt     = COND #( WHEN <ls_update>-control-stepinputprompt     = abap_true THEN if_abap_behv=>mk-on ).
      <ls_update_in>-%control-aipf7stepoutputresponse    = COND #( WHEN <ls_update>-control-stepoutputresponse    = abap_true THEN if_abap_behv=>mk-on ).
    ENDLOOP.

    MODIFY ENTITIES OF zr_pru_axc_head
           ENTITY executionStep
           UPDATE FROM lt_update_in
           FAILED DATA(ls_failed_eml)
           REPORTED DATA(ls_reported_eml).

    LOOP AT ls_failed_eml-executionstep ASSIGNING FIELD-SYMBOL(<ls_failed_step>).
      APPEND INITIAL LINE TO cs_failed-step ASSIGNING FIELD-SYMBOL(<ls_failed_step_target>).
      <ls_failed_step_target>-stepuuid = <ls_failed_step>-aipf7stepuuid.
      <ls_failed_step_target>-fail      = CONV #( <ls_failed_step>-%fail-cause ).
    ENDLOOP.

    LOOP AT ls_reported_eml-executionstep ASSIGNING FIELD-SYMBOL(<ls_reported_step>).
      APPEND INITIAL LINE TO cs_reported-step ASSIGNING FIELD-SYMBOL(<ls_reported_step_target>).
      <ls_reported_step_target>-stepuuid = <ls_reported_step>-aipf7stepuuid.
*      <ls_reported_step_target>-msg       = <ls_reported_step>-%msg.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_axc_service~delete_step.
    DATA ls_reported TYPE zpru_if_agent_frw=>ts_axc_reported.
    DATA ls_failed   TYPE zpru_if_agent_frw=>ts_axc_failed.
    DATA lt_delete_in TYPE TABLE FOR DELETE zr_pru_axc_head\\executionstep.

    IF it_step_delete_imp IS INITIAL.
      RETURN.
    ENDIF.

    precheck_delete_step( EXPORTING it_step_delete_imp = it_step_delete_imp
                          IMPORTING et_entities        = DATA(lt_entities)
                          CHANGING  cs_reported        = ls_reported
                                    cs_failed          = ls_failed ).

    cs_failed = CORRESPONDING #( DEEP ls_failed ).
    cs_reported = CORRESPONDING #( DEEP ls_reported ).

    IF    lt_entities    <> it_step_delete_imp
       OR ls_failed-step IS NOT INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_delete>).
      APPEND INITIAL LINE TO lt_delete_in ASSIGNING FIELD-SYMBOL(<ls_delete_in>).
      <ls_delete_in>-aipf7stepuuid = <ls_delete>-stepuuid.
    ENDLOOP.

    MODIFY ENTITIES OF zr_pru_axc_head
           ENTITY executionStep
           DELETE FROM lt_delete_in
           FAILED DATA(ls_failed_eml)
           REPORTED DATA(ls_reported_eml).

    LOOP AT ls_failed_eml-executionstep ASSIGNING FIELD-SYMBOL(<ls_failed_step>).
      APPEND INITIAL LINE TO cs_failed-step ASSIGNING FIELD-SYMBOL(<ls_failed_step_target>).
      <ls_failed_step_target>-stepuuid = <ls_failed_step>-aipf7stepuuid.
      <ls_failed_step_target>-fail      = CONV #( <ls_failed_step>-%fail-cause ).
    ENDLOOP.

    LOOP AT ls_reported_eml-executionstep ASSIGNING FIELD-SYMBOL(<ls_reported_step>).
      APPEND INITIAL LINE TO cs_reported-step ASSIGNING FIELD-SYMBOL(<ls_reported_step_target>).
      <ls_reported_step_target>-stepuuid = <ls_reported_step>-aipf7stepuuid.
*      <ls_reported_step_target>-msg       = <ls_reported_step>-%msg.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_axc_service~determine.
    " Placeholder for business logic that determines what to persist.
    " Currently a no-op; callers populate buffers via CBA/UPDATE/DELETE flows.
    IF cs_mapped IS INITIAL.
      " leave as-is
    ENDIF.
  ENDMETHOD.

  METHOD zpru_if_axc_service~validate.
    " Placeholder for validation logic. Add domain-specific checks here.
    " For now, we keep this minimal: if there are existing failures, leave them.
    IF cs_failed-header IS NOT INITIAL OR cs_failed-query IS NOT INITIAL OR cs_failed-step IS NOT INITIAL.
      " existing failures already present
    ENDIF.
  ENDMETHOD.

  METHOD zpru_if_axc_service~clean_up.
    " Clear in-memory buffers and mapped entries after save/rollback.
    CLEAR cs_mapped.
    CLEAR: zpru_cl_axc_buffer=>header_buffer,
           zpru_cl_axc_buffer=>query_buffer,
           zpru_cl_axc_buffer=>step_buffer.
  ENDMETHOD.

  METHOD zpru_if_axc_service~do_save.
    DATA(lv_err) = abap_false.

    " Run determination step
    me->zpru_if_axc_service~determine( CHANGING cs_reported = cs_reported
                                                cs_failed   = cs_failed
                                                cs_mapped   = cs_mapped ).

    " Run validation
    me->zpru_if_axc_service~validate( CHANGING cs_reported = cs_reported
                                               cs_failed   = cs_failed ).

    " If validation produced failures, skip DB modification
    IF cs_failed-header IS NOT INITIAL OR cs_failed-query IS NOT INITIAL OR cs_failed-step IS NOT INITIAL.
      RETURN.
    ENDIF.

    " Persist buffers to DB (returns abap_true on error)
    lv_err = db_modify( EXPORTING iv_do_commit = iv_do_commit
                        CHANGING  cs_reported  = cs_reported
                                  cs_failed    = cs_failed
                                  cs_mapped    = cs_mapped ).

    IF lv_err = abap_true.
      IF iv_do_commit = abap_true.
        ROLLBACK WORK.
      ENDIF.
      " leave failure info in cs_failed
      me->zpru_if_axc_service~clean_up( CHANGING cs_mapped = cs_mapped ).
      RETURN.
    ENDIF.

    IF iv_do_commit = abap_true.
      COMMIT WORK AND WAIT.
    ENDIF.

    me->zpru_if_axc_service~clean_up( CHANGING cs_mapped = cs_mapped ).
  ENDMETHOD.

  METHOD zpru_if_axc_service~get_actual_query.
    CLEAR et_axc_head_query_link.

    IF it_axc_head_k IS INITIAL.
      RETURN.
    ENDIF.

    zpru_if_axc_service~rba_query(
      EXPORTING it_rba_query_k = VALUE #( FOR <ls_h> IN it_axc_head_k
                                          ( runuuid = <ls_h>-runuuid
                                            control = VALUE #( runuuid             = abap_true
                                                               querynumber         = abap_true
                                                               queryuuid           = abap_true
                                                               querylanguage       = abap_true
                                                               querystatus         = abap_true
                                                               querystartdatetime  = abap_true
                                                               queryenddatetime    = abap_true
                                                               queryinputprompt    = abap_true
                                                               querydecisionlog    = abap_true
                                                               queryoutputresponse = abap_true )  ) )
      IMPORTING et_axc_query   = DATA(lt_query_candidates)
      CHANGING  cs_reported    = cs_reported
                cs_failed      = cs_failed ).

    LOOP AT it_axc_head_k ASSIGNING FIELD-SYMBOL(<ls_axc_head_k>).

      DATA(lt_query_copy) = lt_query_candidates.
      DELETE lt_query_copy WHERE runuuid <> <ls_axc_head_k>-runuuid.
      DELETE lt_query_copy WHERE querystatus <> zpru_if_axc_type_and_constant=>sc_query_status-new.

      SORT lt_query_copy BY querystartdatetime ASCENDING.

      APPEND INITIAL LINE TO et_axc_head_query_link ASSIGNING FIELD-SYMBOL(<ls_axc_head_query_link>).
      <ls_axc_head_query_link>-runuuid   = <ls_axc_head_k>-runuuid.
      <ls_axc_head_query_link>-queryuuid = VALUE #( lt_query_copy[ 1 ]-queryuuid OPTIONAL ).

    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_axc_service~cba_query.
    DATA ls_reported TYPE zpru_if_agent_frw=>ts_axc_reported.
    DATA ls_failed   TYPE zpru_if_agent_frw=>ts_axc_failed.
    DATA lt_cba_in   TYPE TABLE FOR CREATE zr_pru_axc_head\_executionquery.

    IF it_axc_query_imp IS INITIAL.
      RETURN.
    ENDIF.

    precheck_cba_query( EXPORTING it_axc_query_imp = it_axc_query_imp
                        IMPORTING et_entities      = DATA(lt_entities)
                        CHANGING  cs_reported      = ls_reported
                                  cs_failed        = ls_failed ).

    cs_failed = CORRESPONDING #( DEEP ls_failed ).
    cs_reported = CORRESPONDING #( DEEP ls_reported ).

    IF    lt_entities     <> it_axc_query_imp
       OR ls_failed-query IS NOT INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_create>).
      APPEND INITIAL LINE TO lt_cba_in ASSIGNING FIELD-SYMBOL(<ls_cba_in>).
      <ls_cba_in>-aipf7runuuid = <ls_create>-runuuid.
      APPEND INITIAL LINE TO <ls_cba_in>-%target ASSIGNING FIELD-SYMBOL(<ls_target>).
      <ls_target>-%cid                     = |CID_{ sy-index }_{ <ls_create>-runuuid }|.
      <ls_target>-aipf7runuuid             = <ls_create>-runuuid.
      <ls_target>-aipf7querylanguage       = COND #( WHEN <ls_create>-control-querylanguage = abap_true
                                                     THEN <ls_create>-querylanguage ).
      <ls_target>-aipf7querystatus         = COND #( WHEN <ls_create>-control-querystatus = abap_true
                                                     THEN <ls_create>-querystatus ).
      <ls_target>-aipf7querystartdatetime  = COND #( WHEN <ls_create>-control-querystartdatetime = abap_true
                                                     THEN <ls_create>-querystartdatetime ).
      <ls_target>-aipf7queryenddatetime    = COND #( WHEN <ls_create>-control-queryenddatetime = abap_true
                                                     THEN <ls_create>-queryenddatetime ).
      <ls_target>-aipf7queryinputprompt    = COND #( WHEN <ls_create>-control-queryinputprompt = abap_true
                                                     THEN <ls_create>-queryinputprompt ).
      <ls_target>-aipf7querydecisionlog    = COND #( WHEN <ls_create>-control-querydecisionlog = abap_true
                                                     THEN <ls_create>-querydecisionlog ).
      <ls_target>-aipf7queryoutputresponse = COND #( WHEN <ls_create>-control-queryoutputresponse = abap_true
                                                     THEN <ls_create>-queryoutputresponse ).

      <ls_target>-%control-aipf7querylanguage       = COND #( WHEN <ls_create>-control-querylanguage = abap_true
                                                              THEN if_abap_behv=>mk-on ).
      <ls_target>-%control-aipf7querystatus         = COND #( WHEN <ls_create>-control-querystatus = abap_true
                                                              THEN if_abap_behv=>mk-on ).
      <ls_target>-%control-aipf7querystartdatetime  = COND #( WHEN <ls_create>-control-querystartdatetime = abap_true
                                                              THEN if_abap_behv=>mk-on ).
      <ls_target>-%control-aipf7queryenddatetime    = COND #( WHEN <ls_create>-control-queryenddatetime = abap_true
                                                              THEN if_abap_behv=>mk-on ).
      <ls_target>-%control-aipf7queryinputprompt    = COND #( WHEN <ls_create>-control-queryinputprompt = abap_true
                                                              THEN if_abap_behv=>mk-on ).
      <ls_target>-%control-aipf7querydecisionlog    = COND #( WHEN <ls_create>-control-querydecisionlog = abap_true
                                                              THEN if_abap_behv=>mk-on ).
      <ls_target>-%control-aipf7queryoutputresponse = COND #( WHEN <ls_create>-control-queryoutputresponse = abap_true
                                                              THEN if_abap_behv=>mk-on ).
    ENDLOOP.

    MODIFY ENTITIES OF zr_pru_axc_head
           ENTITY executionheader
           CREATE BY \_executionquery FROM lt_cba_in
           MAPPED DATA(ls_mapped_eml)
           FAILED DATA(ls_failed_eml)
           REPORTED DATA(ls_reported_eml).

    LOOP AT ls_failed_eml-executionquery ASSIGNING FIELD-SYMBOL(<ls_failed_query>).
      APPEND INITIAL LINE TO cs_failed-query ASSIGNING FIELD-SYMBOL(<ls_failed_query_target>).
      <ls_failed_query_target>-queryuuid = <ls_failed_query>-aipf7queryuuid.
      <ls_failed_query_target>-fail      = CONV #( <ls_failed_query>-%fail-cause ).
    ENDLOOP.

    LOOP AT ls_reported_eml-executionquery ASSIGNING FIELD-SYMBOL(<ls_reported_query>).
      APPEND INITIAL LINE TO cs_reported-query ASSIGNING FIELD-SYMBOL(<ls_reported_query_target>).
      <ls_reported_query_target>-queryuuid = <ls_reported_query>-aipf7queryuuid.
*      <ls_reported_query_target>-msg        = <ls_reported_query>-%msg.
    ENDLOOP.

    LOOP AT ls_mapped_eml-executionquery ASSIGNING FIELD-SYMBOL(<ls_mapped_query>).
      APPEND INITIAL LINE TO cs_mapped-query ASSIGNING FIELD-SYMBOL(<ls_mapped_query_target>).
      <ls_mapped_query_target>-queryuuid = <ls_mapped_query>-aipf7queryuuid.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_axc_service~lock.
  ENDMETHOD.

  METHOD zpru_if_axc_service~rba_query.
    DATA lt_rba_in TYPE TABLE FOR READ IMPORT zr_pru_axc_head\_executionquery.

    IF it_rba_query_k IS INITIAL.
      RETURN.
    ENDIF.

    precheck_rba_query( EXPORTING it_rba_query_k = it_rba_query_k
                        IMPORTING et_entities    = DATA(lt_entities)
                        CHANGING  cs_reported    = cs_reported
                                  cs_failed      = cs_failed ).

    IF lt_entities IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_read>).
      APPEND INITIAL LINE TO lt_rba_in ASSIGNING FIELD-SYMBOL(<ls_rba_in>).
      <ls_rba_in>-aipf7runuuid = <ls_read>-runuuid.
      <ls_rba_in>-%control-aipf7runuuid             = <ls_read>-control-runuuid.
      <ls_rba_in>-%control-aipf7querynumber         = <ls_read>-control-querynumber.
      <ls_rba_in>-%control-aipf7queryuuid           = <ls_read>-control-queryuuid.
      <ls_rba_in>-%control-aipf7querylanguage       = <ls_read>-control-querylanguage.
      <ls_rba_in>-%control-aipf7querystatus         = <ls_read>-control-querystatus.
      <ls_rba_in>-%control-aipf7querystartdatetime  = <ls_read>-control-querystartdatetime.
      <ls_rba_in>-%control-aipf7queryenddatetime    = <ls_read>-control-queryenddatetime.
      <ls_rba_in>-%control-aipf7queryinputprompt    = <ls_read>-control-queryinputprompt.
      <ls_rba_in>-%control-aipf7querydecisionlog    = <ls_read>-control-querydecisionlog.
      <ls_rba_in>-%control-aipf7queryoutputresponse = <ls_read>-control-queryoutputresponse.
    ENDLOOP.

    READ ENTITIES OF zr_pru_axc_head
         ENTITY executionheader
         BY \_executionquery
         FROM lt_rba_in
         RESULT DATA(lt_result)
         FAILED DATA(ls_failed_eml)
         REPORTED DATA(ls_reported_eml).

    LOOP AT ls_failed_eml-executionheader ASSIGNING FIELD-SYMBOL(<ls_failed_header>).
      APPEND INITIAL LINE TO cs_failed-header ASSIGNING FIELD-SYMBOL(<ls_failed_header_target>).
      <ls_failed_header_target>-runuuid = <ls_failed_header>-aipf7runuuid.
      <ls_failed_header_target>-fail    = CONV #( <ls_failed_header>-%fail-cause ).
    ENDLOOP.

    LOOP AT ls_reported_eml-executionheader ASSIGNING FIELD-SYMBOL(<ls_reported_header>).
      APPEND INITIAL LINE TO cs_reported-header ASSIGNING FIELD-SYMBOL(<ls_reported_header_target>).
      <ls_reported_header_target>-runuuid = <ls_reported_header>-aipf7runuuid.
*      <ls_reported_header_target>-msg      = <ls_reported_header>-%msg.
    ENDLOOP.

    LOOP AT lt_result ASSIGNING FIELD-SYMBOL(<ls_res>).
      APPEND INITIAL LINE TO et_axc_query ASSIGNING FIELD-SYMBOL(<ls_out>).
      <ls_out>-queryuuid           = <ls_res>-aipf7queryuuid.
      <ls_out>-runuuid             = <ls_res>-aipf7runuuid.
      <ls_out>-querynumber         = <ls_res>-aipf7querynumber.
      <ls_out>-querylanguage       = <ls_res>-aipf7querylanguage.
      <ls_out>-querystatus         = <ls_res>-aipf7querystatus.
      <ls_out>-querystartdatetime  = <ls_res>-aipf7querystartdatetime.
      <ls_out>-queryenddatetime    = <ls_res>-aipf7queryenddatetime.
      <ls_out>-queryinputprompt    = <ls_res>-aipf7queryinputprompt.
      <ls_out>-querydecisionlog    = <ls_res>-aipf7querydecisionlog.
      <ls_out>-queryoutputresponse = <ls_res>-aipf7queryoutputresponse.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_axc_service~read_query.
    DATA lt_read_in TYPE TABLE FOR READ IMPORT zr_pru_axc_query.

    IF it_query_read_k IS INITIAL.
      RETURN.
    ENDIF.

    precheck_read_query( EXPORTING it_query_read_k = it_query_read_k
                         IMPORTING et_entities     = DATA(lt_entities)
                         CHANGING  cs_reported     = cs_reported
                                   cs_failed       = cs_failed ).

    IF lt_entities IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_read>).
      APPEND INITIAL LINE TO lt_read_in ASSIGNING FIELD-SYMBOL(<ls_read_in>).
      <ls_read_in>-aipf7queryuuid = <ls_read>-queryuuid.
      <ls_read_in>-%control-aipf7runuuid             = <ls_read>-control-runuuid.
      <ls_read_in>-%control-aipf7querynumber         = <ls_read>-control-querynumber.
      <ls_read_in>-%control-aipf7querylanguage       = <ls_read>-control-querylanguage.
      <ls_read_in>-%control-aipf7querystatus         = <ls_read>-control-querystatus.
      <ls_read_in>-%control-aipf7querystartdatetime  = <ls_read>-control-querystartdatetime.
      <ls_read_in>-%control-aipf7queryenddatetime    = <ls_read>-control-queryenddatetime.
      <ls_read_in>-%control-aipf7queryinputprompt    = <ls_read>-control-queryinputprompt.
      <ls_read_in>-%control-aipf7querydecisionlog    = <ls_read>-control-querydecisionlog.
      <ls_read_in>-%control-aipf7queryoutputresponse = <ls_read>-control-queryoutputresponse.
    ENDLOOP.

    READ ENTITIES OF zr_pru_axc_head
         ENTITY executionquery
         FROM lt_read_in
         RESULT DATA(lt_result)
         FAILED DATA(ls_failed_eml)
         REPORTED DATA(ls_reported_eml).

    LOOP AT ls_failed_eml-executionquery ASSIGNING FIELD-SYMBOL(<ls_failed_query>).
      APPEND INITIAL LINE TO cs_failed-query ASSIGNING FIELD-SYMBOL(<ls_failed_query_target>).
      <ls_failed_query_target>-queryuuid = <ls_failed_query>-aipf7queryuuid.
      <ls_failed_query_target>-fail      = CONV #( <ls_failed_query>-%fail-cause ).
    ENDLOOP.

    LOOP AT ls_reported_eml-executionquery ASSIGNING FIELD-SYMBOL(<ls_reported_query>).
      APPEND INITIAL LINE TO cs_reported-query ASSIGNING FIELD-SYMBOL(<ls_reported_query_target>).
      <ls_reported_query_target>-queryuuid = <ls_reported_query>-aipf7queryuuid.
*      <ls_reported_query_target>-msg        = <ls_reported_query>-%msg.
    ENDLOOP.

    LOOP AT lt_result ASSIGNING FIELD-SYMBOL(<ls_res>).
      APPEND INITIAL LINE TO et_axc_query ASSIGNING FIELD-SYMBOL(<ls_out>).
      <ls_out>-queryuuid           = <ls_res>-aipf7queryuuid.
      <ls_out>-runuuid             = <ls_res>-aipf7runuuid.
      <ls_out>-querynumber         = <ls_res>-aipf7querynumber.
      <ls_out>-querylanguage       = <ls_res>-aipf7querylanguage.
      <ls_out>-querystatus         = <ls_res>-aipf7querystatus.
      <ls_out>-querystartdatetime  = <ls_res>-aipf7querystartdatetime.
      <ls_out>-queryenddatetime    = <ls_res>-aipf7queryenddatetime.
      <ls_out>-queryinputprompt    = <ls_res>-aipf7queryinputprompt.
      <ls_out>-querydecisionlog    = <ls_res>-aipf7querydecisionlog.
      <ls_out>-queryoutputresponse = <ls_res>-aipf7queryoutputresponse.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_axc_service~update_query.
    DATA ls_reported  TYPE zpru_if_agent_frw=>ts_axc_reported.
    DATA ls_failed    TYPE zpru_if_agent_frw=>ts_axc_failed.
    DATA lt_update_in TYPE TABLE FOR UPDATE zr_pru_axc_head\\executionquery.

    IF it_query_update_imp IS INITIAL.
      RETURN.
    ENDIF.

    precheck_update_query( EXPORTING it_query_update_imp = it_query_update_imp
                           IMPORTING et_entities         = DATA(lt_entities)
                           CHANGING  cs_reported         = ls_reported
                                     cs_failed           = ls_failed ).

    cs_failed = CORRESPONDING #( DEEP ls_failed ).
    cs_reported = CORRESPONDING #( DEEP ls_reported ).

    IF    lt_entities     <> it_query_update_imp
       OR ls_failed-query IS NOT INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_update>).
      APPEND INITIAL LINE TO lt_update_in ASSIGNING FIELD-SYMBOL(<ls_update_in>).
      <ls_update_in>-aipf7queryuuid           = <ls_update>-queryuuid.
      <ls_update_in>-aipf7runuuid             = <ls_update>-runuuid.
      <ls_update_in>-aipf7querylanguage       = COND #( WHEN <ls_update>-control-querylanguage = abap_true
                                                        THEN <ls_update>-querylanguage ).
      <ls_update_in>-aipf7querystatus         = COND #( WHEN <ls_update>-control-querystatus = abap_true
                                                        THEN <ls_update>-querystatus ).
      <ls_update_in>-aipf7querystartdatetime  = COND #( WHEN <ls_update>-control-querystartdatetime = abap_true
                                                        THEN <ls_update>-querystartdatetime ).
      <ls_update_in>-aipf7queryenddatetime    = COND #( WHEN <ls_update>-control-queryenddatetime = abap_true
                                                        THEN <ls_update>-queryenddatetime ).
      <ls_update_in>-aipf7queryinputprompt    = COND #( WHEN <ls_update>-control-queryinputprompt = abap_true
                                                        THEN <ls_update>-queryinputprompt ).
      <ls_update_in>-aipf7querydecisionlog    = COND #( WHEN <ls_update>-control-querydecisionlog = abap_true
                                                        THEN <ls_update>-querydecisionlog ).
      <ls_update_in>-aipf7queryoutputresponse = COND #( WHEN <ls_update>-control-queryoutputresponse = abap_true
                                                        THEN <ls_update>-queryoutputresponse ).

      <ls_update_in>-%control-aipf7querylanguage       = COND #( WHEN <ls_update>-control-querylanguage = abap_true
                                                                 THEN if_abap_behv=>mk-on ).
      <ls_update_in>-%control-aipf7querystatus         = COND #( WHEN <ls_update>-control-querystatus = abap_true
                                                                 THEN if_abap_behv=>mk-on ).
      <ls_update_in>-%control-aipf7querystartdatetime  = COND #( WHEN <ls_update>-control-querystartdatetime = abap_true
                                                                 THEN if_abap_behv=>mk-on ).
      <ls_update_in>-%control-aipf7queryenddatetime    = COND #( WHEN <ls_update>-control-queryenddatetime = abap_true
                                                                 THEN if_abap_behv=>mk-on ).
      <ls_update_in>-%control-aipf7queryinputprompt    = COND #( WHEN <ls_update>-control-queryinputprompt = abap_true
                                                                 THEN if_abap_behv=>mk-on ).
      <ls_update_in>-%control-aipf7querydecisionlog    = COND #( WHEN <ls_update>-control-querydecisionlog = abap_true
                                                                 THEN if_abap_behv=>mk-on ).
      <ls_update_in>-%control-aipf7queryoutputresponse = COND #( WHEN <ls_update>-control-queryoutputresponse = abap_true
                                                                 THEN if_abap_behv=>mk-on ).
    ENDLOOP.

    MODIFY ENTITIES OF zr_pru_axc_head
           ENTITY executionquery
           UPDATE FROM lt_update_in
           FAILED DATA(ls_failed_eml)
           REPORTED DATA(ls_reported_eml).

    LOOP AT ls_failed_eml-executionquery ASSIGNING FIELD-SYMBOL(<ls_failed_query>).
      APPEND INITIAL LINE TO cs_failed-query ASSIGNING FIELD-SYMBOL(<ls_failed_query_target>).
      <ls_failed_query_target>-queryuuid = <ls_failed_query>-aipf7queryuuid.
      <ls_failed_query_target>-fail      = CONV #( <ls_failed_query>-%fail-cause ).
    ENDLOOP.

    LOOP AT ls_reported_eml-executionquery ASSIGNING FIELD-SYMBOL(<ls_reported_query>).
      APPEND INITIAL LINE TO cs_reported-query ASSIGNING FIELD-SYMBOL(<ls_reported_query_target>).
      <ls_reported_query_target>-queryuuid = <ls_reported_query>-aipf7queryuuid.
*      <ls_reported_query_target>-msg        = <ls_reported_query>-%msg.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_axc_service~delete_query.
    DATA ls_reported  TYPE zpru_if_agent_frw=>ts_axc_reported.
    DATA ls_failed    TYPE zpru_if_agent_frw=>ts_axc_failed.
    DATA lt_delete_in TYPE TABLE FOR DELETE zr_pru_axc_head\\executionquery.

    IF it_query_delete_imp IS INITIAL.
      RETURN.
    ENDIF.

    precheck_delete_query( EXPORTING it_query_delete_imp = it_query_delete_imp
                           IMPORTING et_entities         = DATA(lt_entities)
                           CHANGING  cs_reported         = ls_reported
                                     cs_failed           = ls_failed ).

    cs_failed = CORRESPONDING #( DEEP ls_failed ).
    cs_reported = CORRESPONDING #( DEEP ls_reported ).

    IF    lt_entities     <> it_query_delete_imp
       OR ls_failed-query IS NOT INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_delete>).
      APPEND INITIAL LINE TO lt_delete_in ASSIGNING FIELD-SYMBOL(<ls_delete_in>).
      <ls_delete_in>-aipf7queryuuid = <ls_delete>-queryuuid.
    ENDLOOP.

    MODIFY ENTITIES OF zr_pru_axc_head
           ENTITY executionquery
           DELETE FROM lt_delete_in
           FAILED DATA(ls_failed_eml)
           REPORTED DATA(ls_reported_eml).

    LOOP AT ls_failed_eml-executionquery ASSIGNING FIELD-SYMBOL(<ls_failed_query>).
      APPEND INITIAL LINE TO cs_failed-query ASSIGNING FIELD-SYMBOL(<ls_failed_query_target>).
      <ls_failed_query_target>-queryuuid = <ls_failed_query>-aipf7queryuuid.
      <ls_failed_query_target>-fail      = CONV #( <ls_failed_query>-%fail-cause ).
    ENDLOOP.

    LOOP AT ls_reported_eml-executionquery ASSIGNING FIELD-SYMBOL(<ls_reported_query>).
      APPEND INITIAL LINE TO cs_reported-query ASSIGNING FIELD-SYMBOL(<ls_reported_query_target>).
      <ls_reported_query_target>-queryuuid = <ls_reported_query>-aipf7queryuuid.
*      <ls_reported_query_target>-msg        = <ls_reported_query>-%msg.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_axc_service~read_header.
    DATA lt_read_in TYPE TABLE FOR READ IMPORT zr_pru_axc_head.

    CLEAR et_axc_head.

    IF it_head_read_k IS INITIAL.
      RETURN.
    ENDIF.

    precheck_read_header( EXPORTING it_head_read_k = it_head_read_k
                          IMPORTING et_entities    = DATA(lt_entities)
                          CHANGING  cs_reported    = cs_reported
                                    cs_failed      = cs_failed ).

    IF lt_entities IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_read>).
      APPEND INITIAL LINE TO lt_read_in ASSIGNING FIELD-SYMBOL(<ls_read_in>).
      <ls_read_in>-aipf7runuuid = <ls_read>-runuuid.
      <ls_read_in>-%control-aipf7runid            = <ls_read>-control-runid.
      <ls_read_in>-%control-aipf7agentuuid        = <ls_read>-control-agentuuid.
      <ls_read_in>-%control-aipf7userid           = <ls_read>-control-userid.
      <ls_read_in>-%control-aipf7runstartdatetime = <ls_read>-control-runstartdatetime.
      <ls_read_in>-%control-aipf7runenddatetime   = <ls_read>-control-runenddatetime.
      <ls_read_in>-%control-aipf7createdby        = <ls_read>-control-createdby.
      <ls_read_in>-%control-aipf7createdat        = <ls_read>-control-createdat.
      <ls_read_in>-%control-aipf7changedby        = <ls_read>-control-changedby.
      <ls_read_in>-%control-aipf7lastchanged      = <ls_read>-control-lastchanged.
      <ls_read_in>-%control-aipf7locallastchanged = <ls_read>-control-locallastchanged.
    ENDLOOP.

    READ ENTITIES OF zr_pru_axc_head
         ENTITY executionheader
         FROM lt_read_in
         RESULT DATA(lt_result)
         FAILED DATA(ls_failed_eml)
         REPORTED DATA(ls_reported_eml).

    LOOP AT ls_failed_eml-executionheader ASSIGNING FIELD-SYMBOL(<ls_failed_header>).
      APPEND INITIAL LINE TO cs_failed-header ASSIGNING FIELD-SYMBOL(<ls_failed_header_target>).
      <ls_failed_header_target>-runuuid = <ls_failed_header>-aipf7runuuid.
      <ls_failed_header_target>-fail    = CONV #( <ls_failed_header>-%fail-cause ).
    ENDLOOP.

    LOOP AT ls_reported_eml-executionheader ASSIGNING FIELD-SYMBOL(<ls_reported_header>).
      APPEND INITIAL LINE TO cs_reported-header ASSIGNING FIELD-SYMBOL(<ls_reported_header_target>).
      <ls_reported_header_target>-runuuid = <ls_reported_header>-aipf7runuuid.
*      <ls_reported_header_target>-msg      = <ls_reported_header>-%msg.
    ENDLOOP.

    LOOP AT lt_result ASSIGNING FIELD-SYMBOL(<ls_res>).
      APPEND INITIAL LINE TO et_axc_head ASSIGNING FIELD-SYMBOL(<ls_out>).
      <ls_out>-runuuid          = <ls_res>-aipf7runuuid.
      <ls_out>-runid            = <ls_res>-aipf7runid.
      <ls_out>-agentuuid        = <ls_res>-aipf7agentuuid.
      <ls_out>-userid           = <ls_res>-aipf7userid.
      <ls_out>-runstartdatetime = <ls_res>-aipf7runstartdatetime.
      <ls_out>-runenddatetime   = <ls_res>-aipf7runenddatetime.
      <ls_out>-createdby        = <ls_res>-aipf7createdby.
      <ls_out>-createdat        = <ls_res>-aipf7createdat.
      <ls_out>-changedby        = <ls_res>-aipf7changedby.
      <ls_out>-lastchanged      = <ls_res>-aipf7lastchanged.
      <ls_out>-locallastchanged = <ls_res>-aipf7locallastchanged.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_axc_service~create_header.
    DATA ls_reported  TYPE zpru_if_agent_frw=>ts_axc_reported.
    DATA ls_failed    TYPE zpru_if_agent_frw=>ts_axc_failed.
    DATA lt_create_in TYPE TABLE FOR CREATE zr_pru_axc_head.

    precheck_create_header( EXPORTING it_head_create_imp = it_head_create_imp
                            IMPORTING et_entities        = DATA(lt_entities)
                            CHANGING  cs_reported        = ls_reported
                                      cs_failed          = ls_failed ).

    cs_failed = CORRESPONDING #( DEEP ls_failed ).
    cs_reported = CORRESPONDING #( DEEP ls_reported ).

    IF    lt_entities      <> it_head_create_imp
       OR ls_failed-header IS NOT INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_create>).
      APPEND INITIAL LINE TO lt_create_in ASSIGNING FIELD-SYMBOL(<ls_create_in>).
      <ls_create_in>-aipf7agentuuid        = COND #( WHEN <ls_create>-control-agentuuid = abap_true
                                                     THEN <ls_create>-agentuuid ).
      <ls_create_in>-aipf7userid           = COND #( WHEN <ls_create>-control-userid = abap_true THEN <ls_create>-userid ).
      <ls_create_in>-aipf7runstartdatetime = COND #( WHEN <ls_create>-control-runstartdatetime = abap_true
                                                     THEN <ls_create>-runstartdatetime ).
      <ls_create_in>-aipf7runenddatetime   = COND #( WHEN <ls_create>-control-runenddatetime = abap_true
                                                     THEN <ls_create>-runenddatetime ).
      <ls_create_in>-aipf7createdby        = COND #( WHEN <ls_create>-control-createdby = abap_true
                                                     THEN <ls_create>-createdby ).
      <ls_create_in>-aipf7createdat        = COND #( WHEN <ls_create>-control-createdat = abap_true
                                                     THEN <ls_create>-createdat ).
      <ls_create_in>-aipf7changedby        = COND #( WHEN <ls_create>-control-changedby = abap_true
                                                     THEN <ls_create>-changedby ).
      <ls_create_in>-aipf7lastchanged      = COND #( WHEN <ls_create>-control-lastchanged = abap_true
                                                     THEN <ls_create>-lastchanged ).
      <ls_create_in>-aipf7locallastchanged = COND #( WHEN <ls_create>-control-locallastchanged = abap_true
                                                     THEN <ls_create>-locallastchanged ).

      <ls_create_in>-%control-aipf7agentuuid        = COND #( WHEN <ls_create>-control-agentuuid = abap_true
                                                              THEN if_abap_behv=>mk-on ).
      <ls_create_in>-%control-aipf7userid           = COND #( WHEN <ls_create>-control-userid = abap_true
                                                              THEN if_abap_behv=>mk-on ).
      <ls_create_in>-%control-aipf7runstartdatetime = COND #( WHEN <ls_create>-control-runstartdatetime = abap_true
                                                              THEN if_abap_behv=>mk-on ).
      <ls_create_in>-%control-aipf7runenddatetime   = COND #( WHEN <ls_create>-control-runenddatetime = abap_true
                                                              THEN if_abap_behv=>mk-on ).
      <ls_create_in>-%control-aipf7createdby        = COND #( WHEN <ls_create>-control-createdby = abap_true
                                                              THEN if_abap_behv=>mk-on ).
      <ls_create_in>-%control-aipf7createdat        = COND #( WHEN <ls_create>-control-createdat = abap_true
                                                              THEN if_abap_behv=>mk-on ).
      <ls_create_in>-%control-aipf7changedby        = COND #( WHEN <ls_create>-control-changedby = abap_true
                                                              THEN if_abap_behv=>mk-on ).
    ENDLOOP.

    MODIFY ENTITIES OF zr_pru_axc_head
           ENTITY executionheader
           CREATE AUTO FILL CID WITH lt_create_in
           MAPPED DATA(ls_mapped_eml)
           FAILED DATA(ls_failed_eml)
           REPORTED DATA(ls_reported_eml).

    LOOP AT ls_failed_eml-executionheader ASSIGNING FIELD-SYMBOL(<ls_failed_header>).
      APPEND INITIAL LINE TO cs_failed-header ASSIGNING FIELD-SYMBOL(<ls_failed_target>).
      <ls_failed_target>-runuuid = <ls_failed_header>-aipf7runuuid.
      <ls_failed_target>-fail    = CONV #( <ls_failed_header>-%fail-cause ).
    ENDLOOP.

    LOOP AT ls_reported_eml-executionheader ASSIGNING FIELD-SYMBOL(<ls_reported_header>).
      APPEND INITIAL LINE TO cs_reported-header ASSIGNING FIELD-SYMBOL(<ls_reported_header_target>).
      <ls_reported_header_target>-runuuid = <ls_reported_header>-aipf7runuuid.
*      <ls_reported_header_target>-msg      = <ls_reported_header>-%msg.
    ENDLOOP.

    LOOP AT ls_mapped_eml-executionheader ASSIGNING FIELD-SYMBOL(<ls_mapped_header>).
      APPEND INITIAL LINE TO cs_mapped-header ASSIGNING FIELD-SYMBOL(<ls_mapped_header_target>).
      <ls_mapped_header_target>-runuuid = <ls_mapped_header>-aipf7runuuid.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_axc_service~update_header.
    DATA ls_reported  TYPE zpru_if_agent_frw=>ts_axc_reported.
    DATA ls_failed    TYPE zpru_if_agent_frw=>ts_axc_failed.
    DATA lt_update_in TYPE TABLE FOR UPDATE zr_pru_axc_head.

    precheck_update_header( EXPORTING it_head_update_imp = it_head_update_imp
                            IMPORTING et_entities        = DATA(lt_entities)
                            CHANGING  cs_reported        = ls_reported
                                      cs_failed          = ls_failed ).

    cs_failed = CORRESPONDING #( DEEP ls_failed ).
    cs_reported = CORRESPONDING #( DEEP ls_reported ).

    IF    lt_entities      <> it_head_update_imp
       OR ls_failed-header IS NOT INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_update>).
      APPEND INITIAL LINE TO lt_update_in ASSIGNING FIELD-SYMBOL(<ls_update_in>).
      <ls_update_in>-aipf7runuuid          = <ls_update>-runuuid.
      <ls_update_in>-aipf7agentuuid        = COND #( WHEN <ls_update>-control-agentuuid = abap_true
                                                     THEN <ls_update>-agentuuid ).
      <ls_update_in>-aipf7userid           = COND #( WHEN <ls_update>-control-userid = abap_true THEN <ls_update>-userid ).
      <ls_update_in>-aipf7runstartdatetime = COND #( WHEN <ls_update>-control-runstartdatetime = abap_true
                                                     THEN <ls_update>-runstartdatetime ).
      <ls_update_in>-aipf7runenddatetime   = COND #( WHEN <ls_update>-control-runenddatetime = abap_true
                                                     THEN <ls_update>-runenddatetime ).
      <ls_update_in>-aipf7createdby        = COND #( WHEN <ls_update>-control-createdby = abap_true
                                                     THEN <ls_update>-createdby ).
      <ls_update_in>-aipf7createdat        = COND #( WHEN <ls_update>-control-createdat = abap_true
                                                     THEN <ls_update>-createdat ).
      <ls_update_in>-aipf7changedby        = COND #( WHEN <ls_update>-control-changedby = abap_true
                                                     THEN <ls_update>-changedby ).

      <ls_update_in>-%control-aipf7agentuuid        = COND #( WHEN <ls_update>-control-agentuuid = abap_true
                                                              THEN if_abap_behv=>mk-on ).
      <ls_update_in>-%control-aipf7userid           = COND #( WHEN <ls_update>-control-userid = abap_true
                                                              THEN if_abap_behv=>mk-on ).
      <ls_update_in>-%control-aipf7runstartdatetime = COND #( WHEN <ls_update>-control-runstartdatetime = abap_true
                                                              THEN if_abap_behv=>mk-on ).
      <ls_update_in>-%control-aipf7runenddatetime   = COND #( WHEN <ls_update>-control-runenddatetime = abap_true
                                                              THEN if_abap_behv=>mk-on ).
      <ls_update_in>-%control-aipf7createdby        = COND #( WHEN <ls_update>-control-createdby = abap_true
                                                              THEN if_abap_behv=>mk-on ).
      <ls_update_in>-%control-aipf7createdat        = COND #( WHEN <ls_update>-control-createdat = abap_true
                                                              THEN if_abap_behv=>mk-on ).
      <ls_update_in>-%control-aipf7changedby        = COND #( WHEN <ls_update>-control-changedby = abap_true
                                                              THEN if_abap_behv=>mk-on ).

    ENDLOOP.

    MODIFY ENTITIES OF zr_pru_axc_head
           ENTITY executionheader
           UPDATE FROM lt_update_in
           FAILED DATA(ls_failed_eml)
           REPORTED DATA(ls_reported_eml).

    LOOP AT ls_failed_eml-executionheader ASSIGNING FIELD-SYMBOL(<ls_failed_header>).
      APPEND INITIAL LINE TO cs_failed-header ASSIGNING FIELD-SYMBOL(<ls_failed_header_target>).
      <ls_failed_header_target>-runuuid = <ls_failed_header>-aipf7runuuid.
      <ls_failed_header_target>-fail    = CONV #( <ls_failed_header>-%fail-cause ).
    ENDLOOP.

    LOOP AT ls_reported_eml-executionheader ASSIGNING FIELD-SYMBOL(<ls_reported_header>).
      APPEND INITIAL LINE TO cs_reported-header ASSIGNING FIELD-SYMBOL(<ls_reported_header_target>).
      <ls_reported_header_target>-runuuid = <ls_reported_header>-aipf7runuuid.
*      <ls_reported_header_target>-msg      = <ls_reported_header>-%msg.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_axc_service~delete_header.
    DATA ls_reported  TYPE zpru_if_agent_frw=>ts_axc_reported.
    DATA ls_failed    TYPE zpru_if_agent_frw=>ts_axc_failed.
    DATA lt_delete_in TYPE TABLE FOR DELETE zr_pru_axc_head.

    precheck_delete_header( EXPORTING it_head_delete_imp = it_head_delete_imp
                            IMPORTING et_entities        = DATA(lt_entities)
                            CHANGING  cs_reported        = ls_reported
                                      cs_failed          = ls_failed ).

    cs_failed = CORRESPONDING #( DEEP ls_failed ).
    cs_reported = CORRESPONDING #( DEEP ls_reported ).

    IF    lt_entities      <> it_head_delete_imp
       OR ls_failed-header IS NOT INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_delete>).
      APPEND INITIAL LINE TO lt_delete_in ASSIGNING FIELD-SYMBOL(<ls_delete_in>).
      <ls_delete_in>-aipf7runuuid = <ls_delete>-runuuid.
    ENDLOOP.

    MODIFY ENTITIES OF zr_pru_axc_head
           ENTITY executionheader
           DELETE FROM lt_delete_in
           FAILED DATA(ls_failed_eml)
           REPORTED DATA(ls_reported_eml).

    LOOP AT ls_failed_eml-executionheader ASSIGNING FIELD-SYMBOL(<ls_failed_header>).
      APPEND INITIAL LINE TO cs_failed-header ASSIGNING FIELD-SYMBOL(<ls_failed_header_target>).
      <ls_failed_header_target>-runuuid = <ls_failed_header>-aipf7runuuid.
      <ls_failed_header_target>-fail    = CONV #( <ls_failed_header>-%fail-cause ).
    ENDLOOP.

    LOOP AT ls_reported_eml-executionheader ASSIGNING FIELD-SYMBOL(<ls_reported_header>).
      APPEND INITIAL LINE TO cs_reported-header ASSIGNING FIELD-SYMBOL(<ls_reported_header_target>).
      <ls_reported_header_target>-runuuid = <ls_reported_header>-aipf7runuuid.
*      <ls_reported_header_target>-msg      = <ls_reported_header>-%msg.
    ENDLOOP.
  ENDMETHOD.

  METHOD precheck_update_header.
    DATA lo_pre TYPE REF TO zpru_if_axc_precheck.

    TRY.
        lo_pre ?= zpru_cl_agent_service_mngr=>get_service(
                      iv_service = `ZPRU_IF_AXC_PRECHECK`
                      iv_context = zpru_if_agent_frw=>cs_context-st_agent_execution ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    lo_pre->precheck_update_header( EXPORTING it_head_update_imp = it_head_update_imp
                                    IMPORTING et_entities        = et_entities
                                    CHANGING  cs_reported        = cs_reported
                                              cs_failed          = cs_failed ).
  ENDMETHOD.

  METHOD precheck_delete_header.
    DATA lo_pre TYPE REF TO zpru_if_axc_precheck.

    TRY.
        lo_pre ?= zpru_cl_agent_service_mngr=>get_service(
                      iv_service = `ZPRU_IF_AXC_PRECHECK`
                      iv_context = zpru_if_agent_frw=>cs_context-st_agent_execution ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    lo_pre->precheck_delete_header( EXPORTING it_head_delete_imp = it_head_delete_imp
                                    IMPORTING et_entities        = et_entities
                                    CHANGING  cs_reported        = cs_reported
                                              cs_failed          = cs_failed ).
  ENDMETHOD.

  METHOD precheck_create_header.
    DATA lo_pre TYPE REF TO zpru_if_axc_precheck.

    TRY.
        lo_pre ?= zpru_cl_agent_service_mngr=>get_service(
                      iv_service = `ZPRU_IF_AXC_PRECHECK`
                      iv_context = zpru_if_agent_frw=>cs_context-st_agent_execution ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    lo_pre->precheck_create_header( EXPORTING it_head_create_imp = it_head_create_imp
                                    IMPORTING et_entities        = et_entities
                                    CHANGING  cs_reported        = cs_reported
                                              cs_failed          = cs_failed ).
  ENDMETHOD.

  METHOD precheck_cba_query.
    DATA lo_pre TYPE REF TO zpru_if_axc_precheck.

    TRY.
        lo_pre ?= zpru_cl_agent_service_mngr=>get_service(
                      iv_service = `ZPRU_IF_AXC_PRECHECK`
                      iv_context = zpru_if_agent_frw=>cs_context-st_agent_execution ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    lo_pre->precheck_cba_query( EXPORTING it_axc_query_imp = it_axc_query_imp
                                IMPORTING et_entities      = et_entities
                                CHANGING  cs_reported      = cs_reported
                                          cs_failed        = cs_failed ).
  ENDMETHOD.

  METHOD precheck_read_header.
    DATA lo_pre TYPE REF TO zpru_if_axc_precheck.

    TRY.
        lo_pre ?= zpru_cl_agent_service_mngr=>get_service(
                      iv_service = `ZPRU_IF_AXC_PRECHECK`
                      iv_context = zpru_if_agent_frw=>cs_context-st_agent_execution ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    lo_pre->precheck_read_header( EXPORTING it_head_read_k = it_head_read_k
                                  IMPORTING et_entities    = et_entities
                                  CHANGING  cs_reported    = cs_reported
                                            cs_failed      = cs_failed ).
  ENDMETHOD.

  METHOD precheck_rba_query.
    DATA lo_pre TYPE REF TO zpru_if_axc_precheck.

    TRY.
        lo_pre ?= zpru_cl_agent_service_mngr=>get_service(
                      iv_service = `ZPRU_IF_AXC_PRECHECK`
                      iv_context = zpru_if_agent_frw=>cs_context-st_agent_execution ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    lo_pre->precheck_rba_query( EXPORTING it_rba_query_k = it_rba_query_k
                                IMPORTING et_entities    = et_entities
                                CHANGING  cs_reported    = cs_reported
                                          cs_failed      = cs_failed ).
  ENDMETHOD.

  METHOD precheck_read_query.
    DATA lo_pre TYPE REF TO zpru_if_axc_precheck.

    TRY.
        lo_pre ?= zpru_cl_agent_service_mngr=>get_service(
                      iv_service = `ZPRU_IF_AXC_PRECHECK`
                      iv_context = zpru_if_agent_frw=>cs_context-st_agent_execution ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    lo_pre->precheck_read_query( EXPORTING it_query_read_k = it_query_read_k
                                 IMPORTING et_entities     = et_entities
                                 CHANGING  cs_reported     = cs_reported
                                           cs_failed       = cs_failed ).
  ENDMETHOD.

  METHOD precheck_update_query.
    DATA lo_pre TYPE REF TO zpru_if_axc_precheck.

    TRY.
        lo_pre ?= zpru_cl_agent_service_mngr=>get_service(
                      iv_service = `ZPRU_IF_AXC_PRECHECK`
                      iv_context = zpru_if_agent_frw=>cs_context-st_agent_execution ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    lo_pre->precheck_update_query( EXPORTING it_query_update_imp = it_query_update_imp
                                   IMPORTING et_entities         = et_entities
                                   CHANGING  cs_reported         = cs_reported
                                             cs_failed           = cs_failed ).
  ENDMETHOD.

  METHOD precheck_delete_query.
    DATA lo_pre TYPE REF TO zpru_if_axc_precheck.

    TRY.
        lo_pre ?= zpru_cl_agent_service_mngr=>get_service(
                      iv_service = `ZPRU_IF_AXC_PRECHECK`
                      iv_context = zpru_if_agent_frw=>cs_context-st_agent_execution ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    lo_pre->precheck_delete_query( EXPORTING it_query_delete_imp = it_query_delete_imp
                                   IMPORTING et_entities         = et_entities
                                   CHANGING  cs_reported         = cs_reported
                                             cs_failed           = cs_failed ).
  ENDMETHOD.

  METHOD precheck_cba_step.
    DATA lo_pre TYPE REF TO zpru_if_axc_precheck.

    TRY.
        lo_pre ?= zpru_cl_agent_service_mngr=>get_service(
                      iv_service = `ZPRU_IF_AXC_PRECHECK`
                      iv_context = zpru_if_agent_frw=>cs_context-st_agent_execution ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    lo_pre->precheck_cba_step( EXPORTING it_axc_step_imp = it_axc_step_imp
                               IMPORTING et_entities     = et_entities
                               CHANGING  cs_reported     = cs_reported
                                         cs_failed       = cs_failed ).
  ENDMETHOD.

  METHOD precheck_rba_step.
    DATA lo_pre TYPE REF TO zpru_if_axc_precheck.

    TRY.
        lo_pre ?= zpru_cl_agent_service_mngr=>get_service(
                      iv_service = `ZPRU_IF_AXC_PRECHECK`
                      iv_context = zpru_if_agent_frw=>cs_context-st_agent_execution ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    lo_pre->precheck_rba_step( EXPORTING it_rba_step_k = it_rba_step_k
                               IMPORTING et_entities   = et_entities
                               CHANGING  cs_reported   = cs_reported
                                         cs_failed     = cs_failed ).
  ENDMETHOD.

  METHOD precheck_read_step.
    DATA lo_pre TYPE REF TO zpru_if_axc_precheck.

    TRY.
        lo_pre ?= zpru_cl_agent_service_mngr=>get_service(
                      iv_service = `ZPRU_IF_AXC_PRECHECK`
                      iv_context = zpru_if_agent_frw=>cs_context-st_agent_execution ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    lo_pre->precheck_read_step( EXPORTING it_step_read_k = it_step_read_k
                                IMPORTING et_entities    = et_entities
                                CHANGING  cs_reported    = cs_reported
                                          cs_failed      = cs_failed ).
  ENDMETHOD.

  METHOD precheck_update_step.
    DATA lo_pre TYPE REF TO zpru_if_axc_precheck.

    TRY.
        lo_pre ?= zpru_cl_agent_service_mngr=>get_service(
                      iv_service = `ZPRU_IF_AXC_PRECHECK`
                      iv_context = zpru_if_agent_frw=>cs_context-st_agent_execution ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    lo_pre->precheck_update_step( EXPORTING it_step_update_imp = it_step_update_imp
                                  IMPORTING et_entities        = et_entities
                                  CHANGING  cs_reported        = cs_reported
                                            cs_failed          = cs_failed ).
  ENDMETHOD.

  METHOD precheck_delete_step.
    DATA lo_pre TYPE REF TO zpru_if_axc_precheck.

    TRY.
        lo_pre ?= zpru_cl_agent_service_mngr=>get_service(
                      iv_service = `ZPRU_IF_AXC_PRECHECK`
                      iv_context = zpru_if_agent_frw=>cs_context-st_agent_execution ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    lo_pre->precheck_delete_step( EXPORTING it_step_delete_imp = it_step_delete_imp
                                  IMPORTING et_entities        = et_entities
                                  CHANGING  cs_reported        = cs_reported
                                            cs_failed          = cs_failed ).
  ENDMETHOD.
ENDCLASS.
