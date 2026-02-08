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
*    DATA ls_reported TYPE zpru_if_agent_frw=>ts_axc_reported.
*    DATA ls_failed   TYPE zpru_if_agent_frw=>ts_axc_failed.
*    DATA lt_cba_in   TYPE TABLE FOR CREATE zr_pru_axc_head\\executionQuery\_executionstep.
*
*    IF it_axc_step_imp IS INITIAL.
*      RETURN.
*    ENDIF.
*
*    precheck_cba_step( EXPORTING it_axc_step_imp = it_axc_step_imp
*                       IMPORTING et_entities     = DATA(lt_entities)
*                       CHANGING  cs_reported     = ls_reported
*                                 cs_failed       = ls_failed ).
*
*    cs_failed = CORRESPONDING #( DEEP ls_failed ).
*    cs_reported = CORRESPONDING #( DEEP ls_reported ).
*
*    IF    lt_entities    <> it_axc_step_imp
*       OR ls_failed-step IS NOT INITIAL.
*      RETURN.
*    ENDIF.
*
*    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_create>).
*      APPEND INITIAL LINE TO lt_cba_in ASSIGNING FIELD-SYMBOL(<ls_cba_in>).
*      <ls_cba_in>-runuuid = <ls_create>-run_uuid.
*      APPEND INITIAL LINE TO <ls_cba_in>-%target ASSIGNING FIELD-SYMBOL(<ls_target>).
*      <ls_target>-%cid            = |CID_{ sy-index }_{ <ls_create>-step_uuid }|.
*      <ls_target>-stepuuid        = <ls_create>-step_uuid.
*      <ls_target>-queryuuid       = <ls_create>-query_uuid.
*      <ls_target>-runuuid         = <ls_create>-run_uuid.
*      <ls_target>-stepnumber      = COND #( WHEN <ls_create>-control-step_number     = abap_true THEN <ls_create>-step_number ).
*      <ls_target>-tooluuid        = COND #( WHEN <ls_create>-control-tool_uuid      = abap_true THEN <ls_create>-tool_uuid ).
*      <ls_target>-executionseq    = COND #( WHEN <ls_create>-control-execution_seq    = abap_true THEN <ls_create>-execution_seq ).
*      <ls_target>-stepstatus      = COND #( WHEN <ls_create>-control-step_status      = abap_true THEN <ls_create>-step_status ).
*      <ls_target>-starttimestamp  = COND #( WHEN <ls_create>-control-start_timestamp  = abap_true THEN <ls_create>-start_timestamp ).
*      <ls_target>-endtimestamp    = COND #( WHEN <ls_create>-control-end_timestamp    = abap_true THEN <ls_create>-end_timestamp ).
*      <ls_target>-inputprompt     = COND #( WHEN <ls_create>-control-input_prompt     = abap_true THEN <ls_create>-input_prompt ).
*      <ls_target>-outputprompt    = COND #( WHEN <ls_create>-control-output_prompt    = abap_true THEN <ls_create>-output_prompt ).
*
*      <ls_target>-%control-stepnumber      = COND #( WHEN <ls_create>-control-step_number     = abap_true THEN if_abap_behv=>mk-on ).
*      <ls_target>-%control-tooluuid        = COND #( WHEN <ls_create>-control-tool_uuid      = abap_true THEN if_abap_behv=>mk-on ).
*      <ls_target>-%control-executionseq    = COND #( WHEN <ls_create>-control-execution_seq    = abap_true THEN if_abap_behv=>mk-on ).
*      <ls_target>-%control-stepstatus      = COND #( WHEN <ls_create>-control-step_status      = abap_true THEN if_abap_behv=>mk-on ).
*      <ls_target>-%control-starttimestamp  = COND #( WHEN <ls_create>-control-start_timestamp  = abap_true THEN if_abap_behv=>mk-on ).
*      <ls_target>-%control-endtimestamp    = COND #( WHEN <ls_create>-control-end_timestamp    = abap_true THEN if_abap_behv=>mk-on ).
*      <ls_target>-%control-inputprompt     = COND #( WHEN <ls_create>-control-input_prompt     = abap_true THEN if_abap_behv=>mk-on ).
*      <ls_target>-%control-outputprompt    = COND #( WHEN <ls_create>-control-output_prompt    = abap_true THEN if_abap_behv=>mk-on ).
*    ENDLOOP.
*
*    MODIFY ENTITIES OF zr_pru_axc_head
*           ENTITY executionHeader
*           CREATE BY \_executionstep FROM lt_cba_in
*           MAPPED DATA(ls_mapped_eml)
*           FAILED DATA(ls_failed_eml)
*           REPORTED DATA(ls_reported_eml).
*
*    LOOP AT ls_failed_eml-executionstep ASSIGNING FIELD-SYMBOL(<ls_failed_step>).
*      APPEND INITIAL LINE TO cs_failed-step ASSIGNING FIELD-SYMBOL(<ls_failed_step_target>).
*      <ls_failed_step_target>-step_uuid = <ls_failed_step>-stepuuid.
*      <ls_failed_step_target>-fail      = CONV #( <ls_failed_step>-%fail-cause ).
*    ENDLOOP.
*
*    LOOP AT ls_reported_eml-executionstep ASSIGNING FIELD-SYMBOL(<ls_reported_step>).
*      APPEND INITIAL LINE TO cs_reported-step ASSIGNING FIELD-SYMBOL(<ls_reported_step_target>).
*      <ls_reported_step_target>-step_uuid = <ls_reported_step>-stepuuid.
**      <ls_reported_step_target>-msg       = <ls_reported_step>-%msg.
*    ENDLOOP.
*
*    LOOP AT ls_mapped_eml-executionstep ASSIGNING FIELD-SYMBOL(<ls_mapped_step>).
*      APPEND INITIAL LINE TO cs_mapped-step ASSIGNING FIELD-SYMBOL(<ls_mapped_step_target>).
*      <ls_mapped_step_target>-step_uuid = <ls_mapped_step>-stepuuid.
*    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_axc_service~rba_step.
*    DATA lt_rba_in TYPE TABLE FOR READ BY \_executionstep zr_pru_axc_head\_executionquery.
*
*    IF it_rba_step_k IS INITIAL.
*      RETURN.
*    ENDIF.
*
*    precheck_rba_step( EXPORTING it_rba_step_k = it_rba_step_k
*                       IMPORTING et_entities   = DATA(lt_entities)
*                       CHANGING  cs_reported   = cs_reported
*                                 cs_failed     = cs_failed ).
*
*    IF lt_entities IS INITIAL.
*      RETURN.
*    ENDIF.
*
*    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_read>).
*      APPEND INITIAL LINE TO lt_rba_in ASSIGNING FIELD-SYMBOL(<ls_rba_in>).
*      <ls_rba_in>-runuuid         = <ls_read>-run_uuid.
*      <ls_rba_in>-queryuuid       = <ls_read>-query_uuid.
*      <ls_rba_in>-%control-stepnumber      = <ls_read>-control-step_number.
*      <ls_rba_in>-%control-stepuuid        = <ls_read>-control-step_uuid.
*      <ls_rba_in>-%control-queryuuid       = <ls_read>-control-query_uuid.
*      <ls_rba_in>-%control-runuuid         = <ls_read>-control-run_uuid.
*      <ls_rba_in>-%control-tooluuid        = <ls_read>-control-tool_uuid.
*      <ls_rba_in>-%control-executionseq    = <ls_read>-control-execution_seq.
*      <ls_rba_in>-%control-stepstatus      = <ls_read>-control-step_status.
*      <ls_rba_in>-%control-starttimestamp  = <ls_read>-control-start_timestamp.
*      <ls_rba_in>-%control-endtimestamp    = <ls_read>-control-end_timestamp.
*      <ls_rba_in>-%control-inputprompt     = <ls_read>-control-input_prompt.
*      <ls_rba_in>-%control-outputprompt    = <ls_read>-control-output_prompt.
*    ENDLOOP.
*
*    READ ENTITIES OF zr_pru_axc_head
*         ENTITY executionQuery
*         BY \_executionstep
*         FROM lt_rba_in
*         RESULT DATA(lt_result)
*         FAILED DATA(ls_failed_eml)
*         REPORTED DATA(ls_reported_eml).
*
*    LOOP AT ls_failed_eml-executionquery ASSIGNING FIELD-SYMBOL(<ls_failed_query>).
*      APPEND INITIAL LINE TO cs_failed-query ASSIGNING FIELD-SYMBOL(<ls_failed_query_target>).
*      <ls_failed_query_target>-query_uuid = <ls_failed_query>-queryuuid.
*      <ls_failed_query_target>-fail       = CONV #( <ls_failed_query>-%fail-cause ).
*    ENDLOOP.
*
*    LOOP AT ls_reported_eml-executionquery ASSIGNING FIELD-SYMBOL(<ls_reported_query>).
*      APPEND INITIAL LINE TO cs_reported-query ASSIGNING FIELD-SYMBOL(<ls_reported_query_target>).
*      <ls_reported_query_target>-query_uuid = <ls_reported_query>-queryuuid.
**      <ls_reported_query_target>-msg        = <ls_reported_query>-%msg.
*    ENDLOOP.
*
*    LOOP AT lt_result ASSIGNING FIELD-SYMBOL(<ls_res>).
*      APPEND INITIAL LINE TO et_axc_step ASSIGNING FIELD-SYMBOL(<ls_out>).
*      <ls_out>-step_uuid       = <ls_res>-stepuuid.
*      <ls_out>-query_uuid      = <ls_res>-queryuuid.
*      <ls_out>-run_uuid        = <ls_res>-runuuid.
*      <ls_out>-tool_uuid       = <ls_res>-tooluuid.
*      <ls_out>-step_number     = <ls_res>-stepnumber.
*      <ls_out>-execution_seq   = <ls_res>-executionseq.
*      <ls_out>-start_timestamp = <ls_res>-starttimestamp.
*      <ls_out>-end_timestamp   = <ls_res>-endtimestamp.
*      <ls_out>-step_status     = <ls_res>-stepstatus.
*      <ls_out>-input_prompt    = <ls_res>-inputprompt.
*      <ls_out>-output_prompt   = <ls_res>-outputprompt.
*    ENDLOOP.
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
      <ls_read_in>-stepuuid = <ls_read>-stepuuid.
      <ls_read_in>-%control-runuuid        = <ls_read>-control-runuuid.
      <ls_read_in>-%control-queryuuid      = <ls_read>-control-queryuuid.
      <ls_read_in>-%control-stepnumber     = <ls_read>-control-stepnumber.
      <ls_read_in>-%control-tooluuid       = <ls_read>-control-tooluuid.
      <ls_read_in>-%control-executionseq   = <ls_read>-control-executionseq.
      <ls_read_in>-%control-stepstatus     = <ls_read>-control-stepstatus.
      <ls_read_in>-%control-starttimestamp = <ls_read>-control-starttimestamp.
      <ls_read_in>-%control-endtimestamp   = <ls_read>-control-endtimestamp.
      <ls_read_in>-%control-inputprompt    = <ls_read>-control-inputprompt.
      <ls_read_in>-%control-outputprompt   = <ls_read>-control-outputprompt.
    ENDLOOP.

    READ ENTITIES OF zr_pru_axc_head
         ENTITY executionstep
         FROM lt_read_in
         RESULT DATA(lt_result)
         FAILED DATA(ls_failed_eml)
         REPORTED DATA(ls_reported_eml).

    LOOP AT ls_failed_eml-executionstep ASSIGNING FIELD-SYMBOL(<ls_failed_step>).
      APPEND INITIAL LINE TO cs_failed-step ASSIGNING FIELD-SYMBOL(<ls_failed_step_target>).
      <ls_failed_step_target>-stepuuid = <ls_failed_step>-stepuuid.
      <ls_failed_step_target>-fail     = CONV #( <ls_failed_step>-%fail-cause ).
    ENDLOOP.

    LOOP AT ls_reported_eml-executionstep ASSIGNING FIELD-SYMBOL(<ls_reported_step>).
      APPEND INITIAL LINE TO cs_reported-step ASSIGNING FIELD-SYMBOL(<ls_reported_step_target>).
      <ls_reported_step_target>-stepuuid = <ls_reported_step>-stepuuid.
*      <ls_reported_step_target>-msg       = <ls_reported_step>-%msg.
    ENDLOOP.

    LOOP AT lt_result ASSIGNING FIELD-SYMBOL(<ls_res>).
      APPEND INITIAL LINE TO et_axc_step ASSIGNING FIELD-SYMBOL(<ls_out>).
      <ls_out>-stepuuid       = <ls_res>-stepuuid.
      <ls_out>-queryuuid      = <ls_res>-queryuuid.
      <ls_out>-runuuid        = <ls_res>-runuuid.
      <ls_out>-tooluuid       = <ls_res>-tooluuid.
      <ls_out>-stepnumber     = <ls_res>-stepnumber.
      <ls_out>-executionseq   = <ls_res>-executionseq.
      <ls_out>-starttimestamp = <ls_res>-starttimestamp.
      <ls_out>-endtimestamp   = <ls_res>-endtimestamp.
      <ls_out>-stepstatus     = <ls_res>-stepstatus.
      <ls_out>-inputprompt    = <ls_res>-inputprompt.
      <ls_out>-outputprompt   = <ls_res>-outputprompt.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_axc_service~update_step.
*    DATA ls_reported TYPE zpru_if_agent_frw=>ts_axc_reported.
*    DATA ls_failed   TYPE zpru_if_agent_frw=>ts_axc_failed.
*    DATA lt_update_in TYPE TABLE FOR UPDATE zr_pru_axc_head\_executionstep.
*
*    IF it_step_update_imp IS INITIAL.
*      RETURN.
*    ENDIF.
*
*    precheck_update_step( EXPORTING it_step_update_imp = it_step_update_imp
*                          IMPORTING et_entities        = DATA(lt_entities)
*                          CHANGING  cs_reported        = ls_reported
*                                    cs_failed          = ls_failed ).
*
*    cs_failed = CORRESPONDING #( DEEP ls_failed ).
*    cs_reported = CORRESPONDING #( DEEP ls_reported ).
*
*    IF    lt_entities    <> it_step_update_imp
*       OR ls_failed-step IS NOT INITIAL.
*      RETURN.
*    ENDIF.
*
*    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_update>).
*      APPEND INITIAL LINE TO lt_update_in ASSIGNING FIELD-SYMBOL(<ls_update_in>).
*      <ls_update_in>-stepuuid        = <ls_update>-step_uuid.
*      <ls_update_in>-queryuuid       = <ls_update>-query_uuid.
*      <ls_update_in>-runuuid         = <ls_update>-run_uuid.
*      <ls_update_in>-stepnumber      = COND #( WHEN <ls_update>-control-step_number     = abap_true THEN <ls_update>-step_number ).
*      <ls_update_in>-tooluuid        = COND #( WHEN <ls_update>-control-tool_uuid       = abap_true THEN <ls_update>-tool_uuid ).
*      <ls_update_in>-executionseq    = COND #( WHEN <ls_update>-control-execution_seq    = abap_true THEN <ls_update>-execution_seq ).
*      <ls_update_in>-stepstatus      = COND #( WHEN <ls_update>-control-step_status      = abap_true THEN <ls_update>-step_status ).
*      <ls_update_in>-starttimestamp  = COND #( WHEN <ls_update>-control-start_timestamp  = abap_true THEN <ls_update>-start_timestamp ).
*      <ls_update_in>-endtimestamp    = COND #( WHEN <ls_update>-control-end_timestamp    = abap_true THEN <ls_update>-end_timestamp ).
*      <ls_update_in>-inputprompt     = COND #( WHEN <ls_update>-control-input_prompt     = abap_true THEN <ls_update>-input_prompt ).
*      <ls_update_in>-outputprompt    = COND #( WHEN <ls_update>-control-output_prompt    = abap_true THEN <ls_update>-output_prompt ).
*
*      <ls_update_in>-%control-stepnumber      = COND #( WHEN <ls_update>-control-step_number     = abap_true THEN if_abap_behv=>mk-on ).
*      <ls_update_in>-%control-tooluuid        = COND #( WHEN <ls_update>-control-tool_uuid       = abap_true THEN if_abap_behv=>mk-on ).
*      <ls_update_in>-%control-executionseq    = COND #( WHEN <ls_update>-control-execution_seq    = abap_true THEN if_abap_behv=>mk-on ).
*      <ls_update_in>-%control-stepstatus      = COND #( WHEN <ls_update>-control-step_status      = abap_true THEN if_abap_behv=>mk-on ).
*      <ls_update_in>-%control-starttimestamp  = COND #( WHEN <ls_update>-control-start_timestamp  = abap_true THEN if_abap_behv=>mk-on ).
*      <ls_update_in>-%control-endtimestamp    = COND #( WHEN <ls_update>-control-end_timestamp    = abap_true THEN if_abap_behv=>mk-on ).
*      <ls_update_in>-%control-inputprompt     = COND #( WHEN <ls_update>-control-input_prompt     = abap_true THEN if_abap_behv=>mk-on ).
*      <ls_update_in>-%control-outputprompt    = COND #( WHEN <ls_update>-control-output_prompt    = abap_true THEN if_abap_behv=>mk-on ).
*    ENDLOOP.
*
*    MODIFY ENTITIES OF zr_pru_axc_head
*           ENTITY executionStep
*           UPDATE FROM lt_update_in
*           FAILED DATA(ls_failed_eml)
*           REPORTED DATA(ls_reported_eml).
*
*    LOOP AT ls_failed_eml-executionstep ASSIGNING FIELD-SYMBOL(<ls_failed_step>).
*      APPEND INITIAL LINE TO cs_failed-step ASSIGNING FIELD-SYMBOL(<ls_failed_step_target>).
*      <ls_failed_step_target>-step_uuid = <ls_failed_step>-stepuuid.
*      <ls_failed_step_target>-fail      = CONV #( <ls_failed_step>-%fail-cause ).
*    ENDLOOP.
*
*    LOOP AT ls_reported_eml-executionstep ASSIGNING FIELD-SYMBOL(<ls_reported_step>).
*      APPEND INITIAL LINE TO cs_reported-step ASSIGNING FIELD-SYMBOL(<ls_reported_step_target>).
*      <ls_reported_step_target>-step_uuid = <ls_reported_step>-stepuuid.
**      <ls_reported_step_target>-msg       = <ls_reported_step>-%msg.
*    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_axc_service~delete_step.
*    DATA ls_reported TYPE zpru_if_agent_frw=>ts_axc_reported.
*    DATA ls_failed   TYPE zpru_if_agent_frw=>ts_axc_failed.
*    DATA lt_delete_in TYPE TABLE FOR DELETE zr_pru_axc_head\_executionstep.
*
*    IF it_step_delete_imp IS INITIAL.
*      RETURN.
*    ENDIF.
*
*    precheck_delete_step( EXPORTING it_step_delete_imp = it_step_delete_imp
*                          IMPORTING et_entities        = DATA(lt_entities)
*                          CHANGING  cs_reported        = ls_reported
*                                    cs_failed          = ls_failed ).
*
*    cs_failed = CORRESPONDING #( DEEP ls_failed ).
*    cs_reported = CORRESPONDING #( DEEP ls_reported ).
*
*    IF    lt_entities    <> it_step_delete_imp
*       OR ls_failed-step IS NOT INITIAL.
*      RETURN.
*    ENDIF.
*
*    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_delete>).
*      APPEND INITIAL LINE TO lt_delete_in ASSIGNING FIELD-SYMBOL(<ls_delete_in>).
*      <ls_delete_in>-stepuuid = <ls_delete>-step_uuid.
*    ENDLOOP.
*
*    MODIFY ENTITIES OF zr_pru_axc_head
*           ENTITY executionStep
*           DELETE FROM lt_delete_in
*           FAILED DATA(ls_failed_eml)
*           REPORTED DATA(ls_reported_eml).
*
*    LOOP AT ls_failed_eml-executionstep ASSIGNING FIELD-SYMBOL(<ls_failed_step>).
*      APPEND INITIAL LINE TO cs_failed-step ASSIGNING FIELD-SYMBOL(<ls_failed_step_target>).
*      <ls_failed_step_target>-step_uuid = <ls_failed_step>-stepuuid.
*      <ls_failed_step_target>-fail      = CONV #( <ls_failed_step>-%fail-cause ).
*    ENDLOOP.
*
*    LOOP AT ls_reported_eml-executionstep ASSIGNING FIELD-SYMBOL(<ls_reported_step>).
*      APPEND INITIAL LINE TO cs_reported-step ASSIGNING FIELD-SYMBOL(<ls_reported_step_target>).
*      <ls_reported_step_target>-step_uuid = <ls_reported_step>-stepuuid.
**      <ls_reported_step_target>-msg       = <ls_reported_step>-%msg.
*    ENDLOOP.
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
                                            control = VALUE #( runuuid         = abap_true
                                                               querynumber     = abap_true
                                                               queryuuid       = abap_true
                                                               language        = abap_true
                                                               executionstatus = abap_true
                                                               starttimestamp  = abap_true
                                                               endtimestamp    = abap_true
                                                               inputprompt     = abap_true
                                                               decisionlog     = abap_true
                                                               outputresponse  = abap_true )  ) )
      IMPORTING et_axc_query   = DATA(lt_query_candidates)
      CHANGING  cs_reported    = cs_reported
                cs_failed      = cs_failed ).

    LOOP AT it_axc_head_k ASSIGNING FIELD-SYMBOL(<ls_axc_head_k>).

      DATA(lt_query_copy) = lt_query_candidates.
      DELETE lt_query_copy WHERE runuuid <> <ls_axc_head_k>-runuuid.
      DELETE lt_query_copy WHERE executionstatus <> zpru_if_axc_type_and_constant=>sc_query_status-new.

      SORT lt_query_copy BY starttimestamp ASCENDING.

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
      <ls_cba_in>-runuuid = <ls_create>-runuuid.
      APPEND INITIAL LINE TO <ls_cba_in>-%target ASSIGNING FIELD-SYMBOL(<ls_target>).
      <ls_target>-%cid            = |CID_{ sy-index }_{ <ls_create>-queryuuid }|.
      <ls_target>-queryuuid       = <ls_create>-queryuuid.
      <ls_target>-runuuid         = <ls_create>-runuuid.
      <ls_target>-querynumber     = COND #( WHEN <ls_create>-control-querynumber = abap_true
                                            THEN <ls_create>-querynumber ).
      <ls_target>-language        = COND #( WHEN <ls_create>-control-language = abap_true THEN <ls_create>-language ).
      <ls_target>-executionstatus = COND #( WHEN <ls_create>-control-executionstatus = abap_true
                                            THEN <ls_create>-executionstatus ).
      <ls_target>-starttimestamp  = COND #( WHEN <ls_create>-control-starttimestamp = abap_true
                                            THEN <ls_create>-starttimestamp ).
      <ls_target>-endtimestamp    = COND #( WHEN <ls_create>-control-endtimestamp = abap_true
                                            THEN <ls_create>-endtimestamp ).
      <ls_target>-inputprompt     = COND #( WHEN <ls_create>-control-inputprompt = abap_true
                                            THEN <ls_create>-inputprompt ).
      <ls_target>-decisionlog     = COND #( WHEN <ls_create>-control-decisionlog = abap_true
                                            THEN <ls_create>-decisionlog ).
      <ls_target>-outputresponse  = COND #( WHEN <ls_create>-control-outputresponse = abap_true
                                            THEN <ls_create>-outputresponse ).

      <ls_target>-%control-querynumber     = COND #( WHEN <ls_create>-control-querynumber = abap_true
                                                     THEN if_abap_behv=>mk-on ).
      <ls_target>-%control-language        = COND #( WHEN <ls_create>-control-language = abap_true
                                                     THEN if_abap_behv=>mk-on ).
      <ls_target>-%control-executionstatus = COND #( WHEN <ls_create>-control-executionstatus = abap_true
                                                     THEN if_abap_behv=>mk-on ).
      <ls_target>-%control-starttimestamp  = COND #( WHEN <ls_create>-control-starttimestamp = abap_true
                                                     THEN if_abap_behv=>mk-on ).
      <ls_target>-%control-endtimestamp    = COND #( WHEN <ls_create>-control-endtimestamp = abap_true
                                                     THEN if_abap_behv=>mk-on ).
      <ls_target>-%control-inputprompt     = COND #( WHEN <ls_create>-control-inputprompt = abap_true
                                                     THEN if_abap_behv=>mk-on ).
      <ls_target>-%control-decisionlog     = COND #( WHEN <ls_create>-control-decisionlog = abap_true
                                                     THEN if_abap_behv=>mk-on ).
      <ls_target>-%control-outputresponse  = COND #( WHEN <ls_create>-control-outputresponse = abap_true
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
      <ls_failed_query_target>-queryuuid = <ls_failed_query>-queryuuid.
      <ls_failed_query_target>-fail      = CONV #( <ls_failed_query>-%fail-cause ).
    ENDLOOP.

    LOOP AT ls_reported_eml-executionquery ASSIGNING FIELD-SYMBOL(<ls_reported_query>).
      APPEND INITIAL LINE TO cs_reported-query ASSIGNING FIELD-SYMBOL(<ls_reported_query_target>).
      <ls_reported_query_target>-queryuuid = <ls_reported_query>-queryuuid.
*      <ls_reported_query_target>-msg        = <ls_reported_query>-%msg.
    ENDLOOP.

    LOOP AT ls_mapped_eml-executionquery ASSIGNING FIELD-SYMBOL(<ls_mapped_query>).
      APPEND INITIAL LINE TO cs_mapped-query ASSIGNING FIELD-SYMBOL(<ls_mapped_query_target>).
      <ls_mapped_query_target>-queryuuid = <ls_mapped_query>-queryuuid.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_axc_service~lock.
  ENDMETHOD.

  METHOD zpru_if_axc_service~rba_query.
*    DATA lt_rba_in TYPE TABLE FOR READ BY \_executionquery zr_pru_axc_head.
*
*    IF it_rba_query_k IS INITIAL.
*      RETURN.
*    ENDIF.
*
*    precheck_rba_query( EXPORTING it_rba_query_k = it_rba_query_k
*                        IMPORTING et_entities    = DATA(lt_entities)
*                        CHANGING  cs_reported    = cs_reported
*                                  cs_failed      = cs_failed ).
*
*    IF lt_entities IS INITIAL.
*      RETURN.
*    ENDIF.
*
*    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_read>).
*      APPEND INITIAL LINE TO lt_rba_in ASSIGNING FIELD-SYMBOL(<ls_rba_in>).
*      <ls_rba_in>-runuuid = <ls_read>-run_uuid.
*      <ls_rba_in>-%control-runuuid         = <ls_read>-control-run_uuid.
*      <ls_rba_in>-%control-querynumber     = <ls_read>-control-query_number.
*      <ls_rba_in>-%control-queryuuid       = <ls_read>-control-query_uuid.
*      <ls_rba_in>-%control-language        = <ls_read>-control-language.
*      <ls_rba_in>-%control-executionstatus = <ls_read>-control-execution_status.
*      <ls_rba_in>-%control-starttimestamp  = <ls_read>-control-start_timestamp.
*      <ls_rba_in>-%control-endtimestamp    = <ls_read>-control-end_timestamp.
*      <ls_rba_in>-%control-inputprompt     = <ls_read>-control-input_prompt.
*      <ls_rba_in>-%control-decisionlog     = <ls_read>-control-decision_log.
*      <ls_rba_in>-%control-outputresponse  = <ls_read>-control-output_response.
*    ENDLOOP.
*
*    READ ENTITIES OF zr_pru_axc_head
*         ENTITY executionHeader
*         BY \_executionquery
*         FROM lt_rba_in
*         RESULT DATA(lt_result)
*         FAILED DATA(ls_failed_eml)
*         REPORTED DATA(ls_reported_eml).
*
*    LOOP AT ls_failed_eml-executionheader ASSIGNING FIELD-SYMBOL(<ls_failed_header>).
*      APPEND INITIAL LINE TO cs_failed-header ASSIGNING FIELD-SYMBOL(<ls_failed_header_target>).
*      <ls_failed_header_target>-run_uuid = <ls_failed_header>-runuuid.
*      <ls_failed_header_target>-fail     = CONV #( <ls_failed_header>-%fail-cause ).
*    ENDLOOP.
*
*    LOOP AT ls_reported_eml-executionheader ASSIGNING FIELD-SYMBOL(<ls_reported_header>).
*      APPEND INITIAL LINE TO cs_reported-header ASSIGNING FIELD-SYMBOL(<ls_reported_header_target>).
*      <ls_reported_header_target>-run_uuid = <ls_reported_header>-runuuid.
**      <ls_reported_header_target>-msg      = <ls_reported_header>-%msg.
*    ENDLOOP.
*
*    LOOP AT lt_result ASSIGNING FIELD-SYMBOL(<ls_res>).
*      APPEND INITIAL LINE TO et_axc_query ASSIGNING FIELD-SYMBOL(<ls_out>).
*      <ls_out>-query_uuid       = <ls_res>-queryuuid.
*      <ls_out>-run_uuid         = <ls_res>-runuuid.
*      <ls_out>-query_number     = <ls_res>-querynumber.
*      <ls_out>-language         = <ls_res>-language.
*      <ls_out>-execution_status = <ls_res>-executionstatus.
*      <ls_out>-start_timestamp  = <ls_res>-starttimestamp.
*      <ls_out>-end_timestamp    = <ls_res>-endtimestamp.
*      <ls_out>-input_prompt     = <ls_res>-inputprompt.
*      <ls_out>-decision_log     = <ls_res>-decisionlog.
*      <ls_out>-output_response  = <ls_res>-outputresponse.
*    ENDLOOP.
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
      <ls_read_in>-queryuuid = <ls_read>-queryuuid.
      <ls_read_in>-%control-runuuid         = <ls_read>-control-runuuid.
      <ls_read_in>-%control-querynumber     = <ls_read>-control-querynumber.
      <ls_read_in>-%control-language        = <ls_read>-control-language.
      <ls_read_in>-%control-executionstatus = <ls_read>-control-executionstatus.
      <ls_read_in>-%control-starttimestamp  = <ls_read>-control-starttimestamp.
      <ls_read_in>-%control-endtimestamp    = <ls_read>-control-endtimestamp.
      <ls_read_in>-%control-inputprompt     = <ls_read>-control-inputprompt.
      <ls_read_in>-%control-decisionlog     = <ls_read>-control-decisionlog.
      <ls_read_in>-%control-outputresponse  = <ls_read>-control-outputresponse.
    ENDLOOP.

    READ ENTITIES OF zr_pru_axc_head
         ENTITY executionquery
         FROM lt_read_in
         RESULT DATA(lt_result)
         FAILED DATA(ls_failed_eml)
         REPORTED DATA(ls_reported_eml).

    LOOP AT ls_failed_eml-executionquery ASSIGNING FIELD-SYMBOL(<ls_failed_query>).
      APPEND INITIAL LINE TO cs_failed-query ASSIGNING FIELD-SYMBOL(<ls_failed_query_target>).
      <ls_failed_query_target>-queryuuid = <ls_failed_query>-queryuuid.
      <ls_failed_query_target>-fail      = CONV #( <ls_failed_query>-%fail-cause ).
    ENDLOOP.

    LOOP AT ls_reported_eml-executionquery ASSIGNING FIELD-SYMBOL(<ls_reported_query>).
      APPEND INITIAL LINE TO cs_reported-query ASSIGNING FIELD-SYMBOL(<ls_reported_query_target>).
      <ls_reported_query_target>-queryuuid = <ls_reported_query>-queryuuid.
*      <ls_reported_query_target>-msg        = <ls_reported_query>-%msg.
    ENDLOOP.

    LOOP AT lt_result ASSIGNING FIELD-SYMBOL(<ls_res>).
      APPEND INITIAL LINE TO et_axc_query ASSIGNING FIELD-SYMBOL(<ls_out>).
      <ls_out>-queryuuid       = <ls_res>-queryuuid.
      <ls_out>-runuuid         = <ls_res>-runuuid.
      <ls_out>-querynumber     = <ls_res>-querynumber.
      <ls_out>-language        = <ls_res>-language.
      <ls_out>-executionstatus = <ls_res>-executionstatus.
      <ls_out>-starttimestamp  = <ls_res>-starttimestamp.
      <ls_out>-endtimestamp    = <ls_res>-endtimestamp.
      <ls_out>-inputprompt     = <ls_res>-inputprompt.
      <ls_out>-decisionlog     = <ls_res>-decisionlog.
      <ls_out>-outputresponse  = <ls_res>-outputresponse.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_axc_service~update_query.
*    DATA ls_reported TYPE zpru_if_agent_frw=>ts_axc_reported.
*    DATA ls_failed   TYPE zpru_if_agent_frw=>ts_axc_failed.
*    DATA lt_update_in TYPE TABLE FOR UPDATE zr_pru_axc_head\_executionquery.
*
*    IF it_query_update_imp IS INITIAL.
*      RETURN.
*    ENDIF.
*
*    precheck_update_query( EXPORTING it_query_update_imp = it_query_update_imp
*                            IMPORTING et_entities        = DATA(lt_entities)
*                            CHANGING  cs_reported        = ls_reported
*                                      cs_failed          = ls_failed ).
*
*    cs_failed = CORRESPONDING #( DEEP ls_failed ).
*    cs_reported = CORRESPONDING #( DEEP ls_reported ).
*
*    IF    lt_entities       <> it_query_update_imp
*       OR ls_failed-query   IS NOT INITIAL.
*      RETURN.
*    ENDIF.
*
*    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_update>).
*      APPEND INITIAL LINE TO lt_update_in ASSIGNING FIELD-SYMBOL(<ls_update_in>).
*      <ls_update_in>-queryuuid       = <ls_update>-query_uuid.
*      <ls_update_in>-runuuid         = <ls_update>-run_uuid.
*      <ls_update_in>-querynumber     = COND #( WHEN <ls_update>-control-query_number     = abap_true THEN <ls_update>-query_number ).
*      <ls_update_in>-language        = COND #( WHEN <ls_update>-control-language         = abap_true THEN <ls_update>-language ).
*      <ls_update_in>-executionstatus = COND #( WHEN <ls_update>-control-execution_status = abap_true THEN <ls_update>-execution_status ).
*      <ls_update_in>-starttimestamp  = COND #( WHEN <ls_update>-control-start_timestamp  = abap_true THEN <ls_update>-start_timestamp ).
*      <ls_update_in>-endtimestamp    = COND #( WHEN <ls_update>-control-end_timestamp    = abap_true THEN <ls_update>-end_timestamp ).
*      <ls_update_in>-inputprompt     = COND #( WHEN <ls_update>-control-input_prompt     = abap_true THEN <ls_update>-input_prompt ).
*      <ls_update_in>-decisionlog     = COND #( WHEN <ls_update>-control-decision_log     = abap_true THEN <ls_update>-decision_log ).
*      <ls_update_in>-outputresponse  = COND #( WHEN <ls_update>-control-output_response  = abap_true THEN <ls_update>-output_response ).
*
*      <ls_update_in>-%control-querynumber     = COND #( WHEN <ls_update>-control-query_number     = abap_true THEN if_abap_behv=>mk-on ).
*      <ls_update_in>-%control-language        = COND #( WHEN <ls_update>-control-language         = abap_true THEN if_abap_behv=>mk-on ).
*      <ls_update_in>-%control-executionstatus = COND #( WHEN <ls_update>-control-execution_status = abap_true THEN if_abap_behv=>mk-on ).
*      <ls_update_in>-%control-starttimestamp  = COND #( WHEN <ls_update>-control-start_timestamp  = abap_true THEN if_abap_behv=>mk-on ).
*      <ls_update_in>-%control-endtimestamp    = COND #( WHEN <ls_update>-control-end_timestamp    = abap_true THEN if_abap_behv=>mk-on ).
*      <ls_update_in>-%control-inputprompt     = COND #( WHEN <ls_update>-control-input_prompt     = abap_true THEN if_abap_behv=>mk-on ).
*      <ls_update_in>-%control-decisionlog     = COND #( WHEN <ls_update>-control-decision_log     = abap_true THEN if_abap_behv=>mk-on ).
*      <ls_update_in>-%control-outputresponse  = COND #( WHEN <ls_update>-control-output_response  = abap_true THEN if_abap_behv=>mk-on ).
*    ENDLOOP.
*
*    MODIFY ENTITIES OF zr_pru_axc_head
*           ENTITY executionQuery
*           UPDATE FROM lt_update_in
*           FAILED DATA(ls_failed_eml)
*           REPORTED DATA(ls_reported_eml).
*
*    LOOP AT ls_failed_eml-executionquery ASSIGNING FIELD-SYMBOL(<ls_failed_query>).
*      APPEND INITIAL LINE TO cs_failed-query ASSIGNING FIELD-SYMBOL(<ls_failed_query_target>).
*      <ls_failed_query_target>-query_uuid = <ls_failed_query>-queryuuid.
*      <ls_failed_query_target>-fail       = CONV #( <ls_failed_query>-%fail-cause ).
*    ENDLOOP.
*
*    LOOP AT ls_reported_eml-executionquery ASSIGNING FIELD-SYMBOL(<ls_reported_query>).
*      APPEND INITIAL LINE TO cs_reported-query ASSIGNING FIELD-SYMBOL(<ls_reported_query_target>).
*      <ls_reported_query_target>-query_uuid = <ls_reported_query>-queryuuid.
**      <ls_reported_query_target>-msg        = <ls_reported_query>-%msg.
*    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_axc_service~delete_query.
*    DATA ls_reported TYPE zpru_if_agent_frw=>ts_axc_reported.
*    DATA ls_failed   TYPE zpru_if_agent_frw=>ts_axc_failed.
*    DATA lt_delete_in TYPE TABLE FOR DELETE zr_pru_axc_head\_executionquery.
*
*    IF it_query_delete_imp IS INITIAL.
*      RETURN.
*    ENDIF.
*
*    precheck_delete_query( EXPORTING it_query_delete_imp = it_query_delete_imp
*                           IMPORTING et_entities         = DATA(lt_entities)
*                           CHANGING  cs_reported         = ls_reported
*                                     cs_failed           = ls_failed ).
*
*    cs_failed = CORRESPONDING #( DEEP ls_failed ).
*    cs_reported = CORRESPONDING #( DEEP ls_reported ).
*
*    IF    lt_entities       <> it_query_delete_imp
*       OR ls_failed-query   IS NOT INITIAL.
*      RETURN.
*    ENDIF.
*
*    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_delete>).
*      APPEND INITIAL LINE TO lt_delete_in ASSIGNING FIELD-SYMBOL(<ls_delete_in>).
*      <ls_delete_in>-queryuuid = <ls_delete>-query_uuid.
*    ENDLOOP.
*
*    MODIFY ENTITIES OF zr_pru_axc_head
*           ENTITY executionQuery
*           DELETE FROM lt_delete_in
*           FAILED DATA(ls_failed_eml)
*           REPORTED DATA(ls_reported_eml).
*
*    LOOP AT ls_failed_eml-executionquery ASSIGNING FIELD-SYMBOL(<ls_failed_query>).
*      APPEND INITIAL LINE TO cs_failed-query ASSIGNING FIELD-SYMBOL(<ls_failed_query_target>).
*      <ls_failed_query_target>-query_uuid = <ls_failed_query>-queryuuid.
*      <ls_failed_query_target>-fail       = CONV #( <ls_failed_query>-%fail-cause ).
*    ENDLOOP.
*
*    LOOP AT ls_reported_eml-executionquery ASSIGNING FIELD-SYMBOL(<ls_reported_query>).
*      APPEND INITIAL LINE TO cs_reported-query ASSIGNING FIELD-SYMBOL(<ls_reported_query_target>).
*      <ls_reported_query_target>-query_uuid = <ls_reported_query>-queryuuid.
**      <ls_reported_query_target>-msg        = <ls_reported_query>-%msg.
*    ENDLOOP.
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
      <ls_read_in>-runuuid = <ls_read>-runuuid.
      <ls_read_in>-%control-runid            = <ls_read>-control-runid.
      <ls_read_in>-%control-agentuuid        = <ls_read>-control-agentuuid.
      <ls_read_in>-%control-userid           = <ls_read>-control-userid.
      <ls_read_in>-%control-starttimestamp   = <ls_read>-control-starttimestamp.
      <ls_read_in>-%control-endtimestamp     = <ls_read>-control-endtimestamp.
      <ls_read_in>-%control-createdby        = <ls_read>-control-createdby.
      <ls_read_in>-%control-createdat        = <ls_read>-control-createdat.
      <ls_read_in>-%control-changedby        = <ls_read>-control-changedby.
      <ls_read_in>-%control-lastchanged      = <ls_read>-control-lastchanged.
      <ls_read_in>-%control-locallastchanged = <ls_read>-control-locallastchanged.
    ENDLOOP.

    READ ENTITIES OF zr_pru_axc_head
         ENTITY executionheader
         FROM lt_read_in
         RESULT DATA(lt_result)
         FAILED DATA(ls_failed_eml)
         REPORTED DATA(ls_reported_eml).

    LOOP AT ls_failed_eml-executionheader ASSIGNING FIELD-SYMBOL(<ls_failed_header>).
      APPEND INITIAL LINE TO cs_failed-header ASSIGNING FIELD-SYMBOL(<ls_failed_header_target>).
      <ls_failed_header_target>-runuuid = <ls_failed_header>-runuuid.
      <ls_failed_header_target>-fail    = CONV #( <ls_failed_header>-%fail-cause ).
    ENDLOOP.

    LOOP AT ls_reported_eml-executionheader ASSIGNING FIELD-SYMBOL(<ls_reported_header>).
      APPEND INITIAL LINE TO cs_reported-header ASSIGNING FIELD-SYMBOL(<ls_reported_header_target>).
      <ls_reported_header_target>-runuuid = <ls_reported_header>-runuuid.
*      <ls_reported_header_target>-msg      = <ls_reported_header>-%msg.
    ENDLOOP.

    LOOP AT lt_result ASSIGNING FIELD-SYMBOL(<ls_res>).
      APPEND INITIAL LINE TO et_axc_head ASSIGNING FIELD-SYMBOL(<ls_out>).
      <ls_out>-runuuid          = <ls_res>-runuuid.
      <ls_out>-runid            = <ls_res>-runid.
      <ls_out>-agentuuid        = <ls_res>-agentuuid.
      <ls_out>-userid           = <ls_res>-userid.
      <ls_out>-starttimestamp   = <ls_res>-starttimestamp.
      <ls_out>-endtimestamp     = <ls_res>-endtimestamp.
      <ls_out>-createdby        = <ls_res>-createdby.
      <ls_out>-createdat        = <ls_res>-createdat.
      <ls_out>-changedby        = <ls_res>-changedby.
      <ls_out>-lastchanged      = <ls_res>-lastchanged.
      <ls_out>-locallastchanged = <ls_res>-locallastchanged.
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
      <ls_create_in>-runuuid          = <ls_create>-runuuid.
      <ls_create_in>-runid            = COND #( WHEN <ls_create>-control-runid = abap_true THEN <ls_create>-runid ).
      <ls_create_in>-agentuuid        = COND #( WHEN <ls_create>-control-agentuuid = abap_true
                                                THEN <ls_create>-agentuuid ).
      <ls_create_in>-userid           = COND #( WHEN <ls_create>-control-userid = abap_true THEN <ls_create>-userid ).
      <ls_create_in>-starttimestamp   = COND #( WHEN <ls_create>-control-starttimestamp = abap_true
                                                THEN <ls_create>-starttimestamp ).
      <ls_create_in>-endtimestamp     = COND #( WHEN <ls_create>-control-endtimestamp = abap_true
                                                THEN <ls_create>-endtimestamp ).
      <ls_create_in>-createdby        = COND #( WHEN <ls_create>-control-createdby = abap_true
                                                THEN <ls_create>-createdby ).
      <ls_create_in>-createdat        = COND #( WHEN <ls_create>-control-createdat = abap_true
                                                THEN <ls_create>-createdat ).
      <ls_create_in>-changedby        = COND #( WHEN <ls_create>-control-changedby = abap_true
                                                THEN <ls_create>-changedby ).
      <ls_create_in>-lastchanged      = COND #( WHEN <ls_create>-control-lastchanged = abap_true
                                                THEN <ls_create>-lastchanged ).
      <ls_create_in>-locallastchanged = COND #( WHEN <ls_create>-control-locallastchanged = abap_true
                                                THEN <ls_create>-locallastchanged ).

      <ls_create_in>-%control-runid            = COND #( WHEN <ls_create>-control-runid = abap_true
                                                         THEN if_abap_behv=>mk-on ).
      <ls_create_in>-%control-agentuuid        = COND #( WHEN <ls_create>-control-agentuuid = abap_true
                                                         THEN if_abap_behv=>mk-on ).
      <ls_create_in>-%control-userid           = COND #( WHEN <ls_create>-control-userid = abap_true
                                                         THEN if_abap_behv=>mk-on ).
      <ls_create_in>-%control-starttimestamp   = COND #( WHEN <ls_create>-control-starttimestamp = abap_true
                                                         THEN if_abap_behv=>mk-on ).
      <ls_create_in>-%control-endtimestamp     = COND #( WHEN <ls_create>-control-endtimestamp = abap_true
                                                         THEN if_abap_behv=>mk-on ).
      <ls_create_in>-%control-createdby        = COND #( WHEN <ls_create>-control-createdby = abap_true
                                                         THEN if_abap_behv=>mk-on ).
      <ls_create_in>-%control-createdat        = COND #( WHEN <ls_create>-control-createdat = abap_true
                                                         THEN if_abap_behv=>mk-on ).
      <ls_create_in>-%control-changedby        = COND #( WHEN <ls_create>-control-changedby = abap_true
                                                         THEN if_abap_behv=>mk-on ).
      <ls_create_in>-%control-lastchanged      = COND #( WHEN <ls_create>-control-lastchanged = abap_true
                                                         THEN if_abap_behv=>mk-on ).
      <ls_create_in>-%control-locallastchanged = COND #( WHEN <ls_create>-control-locallastchanged = abap_true
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
      <ls_failed_target>-runuuid = <ls_failed_header>-runuuid.
      <ls_failed_target>-fail    = CONV #( <ls_failed_header>-%fail-cause ).
    ENDLOOP.

    LOOP AT ls_reported_eml-executionheader ASSIGNING FIELD-SYMBOL(<ls_reported_header>).
      APPEND INITIAL LINE TO cs_reported-header ASSIGNING FIELD-SYMBOL(<ls_reported_header_target>).
      <ls_reported_header_target>-runuuid = <ls_reported_header>-runuuid.
*      <ls_reported_header_target>-msg      = <ls_reported_header>-%msg.
    ENDLOOP.

    LOOP AT ls_mapped_eml-executionheader ASSIGNING FIELD-SYMBOL(<ls_mapped_header>).
      APPEND INITIAL LINE TO cs_mapped-header ASSIGNING FIELD-SYMBOL(<ls_mapped_header_target>).
      <ls_mapped_header_target>-runuuid = <ls_mapped_header>-runuuid.
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
      <ls_update_in>-runuuid          = <ls_update>-runuuid.
      <ls_update_in>-runid            = COND #( WHEN <ls_update>-control-runid = abap_true THEN <ls_update>-runid ).
      <ls_update_in>-agentuuid        = COND #( WHEN <ls_update>-control-agentuuid = abap_true
                                                THEN <ls_update>-agentuuid ).
      <ls_update_in>-userid           = COND #( WHEN <ls_update>-control-userid = abap_true THEN <ls_update>-userid ).
      <ls_update_in>-starttimestamp   = COND #( WHEN <ls_update>-control-starttimestamp = abap_true
                                                THEN <ls_update>-starttimestamp ).
      <ls_update_in>-endtimestamp     = COND #( WHEN <ls_update>-control-endtimestamp = abap_true
                                                THEN <ls_update>-endtimestamp ).
      <ls_update_in>-createdby        = COND #( WHEN <ls_update>-control-createdby = abap_true
                                                THEN <ls_update>-createdby ).
      <ls_update_in>-createdat        = COND #( WHEN <ls_update>-control-createdat = abap_true
                                                THEN <ls_update>-createdat ).
      <ls_update_in>-changedby        = COND #( WHEN <ls_update>-control-changedby = abap_true
                                                THEN <ls_update>-changedby ).
      <ls_update_in>-lastchanged      = COND #( WHEN <ls_update>-control-lastchanged = abap_true
                                                THEN <ls_update>-lastchanged ).
      <ls_update_in>-locallastchanged = COND #( WHEN <ls_update>-control-locallastchanged = abap_true
                                                THEN <ls_update>-locallastchanged ).

      <ls_update_in>-%control-runid            = COND #( WHEN <ls_update>-control-runid = abap_true
                                                         THEN if_abap_behv=>mk-on ).
      <ls_update_in>-%control-agentuuid        = COND #( WHEN <ls_update>-control-agentuuid = abap_true
                                                         THEN if_abap_behv=>mk-on ).
      <ls_update_in>-%control-userid           = COND #( WHEN <ls_update>-control-userid = abap_true
                                                         THEN if_abap_behv=>mk-on ).
      <ls_update_in>-%control-starttimestamp   = COND #( WHEN <ls_update>-control-starttimestamp = abap_true
                                                         THEN if_abap_behv=>mk-on ).
      <ls_update_in>-%control-endtimestamp     = COND #( WHEN <ls_update>-control-endtimestamp = abap_true
                                                         THEN if_abap_behv=>mk-on ).
      <ls_update_in>-%control-createdby        = COND #( WHEN <ls_update>-control-createdby = abap_true
                                                         THEN if_abap_behv=>mk-on ).
      <ls_update_in>-%control-createdat        = COND #( WHEN <ls_update>-control-createdat = abap_true
                                                         THEN if_abap_behv=>mk-on ).
      <ls_update_in>-%control-changedby        = COND #( WHEN <ls_update>-control-changedby = abap_true
                                                         THEN if_abap_behv=>mk-on ).
      <ls_update_in>-%control-lastchanged      = COND #( WHEN <ls_update>-control-lastchanged = abap_true
                                                         THEN if_abap_behv=>mk-on ).
      <ls_update_in>-%control-locallastchanged = COND #( WHEN <ls_update>-control-locallastchanged = abap_true
                                                         THEN if_abap_behv=>mk-on ).
    ENDLOOP.

    MODIFY ENTITIES OF zr_pru_axc_head
           ENTITY executionheader
           UPDATE FROM lt_update_in
           FAILED DATA(ls_failed_eml)
           REPORTED DATA(ls_reported_eml).

    LOOP AT ls_failed_eml-executionheader ASSIGNING FIELD-SYMBOL(<ls_failed_header>).
      APPEND INITIAL LINE TO cs_failed-header ASSIGNING FIELD-SYMBOL(<ls_failed_header_target>).
      <ls_failed_header_target>-runuuid = <ls_failed_header>-runuuid.
      <ls_failed_header_target>-fail    = CONV #( <ls_failed_header>-%fail-cause ).
    ENDLOOP.

    LOOP AT ls_reported_eml-executionheader ASSIGNING FIELD-SYMBOL(<ls_reported_header>).
      APPEND INITIAL LINE TO cs_reported-header ASSIGNING FIELD-SYMBOL(<ls_reported_header_target>).
      <ls_reported_header_target>-runuuid = <ls_reported_header>-runuuid.
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
      <ls_delete_in>-runuuid = <ls_delete>-runuuid.
    ENDLOOP.

    MODIFY ENTITIES OF zr_pru_axc_head
           ENTITY executionheader
           DELETE FROM lt_delete_in
           FAILED DATA(ls_failed_eml)
           REPORTED DATA(ls_reported_eml).

    LOOP AT ls_failed_eml-executionheader ASSIGNING FIELD-SYMBOL(<ls_failed_header>).
      APPEND INITIAL LINE TO cs_failed-header ASSIGNING FIELD-SYMBOL(<ls_failed_header_target>).
      <ls_failed_header_target>-runuuid = <ls_failed_header>-runuuid.
      <ls_failed_header_target>-fail    = CONV #( <ls_failed_header>-%fail-cause ).
    ENDLOOP.

    LOOP AT ls_reported_eml-executionheader ASSIGNING FIELD-SYMBOL(<ls_reported_header>).
      APPEND INITIAL LINE TO cs_reported-header ASSIGNING FIELD-SYMBOL(<ls_reported_header_target>).
      <ls_reported_header_target>-runuuid = <ls_reported_header>-runuuid.
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

  METHOD zpru_if_axc_service~generate_run_id.
    DATA lv_run_id_base      TYPE i.
    DATA lv_run_id_base_char TYPE zpru_de_run_id.

    IF iv_run_id_base IS NOT INITIAL.
      lv_run_id_base_char = CONV #( iv_run_id_base ).
      rv_run_id = |{ lv_run_id_base_char ALPHA = IN }|.
      RETURN.
    ENDIF.

    SELECT SINGLE MAX( runid ) FROM zi_pru_axc_head
      INTO @DATA(lv_last_db_run_id).

    DATA(lt_head_buffer) = zpru_cl_axc_buffer=>header_buffer.
    SORT lt_head_buffer BY instance-runid DESCENDING.
    DATA(lv_last_buf_run_id) = VALUE #( lt_head_buffer[ 1 ]-instance-runid OPTIONAL ).

    IF lv_last_db_run_id IS INITIAL AND lv_last_buf_run_id IS INITIAL.
      lv_run_id_base = 1.
      lv_run_id_base_char = CONV #( lv_run_id_base ).
      rv_run_id = |{ lv_run_id_base_char ALPHA = IN }|.
      RETURN.
    ENDIF.

    IF lv_last_db_run_id = lv_last_buf_run_id.
      lv_run_id_base = CONV #( lv_last_buf_run_id ).
    ENDIF.

    IF lv_run_id_base IS INITIAL.
      IF lv_last_db_run_id < lv_last_buf_run_id.
        lv_run_id_base = CONV #( lv_last_buf_run_id ).
      ELSE.
        lv_run_id_base = CONV #( lv_last_db_run_id ).
      ENDIF.
    ENDIF.

    lv_run_id_base += 1.

    lv_run_id_base_char = CONV #( lv_run_id_base ).
    rv_run_id = |{ lv_run_id_base_char ALPHA = IN }|.
  ENDMETHOD.

  METHOD zpru_if_axc_service~generate_query_number.
    DATA ls_reported TYPE zpru_if_agent_frw=>ts_axc_reported.
    DATA ls_failed   TYPE zpru_if_agent_frw=>ts_axc_failed.

    IF iv_query_number_base IS NOT INITIAL.
      rv_query_number = iv_query_number_base + 1.
      RETURN.
    ENDIF.

    IF iv_run_uuid IS INITIAL.
      RETURN.
    ENDIF.

    zpru_if_axc_service~rba_query(
      EXPORTING it_rba_query_k = VALUE #( ( runuuid = iv_run_uuid
                                            control = VALUE #( querynumber = abap_true ) ) )
      IMPORTING et_axc_query   = DATA(lt_axc_query)
      CHANGING  cs_reported    = ls_reported
                cs_failed      = ls_failed   ).

    SORT lt_axc_query BY querynumber DESCENDING.
    DATA(lv_last_query_number) = VALUE #( lt_axc_query[ 1 ]-querynumber OPTIONAL ).

    lv_last_query_number += 1.
    rv_query_number = lv_last_query_number.
  ENDMETHOD.

  METHOD zpru_if_axc_service~generate_step_number.
    DATA ls_reported TYPE zpru_if_agent_frw=>ts_axc_reported.
    DATA ls_failed   TYPE zpru_if_agent_frw=>ts_axc_failed.

    IF iv_step_number_base IS NOT INITIAL.
      rv_step_number = iv_step_number_base + 1.
      RETURN.
    ENDIF.

    IF iv_query_uuid IS INITIAL.
      RETURN.
    ENDIF.

    zpru_if_axc_service~rba_step( EXPORTING it_rba_step_k = VALUE #( ( queryuuid = iv_query_uuid
                                                                       control   = VALUE #( stepnumber = abap_true ) ) )
                                  IMPORTING et_axc_step   = DATA(lt_axc_step)
                                  CHANGING  cs_reported   = ls_reported
                                            cs_failed     = ls_failed   ).

    SORT lt_axc_step BY stepnumber DESCENDING.
    DATA(lv_last_step_number) = VALUE #( lt_axc_step[ 1 ]-stepnumber OPTIONAL ).

    lv_last_step_number += 1.
    rv_step_number = lv_last_step_number.
  ENDMETHOD.
ENDCLASS.
