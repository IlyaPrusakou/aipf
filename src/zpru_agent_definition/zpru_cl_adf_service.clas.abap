CLASS zpru_cl_adf_service DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zpru_if_agent_frw.
    INTERFACES zpru_if_adf_service.

  PROTECTED SECTION.
    TYPES tt_agent      TYPE STANDARD TABLE OF zpru_s_agent WITH EMPTY KEY.
    TYPES tt_agent_tool TYPE STANDARD TABLE OF zpru_s_agent_tool WITH EMPTY KEY.

    METHODS precheck_create_agent
      IMPORTING it_agent_create_imp TYPE zpru_if_adf_type_and_constant=>tt_agent_create_imp
      EXPORTING et_entities         TYPE zpru_if_adf_type_and_constant=>tt_agent_create_imp
      CHANGING  cs_reported         TYPE zpru_if_agent_frw=>ts_adf_reported OPTIONAL
                cs_failed           TYPE zpru_if_agent_frw=>ts_adf_failed   OPTIONAL.

    METHODS precheck_update_agent
      IMPORTING it_agent_update_imp TYPE zpru_if_adf_type_and_constant=>tt_agent_update_imp
      EXPORTING et_entities         TYPE zpru_if_adf_type_and_constant=>tt_agent_update_imp
      CHANGING  cs_reported         TYPE zpru_if_agent_frw=>ts_adf_reported OPTIONAL
                cs_failed           TYPE zpru_if_agent_frw=>ts_adf_failed   OPTIONAL.

    METHODS precheck_delete_agent
      IMPORTING it_agent_delete_imp TYPE zpru_if_adf_type_and_constant=>tt_agent_delete_imp
      EXPORTING et_entities         TYPE zpru_if_adf_type_and_constant=>tt_agent_delete_imp
      CHANGING  cs_reported         TYPE zpru_if_agent_frw=>ts_adf_reported OPTIONAL
                cs_failed           TYPE zpru_if_agent_frw=>ts_adf_failed   OPTIONAL.

    METHODS precheck_read_agent
      IMPORTING it_agent_read_k TYPE zpru_if_adf_type_and_constant=>tt_agent_read_k
      EXPORTING et_entities     TYPE zpru_if_adf_type_and_constant=>tt_agent_read_k
      CHANGING  cs_reported     TYPE zpru_if_agent_frw=>ts_adf_reported OPTIONAL
                cs_failed       TYPE zpru_if_agent_frw=>ts_adf_failed   OPTIONAL.

    METHODS precheck_cba_tool
      IMPORTING it_tool_create_imp TYPE zpru_if_adf_type_and_constant=>tt_tool_create_imp
      EXPORTING et_entities        TYPE zpru_if_adf_type_and_constant=>tt_tool_create_imp
      CHANGING  cs_reported        TYPE zpru_if_agent_frw=>ts_adf_reported OPTIONAL
                cs_failed          TYPE zpru_if_agent_frw=>ts_adf_failed   OPTIONAL.

    METHODS precheck_update_tool
      IMPORTING it_tool_update_imp TYPE zpru_if_adf_type_and_constant=>tt_tool_update_imp
      EXPORTING et_entities        TYPE zpru_if_adf_type_and_constant=>tt_tool_update_imp
      CHANGING  cs_reported        TYPE zpru_if_agent_frw=>ts_adf_reported OPTIONAL
                cs_failed          TYPE zpru_if_agent_frw=>ts_adf_failed   OPTIONAL.

    METHODS precheck_delete_tool
      IMPORTING it_tool_delete_imp TYPE zpru_if_adf_type_and_constant=>tt_tool_delete_imp
      EXPORTING et_entities        TYPE zpru_if_adf_type_and_constant=>tt_tool_delete_imp
      CHANGING  cs_reported        TYPE zpru_if_agent_frw=>ts_adf_reported OPTIONAL
                cs_failed          TYPE zpru_if_agent_frw=>ts_adf_failed   OPTIONAL.

    METHODS precheck_read_tool
      IMPORTING it_tool_read_k TYPE zpru_if_adf_type_and_constant=>tt_tool_read_k
      EXPORTING et_entities    TYPE zpru_if_adf_type_and_constant=>tt_tool_read_k
      CHANGING  cs_reported    TYPE zpru_if_agent_frw=>ts_adf_reported OPTIONAL
                cs_failed      TYPE zpru_if_agent_frw=>ts_adf_failed   OPTIONAL.

    METHODS precheck_rba_tool
      IMPORTING it_rba_tool_k TYPE zpru_if_adf_type_and_constant=>tt_rba_tool_k
      EXPORTING et_entities   TYPE zpru_if_adf_type_and_constant=>tt_rba_tool_k
      CHANGING  cs_reported   TYPE zpru_if_agent_frw=>ts_adf_reported OPTIONAL
                cs_failed     TYPE zpru_if_agent_frw=>ts_adf_failed   OPTIONAL.


ENDCLASS.


CLASS zpru_cl_adf_service IMPLEMENTATION.
  METHOD precheck_create_agent.
    DATA lo_pre TYPE REF TO zpru_if_adf_precheck.

    TRY.
        lo_pre ?= zpru_cl_agent_service_mngr=>get_service(
                      iv_service = `ZPRU_IF_ADF_PRECHECK`
                      iv_context = zpru_if_agent_frw=>cs_context-st_agent_definition ).
      CATCH zpru_cx_agent_core.
        RETURN.
    ENDTRY.

    lo_pre->precheck_create_agent( EXPORTING it_agent_create_imp = it_agent_create_imp
                                   IMPORTING et_entities         = et_entities
                                   CHANGING  cs_reported         = cs_reported
                                             cs_failed           = cs_failed ).
  ENDMETHOD.

  METHOD precheck_update_agent.
    DATA lo_pre TYPE REF TO zpru_if_adf_precheck.

    TRY.
        lo_pre ?= zpru_cl_agent_service_mngr=>get_service(
                      iv_service = `ZPRU_IF_ADF_PRECHECK`
                      iv_context = zpru_if_agent_frw=>cs_context-st_agent_definition ).
      CATCH zpru_cx_agent_core.
        RETURN.
    ENDTRY.

    lo_pre->precheck_update_agent( EXPORTING it_agent_update_imp = it_agent_update_imp
                                   IMPORTING et_entities         = et_entities
                                   CHANGING  cs_reported         = cs_reported
                                             cs_failed           = cs_failed ).
  ENDMETHOD.

  METHOD precheck_delete_agent.
    DATA lo_pre TYPE REF TO zpru_if_adf_precheck.

    TRY.
        lo_pre ?= zpru_cl_agent_service_mngr=>get_service(
                      iv_service = `ZPRU_IF_ADF_PRECHECK`
                      iv_context = zpru_if_agent_frw=>cs_context-st_agent_definition ).
      CATCH zpru_cx_agent_core.
        RETURN.
    ENDTRY.

    lo_pre->precheck_delete_agent( EXPORTING it_agent_delete_imp = it_agent_delete_imp
                                   IMPORTING et_entities         = et_entities
                                   CHANGING  cs_reported         = cs_reported
                                             cs_failed           = cs_failed ).
  ENDMETHOD.

  METHOD precheck_read_agent.
    DATA lo_pre TYPE REF TO zpru_if_adf_precheck.

    TRY.
        lo_pre ?= zpru_cl_agent_service_mngr=>get_service(
                      iv_service = `ZPRU_IF_ADF_PRECHECK`
                      iv_context = zpru_if_agent_frw=>cs_context-st_agent_definition ).
      CATCH zpru_cx_agent_core.
        RETURN.
    ENDTRY.

    lo_pre->precheck_read_agent( EXPORTING it_agent_read_k = it_agent_read_k
                                 IMPORTING et_entities     = et_entities
                                 CHANGING  cs_reported     = cs_reported
                                           cs_failed       = cs_failed ).
  ENDMETHOD.

  METHOD precheck_cba_tool.
    DATA lo_pre TYPE REF TO zpru_if_adf_precheck.

    TRY.
        lo_pre ?= zpru_cl_agent_service_mngr=>get_service(
                      iv_service = `ZPRU_IF_ADF_PRECHECK`
                      iv_context = zpru_if_agent_frw=>cs_context-st_agent_definition ).
      CATCH zpru_cx_agent_core.
        RETURN.
    ENDTRY.

    lo_pre->precheck_cba_tool( EXPORTING it_tool_create_imp = it_tool_create_imp
                               IMPORTING et_entities        = et_entities
                               CHANGING  cs_reported        = cs_reported
                                         cs_failed          = cs_failed ).
  ENDMETHOD.

  METHOD precheck_update_tool.
    DATA lo_pre TYPE REF TO zpru_if_adf_precheck.

    TRY.
        lo_pre ?= zpru_cl_agent_service_mngr=>get_service(
                      iv_service = `ZPRU_IF_ADF_PRECHECK`
                      iv_context = zpru_if_agent_frw=>cs_context-st_agent_definition ).
      CATCH zpru_cx_agent_core.
        RETURN.
    ENDTRY.

    lo_pre->precheck_update_tool( EXPORTING it_tool_update_imp = it_tool_update_imp
                                  IMPORTING et_entities        = et_entities
                                  CHANGING  cs_reported        = cs_reported
                                            cs_failed          = cs_failed ).
  ENDMETHOD.

  METHOD precheck_delete_tool.
    DATA lo_pre TYPE REF TO zpru_if_adf_precheck.

    TRY.
        lo_pre ?= zpru_cl_agent_service_mngr=>get_service(
                      iv_service = `ZPRU_IF_ADF_PRECHECK`
                      iv_context = zpru_if_agent_frw=>cs_context-st_agent_definition ).
      CATCH zpru_cx_agent_core.
        RETURN.
    ENDTRY.

    lo_pre->precheck_delete_tool( EXPORTING it_tool_delete_imp = it_tool_delete_imp
                                  IMPORTING et_entities        = et_entities
                                  CHANGING  cs_reported        = cs_reported
                                            cs_failed          = cs_failed ).
  ENDMETHOD.

  METHOD precheck_read_tool.
    DATA lo_pre TYPE REF TO zpru_if_adf_precheck.

    TRY.
        lo_pre ?= zpru_cl_agent_service_mngr=>get_service(
                      iv_service = `ZPRU_IF_ADF_PRECHECK`
                      iv_context = zpru_if_agent_frw=>cs_context-st_agent_definition ).
      CATCH zpru_cx_agent_core.
        RETURN.
    ENDTRY.

    lo_pre->precheck_read_tool( EXPORTING it_tool_read_k = it_tool_read_k
                                IMPORTING et_entities    = et_entities
                                CHANGING  cs_reported    = cs_reported
                                          cs_failed      = cs_failed ).
  ENDMETHOD.

  METHOD precheck_rba_tool.
    DATA lo_pre TYPE REF TO zpru_if_adf_precheck.

    TRY.
        lo_pre ?= zpru_cl_agent_service_mngr=>get_service(
                      iv_service = `ZPRU_IF_ADF_PRECHECK`
                      iv_context = zpru_if_agent_frw=>cs_context-st_agent_definition ).
      CATCH zpru_cx_agent_core.
        RETURN.
    ENDTRY.

    lo_pre->precheck_rba_tool( EXPORTING it_rba_tool_k = it_rba_tool_k
                               IMPORTING et_entities   = et_entities
                               CHANGING  cs_reported   = cs_reported
                                         cs_failed     = cs_failed ).
  ENDMETHOD.

  METHOD zpru_if_adf_service~query_agent.
    CLEAR: et_agent_k,
           et_tool_agent_link.

    SELECT aipf7agentuuid as agentuuid FROM zi_pru_agent
      WHERE aipf7agentname            IN @it_agent_name
        AND aipf7agenttype            IN @it_agent_type
        AND aipf7decisionprovider     IN @it_decision_provider
        AND aipf7shortmemoryprovider  IN @it_short_memory_provider
        AND aipf7longmemoryprovider   IN @it_long_memory_provider
        AND aipf7agentinfoprovider    IN @it_agent_info_provider
        AND aipf7systempromptprovider IN @it_system_prompt_provider
        AND aipf7agentstatus               IN @it_status
        AND aipf7createdby            IN @it_created_by
        AND aipf7createdat            IN @it_created_at
        AND aipf7changedby            IN @it_changed_by
        AND aipf7lastchanged          IN @it_last_changed
      INTO TABLE @et_agent_k.

    IF et_agent_k IS INITIAL.
      RETURN.
    ENDIF.

    IF et_tool_agent_link IS SUPPLIED.
      SELECT aipf7agentuuid as agentuuid, aipf7tooluuid as tooluuid FROM zi_pru_agent_tool
        FOR ALL ENTRIES IN @et_agent_k
        WHERE aipf7agentuuid = @et_agent_k-agentuuid
        INTO TABLE @et_tool_agent_link.
    ENDIF.
  ENDMETHOD.

  METHOD zpru_if_adf_service~create_agent.
    DATA ls_reported  TYPE zpru_if_agent_frw=>ts_adf_reported.
    DATA ls_failed    TYPE zpru_if_agent_frw=>ts_adf_failed.
    DATA lt_create_in TYPE TABLE FOR READ IMPORT ZR_PRU_AGENT.

    precheck_create_agent( EXPORTING it_agent_create_imp = it_agent_create_imp
                            IMPORTING et_entities        = DATA(lt_entities)
                            CHANGING  cs_reported        = ls_reported
                                      cs_failed          = ls_failed ).

    cs_failed = CORRESPONDING #( DEEP ls_failed ).
    cs_reported = CORRESPONDING #( DEEP ls_reported ).

    IF    lt_entities       <> it_agent_create_imp
       OR ls_failed-agent IS NOT INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_create>).
      APPEND INITIAL LINE TO lt_create_in ASSIGNING FIELD-SYMBOL(<ls_create_in>).
      <ls_create_in>-AIPF7AgentUUID          = <ls_create>-agentuuid.
      <ls_create_in>-AIPF7AgentName            = COND #( WHEN <ls_create>-control-agentname = abap_true THEN <ls_create>-agentname ).
      <ls_create_in>-AIPF7AgentType            = COND #( WHEN <ls_create>-control-agenttype = abap_true THEN <ls_create>-agenttype ).
      <ls_create_in>-AIPF7DecisionProvider     = COND #( WHEN <ls_create>-control-decisionprovider = abap_true THEN <ls_create>-decisionprovider ).
      <ls_create_in>-AIPF7ShortMemoryProvider  = COND #( WHEN <ls_create>-control-shortmemoryprovider = abap_true THEN <ls_create>-shortmemoryprovider ).
      <ls_create_in>-AIPF7LongMemoryProvider   = COND #( WHEN <ls_create>-control-longmemoryprovider = abap_true THEN <ls_create>-longmemoryprovider ).
      <ls_create_in>-AIPF7AgentInfoProvider    = COND #( WHEN <ls_create>-control-agentinfoprovider = abap_true THEN <ls_create>-agentinfoprovider ).
      <ls_create_in>-AIPF7SystemPromptProvider = COND #( WHEN <ls_create>-control-systempromptprovider = abap_true THEN <ls_create>-systempromptprovider ).
      <ls_create_in>-AIPF7AgentStatus                = COND #( WHEN <ls_create>-control-agentstatus = abap_true THEN <ls_create>-agentstatus ).
      <ls_create_in>-AIPF7CreatedBy            = COND #( WHEN <ls_create>-control-createdby = abap_true THEN <ls_create>-createdby ).
      <ls_create_in>-AIPF7CreatedAt            = COND #( WHEN <ls_create>-control-createdat = abap_true THEN <ls_create>-createdat ).
      <ls_create_in>-AIPF7ChangedBy            = COND #( WHEN <ls_create>-control-changedby = abap_true THEN <ls_create>-changedby ).
      <ls_create_in>-AIPF7LastChanged          = COND #( WHEN <ls_create>-control-lastchanged = abap_true THEN <ls_create>-lastchanged ).
      <ls_create_in>-AIPF7LocalLastChanged     = COND #( WHEN <ls_create>-control-locallastchanged = abap_true THEN <ls_create>-locallastchanged ).

      <ls_create_in>-%control-AIPF7AgentName            = COND #( WHEN <ls_create>-control-agentname = abap_true THEN if_abap_behv=>mk-on ).
      <ls_create_in>-%control-AIPF7AgentType            = COND #( WHEN <ls_create>-control-agenttype = abap_true THEN if_abap_behv=>mk-on ).
      <ls_create_in>-%control-AIPF7DecisionProvider     = COND #( WHEN <ls_create>-control-decisionprovider = abap_true THEN if_abap_behv=>mk-on ).
      <ls_create_in>-%control-AIPF7ShortMemoryProvider  = COND #( WHEN <ls_create>-control-shortmemoryprovider = abap_true THEN if_abap_behv=>mk-on ).
      <ls_create_in>-%control-AIPF7LongMemoryProvider   = COND #( WHEN <ls_create>-control-longmemoryprovider = abap_true THEN if_abap_behv=>mk-on ).
      <ls_create_in>-%control-AIPF7AgentInfoProvider    = COND #( WHEN <ls_create>-control-agentinfoprovider = abap_true THEN if_abap_behv=>mk-on ).
      <ls_create_in>-%control-AIPF7SystemPromptProvider = COND #( WHEN <ls_create>-control-systempromptprovider = abap_true THEN if_abap_behv=>mk-on ).
      <ls_create_in>-%control-AIPF7AgentStatus                = COND #( WHEN <ls_create>-control-agentstatus = abap_true THEN if_abap_behv=>mk-on ).
      <ls_create_in>-%control-AIPF7CreatedBy            = COND #( WHEN <ls_create>-control-createdby = abap_true THEN if_abap_behv=>mk-on ).
      <ls_create_in>-%control-AIPF7CreatedAt            = COND #( WHEN <ls_create>-control-createdat = abap_true THEN if_abap_behv=>mk-on ).
      <ls_create_in>-%control-AIPF7ChangedBy            = COND #( WHEN <ls_create>-control-changedby = abap_true THEN if_abap_behv=>mk-on ).
      <ls_create_in>-%control-AIPF7LastChanged          = COND #( WHEN <ls_create>-control-lastchanged = abap_true THEN if_abap_behv=>mk-on ).
      <ls_create_in>-%control-AIPF7LocalLastChanged     = COND #( WHEN <ls_create>-control-locallastchanged = abap_true THEN if_abap_behv=>mk-on ).
    ENDLOOP.

    MODIFY ENTITIES OF ZR_PRU_AGENT
           ENTITY Agent
           CREATE AUTO FILL CID WITH lt_create_in
           MAPPED DATA(ls_mapped_eml)
           FAILED DATA(ls_failed_eml)
           REPORTED DATA(ls_reported_eml).

    LOOP AT ls_failed_eml-agent ASSIGNING FIELD-SYMBOL(<ls_failed_agent>).
      APPEND INITIAL LINE TO cs_failed-agent ASSIGNING FIELD-SYMBOL(<ls_failed_target>).
      <ls_failed_target>-agentuuid = <ls_failed_agent>-AIPF7AgentUUID.
      <ls_failed_target>-fail    = CONV #( <ls_failed_agent>-%fail-cause ).
    ENDLOOP.

    LOOP AT ls_reported_eml-agent ASSIGNING FIELD-SYMBOL(<ls_reported_agent>).
      APPEND INITIAL LINE TO cs_reported-agent ASSIGNING FIELD-SYMBOL(<ls_reported_agent_target>).
      <ls_reported_agent_target>-agentuuid = <ls_reported_agent>-AIPF7AgentUUID.
*      <ls_reported_agent_target>-msg      = <ls_reported_agent>-%msg.
    ENDLOOP.

    LOOP AT ls_mapped_eml-agent ASSIGNING FIELD-SYMBOL(<ls_mapped_agent>).
      APPEND INITIAL LINE TO cs_mapped-agent ASSIGNING FIELD-SYMBOL(<ls_mapped_agent_target>).
      <ls_mapped_agent_target>-agentuuid = <ls_mapped_agent>-AIPF7AgentUUID.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_adf_service~read_agent.
    DATA lt_read_in TYPE TABLE FOR READ IMPORT ZR_PRU_AGENT.

    CLEAR et_agent.

    IF it_agent_read_k IS INITIAL.
      RETURN.
    ENDIF.

    precheck_read_agent( EXPORTING it_agent_read_k = it_agent_read_k
                         IMPORTING et_entities     = DATA(lt_entities)
                         CHANGING  cs_reported     = cs_reported
                                   cs_failed       = cs_failed ).

    IF lt_entities IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_read>).
      APPEND INITIAL LINE TO lt_read_in ASSIGNING FIELD-SYMBOL(<ls_read_in>).
      <ls_read_in>-AIPF7AgentUUID = <ls_read>-agentuuid.
      <ls_read_in>-%control-AIPF7AgentName            = <ls_read>-control-agentname.
      <ls_read_in>-%control-AIPF7AgentType            = <ls_read>-control-agenttype.
      <ls_read_in>-%control-AIPF7DecisionProvider     = <ls_read>-control-decisionprovider.
      <ls_read_in>-%control-AIPF7ShortMemoryProvider  = <ls_read>-control-shortmemoryprovider.
      <ls_read_in>-%control-AIPF7LongMemoryProvider   = <ls_read>-control-longmemoryprovider.
      <ls_read_in>-%control-AIPF7AgentInfoProvider    = <ls_read>-control-agentinfoprovider.
      <ls_read_in>-%control-AIPF7SystemPromptProvider = <ls_read>-control-systempromptprovider.
      <ls_read_in>-%control-AIPF7AgentStatus                = <ls_read>-control-agentstatus.
      <ls_read_in>-%control-AIPF7CreatedBy            = <ls_read>-control-createdby.
      <ls_read_in>-%control-AIPF7CreatedAt            = <ls_read>-control-createdat.
      <ls_read_in>-%control-AIPF7ChangedBy            = <ls_read>-control-changedby.
      <ls_read_in>-%control-AIPF7LastChanged          = <ls_read>-control-lastchanged.
      <ls_read_in>-%control-AIPF7LocalLastChanged     = <ls_read>-control-locallastchanged.
    ENDLOOP.

    READ ENTITIES OF ZR_PRU_AGENT
         ENTITY Agent
         FROM lt_read_in
         RESULT DATA(lt_result)
         FAILED DATA(ls_failed_eml)
         REPORTED DATA(ls_reported_eml).

    LOOP AT ls_failed_eml-agent ASSIGNING FIELD-SYMBOL(<ls_failed_agent>).
      APPEND INITIAL LINE TO cs_failed-agent ASSIGNING FIELD-SYMBOL(<ls_failed_agent_target>).
      <ls_failed_agent_target>-agentuuid = <ls_failed_agent>-AIPF7AgentUUID.
      <ls_failed_agent_target>-fail     = CONV #( <ls_failed_agent>-%fail-cause ).
    ENDLOOP.

    LOOP AT ls_reported_eml-agent ASSIGNING FIELD-SYMBOL(<ls_reported_agent>).
      APPEND INITIAL LINE TO cs_reported-agent ASSIGNING FIELD-SYMBOL(<ls_reported_agent_target>).
      <ls_reported_agent_target>-agentuuid = <ls_reported_agent>-AIPF7AgentUUID.
*      <ls_reported_agent_target>-msg       = <ls_reported_agent>-%msg.
    ENDLOOP.

    LOOP AT lt_result ASSIGNING FIELD-SYMBOL(<ls_res>).
      APPEND INITIAL LINE TO et_agent ASSIGNING FIELD-SYMBOL(<ls_out>).
      <ls_out>-agentuuid            = <ls_res>-AIPF7AgentUUID.
      <ls_out>-agentname            = <ls_res>-AIPF7AgentName.
      <ls_out>-agenttype            = <ls_res>-AIPF7AgentType.
      <ls_out>-decisionprovider     = <ls_res>-AIPF7DecisionProvider.
      <ls_out>-shortmemoryprovider  = <ls_res>-AIPF7ShortMemoryProvider.
      <ls_out>-longmemoryprovider   = <ls_res>-AIPF7LongMemoryProvider.
      <ls_out>-agentinfoprovider    = <ls_res>-AIPF7AgentInfoProvider.
      <ls_out>-systempromptprovider = <ls_res>-AIPF7SystemPromptProvider.
      <ls_out>-agentstatus               = <ls_res>-AIPF7AgentStatus.
      <ls_out>-createdby            = <ls_res>-AIPF7CreatedBy.
      <ls_out>-createdat            = <ls_res>-AIPF7CreatedAt.
      <ls_out>-changedby            = <ls_res>-AIPF7ChangedBy.
      <ls_out>-lastchanged          = <ls_res>-AIPF7LastChanged.
      <ls_out>-locallastchanged     = <ls_res>-AIPF7LocalLastChanged.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_adf_service~update_agent.
    DATA ls_reported  TYPE zpru_if_agent_frw=>ts_adf_reported.
    DATA ls_failed    TYPE zpru_if_agent_frw=>ts_adf_failed.
    DATA lt_update_in TYPE TABLE FOR UPDATE ZR_PRU_AGENT\\Agent.

    precheck_update_agent( EXPORTING it_agent_update_imp = it_agent_update_imp
                            IMPORTING et_entities        = DATA(lt_entities)
                            CHANGING  cs_reported        = ls_reported
                                      cs_failed          = ls_failed ).

    cs_failed = CORRESPONDING #( DEEP ls_failed ).
    cs_reported = CORRESPONDING #( DEEP ls_reported ).

    IF    lt_entities       <> it_agent_update_imp
       OR ls_failed-agent IS NOT INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_update>).
      APPEND INITIAL LINE TO lt_update_in ASSIGNING FIELD-SYMBOL(<ls_update_in>).
      <ls_update_in>-AIPF7AgentUUID          = <ls_update>-agentuuid.
      <ls_update_in>-AIPF7AgentName            = COND #( WHEN <ls_update>-control-agentname = abap_true THEN <ls_update>-agentname ).
      <ls_update_in>-AIPF7AgentType            = COND #( WHEN <ls_update>-control-agenttype = abap_true THEN <ls_update>-agenttype ).
      <ls_update_in>-AIPF7DecisionProvider     = COND #( WHEN <ls_update>-control-decisionprovider = abap_true THEN <ls_update>-decisionprovider ).
      <ls_update_in>-AIPF7ShortMemoryProvider  = COND #( WHEN <ls_update>-control-shortmemoryprovider = abap_true THEN <ls_update>-shortmemoryprovider ).
      <ls_update_in>-AIPF7LongMemoryProvider   = COND #( WHEN <ls_update>-control-longmemoryprovider = abap_true THEN <ls_update>-longmemoryprovider ).
      <ls_update_in>-AIPF7AgentInfoProvider    = COND #( WHEN <ls_update>-control-agentinfoprovider = abap_true THEN <ls_update>-agentinfoprovider ).
      <ls_update_in>-AIPF7SystemPromptProvider = COND #( WHEN <ls_update>-control-systempromptprovider = abap_true THEN <ls_update>-systempromptprovider ).
      <ls_update_in>-AIPF7AgentStatus                = COND #( WHEN <ls_update>-control-agentstatus = abap_true THEN <ls_update>-agentstatus ).
      <ls_update_in>-AIPF7CreatedBy            = COND #( WHEN <ls_update>-control-createdby = abap_true THEN <ls_update>-createdby ).
      <ls_update_in>-AIPF7CreatedAt            = COND #( WHEN <ls_update>-control-createdat = abap_true THEN <ls_update>-createdat ).
      <ls_update_in>-AIPF7ChangedBy            = COND #( WHEN <ls_update>-control-changedby = abap_true THEN <ls_update>-changedby ).
      <ls_update_in>-AIPF7LastChanged          = COND #( WHEN <ls_update>-control-lastchanged = abap_true THEN <ls_update>-lastchanged ).
      <ls_update_in>-AIPF7LocalLastChanged     = COND #( WHEN <ls_update>-control-locallastchanged = abap_true THEN <ls_update>-locallastchanged ).

      <ls_update_in>-%control-AIPF7AgentName            = COND #( WHEN <ls_update>-control-agentname = abap_true THEN if_abap_behv=>mk-on ).
      <ls_update_in>-%control-AIPF7AgentType            = COND #( WHEN <ls_update>-control-agenttype = abap_true THEN if_abap_behv=>mk-on ).
      <ls_update_in>-%control-AIPF7DecisionProvider     = COND #( WHEN <ls_update>-control-decisionprovider = abap_true THEN if_abap_behv=>mk-on ).
      <ls_update_in>-%control-AIPF7ShortMemoryProvider  = COND #( WHEN <ls_update>-control-shortmemoryprovider = abap_true THEN if_abap_behv=>mk-on ).
      <ls_update_in>-%control-AIPF7LongMemoryProvider   = COND #( WHEN <ls_update>-control-longmemoryprovider = abap_true THEN if_abap_behv=>mk-on ).
      <ls_update_in>-%control-AIPF7AgentInfoProvider    = COND #( WHEN <ls_update>-control-agentinfoprovider = abap_true THEN if_abap_behv=>mk-on ).
      <ls_update_in>-%control-AIPF7SystemPromptProvider = COND #( WHEN <ls_update>-control-systempromptprovider = abap_true THEN if_abap_behv=>mk-on ).
      <ls_update_in>-%control-AIPF7AgentStatus                = COND #( WHEN <ls_update>-control-agentstatus = abap_true THEN if_abap_behv=>mk-on ).
      <ls_update_in>-%control-AIPF7CreatedBy            = COND #( WHEN <ls_update>-control-createdby = abap_true THEN if_abap_behv=>mk-on ).
      <ls_update_in>-%control-AIPF7CreatedAt            = COND #( WHEN <ls_update>-control-createdat = abap_true THEN if_abap_behv=>mk-on ).
      <ls_update_in>-%control-AIPF7ChangedBy            = COND #( WHEN <ls_update>-control-changedby = abap_true THEN if_abap_behv=>mk-on ).
      <ls_update_in>-%control-AIPF7LastChanged          = COND #( WHEN <ls_update>-control-lastchanged = abap_true THEN if_abap_behv=>mk-on ).
      <ls_update_in>-%control-AIPF7LocalLastChanged     = COND #( WHEN <ls_update>-control-locallastchanged = abap_true THEN if_abap_behv=>mk-on ).
    ENDLOOP.

    MODIFY ENTITIES OF ZR_PRU_AGENT
           ENTITY Agent
           UPDATE FROM lt_update_in
           FAILED DATA(ls_failed_eml)
           REPORTED DATA(ls_reported_eml).

    LOOP AT ls_failed_eml-agent ASSIGNING FIELD-SYMBOL(<ls_failed_agent>).
      APPEND INITIAL LINE TO cs_failed-agent ASSIGNING FIELD-SYMBOL(<ls_failed_target>).
      <ls_failed_target>-agentuuid = <ls_failed_agent>-AIPF7AgentUUID.
      <ls_failed_target>-fail    = CONV #( <ls_failed_agent>-%fail-cause ).
    ENDLOOP.

    LOOP AT ls_reported_eml-agent ASSIGNING FIELD-SYMBOL(<ls_reported_agent>).
      APPEND INITIAL LINE TO cs_reported-agent ASSIGNING FIELD-SYMBOL(<ls_reported_agent_target>).
      <ls_reported_agent_target>-agentuuid = <ls_reported_agent>-AIPF7AgentUUID.
*      <ls_reported_agent_target>-msg      = <ls_reported_agent>-%msg.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_adf_service~delete_agent.
    DATA ls_reported  TYPE zpru_if_agent_frw=>ts_adf_reported.
    DATA ls_failed    TYPE zpru_if_agent_frw=>ts_adf_failed.
    DATA lt_delete_in TYPE TABLE FOR DELETE ZR_PRU_AGENT.

    precheck_delete_agent( EXPORTING it_agent_delete_imp = it_agent_delete_imp
                           IMPORTING et_entities         = DATA(lt_entities)
                           CHANGING  cs_reported         = ls_reported
                                     cs_failed           = ls_failed ).

    cs_failed = CORRESPONDING #( DEEP ls_failed ).
    cs_reported = CORRESPONDING #( DEEP ls_reported ).

    IF    lt_entities       <> it_agent_delete_imp
       OR ls_failed-agent IS NOT INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_delete>).
      APPEND INITIAL LINE TO lt_delete_in ASSIGNING FIELD-SYMBOL(<ls_delete_in>).
      <ls_delete_in>-AIPF7AgentUUID = <ls_delete>-agentuuid.
    ENDLOOP.

    MODIFY ENTITIES OF ZR_PRU_AGENT
           ENTITY Agent
           DELETE FROM lt_delete_in
           FAILED DATA(ls_failed_eml)
           REPORTED DATA(ls_reported_eml).

    LOOP AT ls_failed_eml-agent ASSIGNING FIELD-SYMBOL(<ls_failed_agent>).
      APPEND INITIAL LINE TO cs_failed-agent ASSIGNING FIELD-SYMBOL(<ls_failed_target>).
      <ls_failed_target>-agentuuid = <ls_failed_agent>-AIPF7AgentUUID.
      <ls_failed_target>-fail    = CONV #( <ls_failed_agent>-%fail-cause ).
    ENDLOOP.

    LOOP AT ls_reported_eml-agent ASSIGNING FIELD-SYMBOL(<ls_reported_agent>).
      APPEND INITIAL LINE TO cs_reported-agent ASSIGNING FIELD-SYMBOL(<ls_reported_agent_target>).
      <ls_reported_agent_target>-agentuuid = <ls_reported_agent>-AIPF7AgentUUID.
*      <ls_reported_agent_target>-msg      = <ls_reported_agent>-%msg.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_adf_service~cba_tool.
    DATA ls_reported TYPE zpru_if_agent_frw=>ts_adf_reported.
    DATA ls_failed   TYPE zpru_if_agent_frw=>ts_adf_failed.
    DATA lt_cba_in   TYPE TABLE FOR CREATE ZR_PRU_AGENT\_tool.

    IF it_tool_create_imp IS INITIAL.
      RETURN.
    ENDIF.

    precheck_cba_tool( EXPORTING it_tool_create_imp = it_tool_create_imp
                       IMPORTING et_entities        = DATA(lt_entities)
                       CHANGING  cs_reported        = ls_reported
                                 cs_failed          = ls_failed ).

    cs_failed = CORRESPONDING #( DEEP ls_failed ).
    cs_reported = CORRESPONDING #( DEEP ls_reported ).

    IF    lt_entities       <> it_tool_create_imp
       OR ls_failed-tool IS NOT INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_create>).
      APPEND INITIAL LINE TO lt_cba_in ASSIGNING FIELD-SYMBOL(<ls_cba_in>).
      <ls_cba_in>-AIPF7AgentUUID = <ls_create>-agentuuid.
      APPEND INITIAL LINE TO <ls_cba_in>-%target ASSIGNING FIELD-SYMBOL(<ls_target>).
      <ls_target>-%cid            = |CID_{ sy-index }_{ <ls_create>-tooluuid }|.
      <ls_target>-AIPF7ToolUuid             = <ls_create>-tooluuid.
      <ls_target>-AIPF7AgentUuid            = <ls_create>-agentuuid.
      <ls_target>-AIPF7ToolName             = COND #( WHEN <ls_create>-control-toolname = abap_true THEN <ls_create>-toolname ).
      <ls_target>-AIPF7ToolProvider         = COND #( WHEN <ls_create>-control-toolprovider = abap_true THEN <ls_create>-toolprovider ).
      <ls_target>-AIPF7StepType             = COND #( WHEN <ls_create>-control-steptype = abap_true THEN <ls_create>-steptype ).
      <ls_target>-AIPF7ToolSchemaProvider   = COND #( WHEN <ls_create>-control-toolschemaprovider = abap_true THEN <ls_create>-toolschemaprovider ).
      <ls_target>-AIPF7ToolInfoProvider     = COND #( WHEN <ls_create>-control-toolinfoprovider = abap_true THEN <ls_create>-toolinfoprovider ).
      <ls_target>-AIPF7ToolIsBorrowed       = COND #( WHEN <ls_create>-control-toolisborrowed = abap_true THEN <ls_create>-toolisborrowed ).
      <ls_target>-AIPF7ToolIsTransient      = COND #( WHEN <ls_create>-control-toolistransient = abap_true THEN <ls_create>-toolistransient ).

      <ls_target>-%control-AIPF7ToolName             = COND #( WHEN <ls_create>-control-toolname = abap_true THEN if_abap_behv=>mk-on ).
      <ls_target>-%control-AIPF7ToolProvider         = COND #( WHEN <ls_create>-control-toolprovider = abap_true THEN if_abap_behv=>mk-on ).
      <ls_target>-%control-AIPF7StepType             = COND #( WHEN <ls_create>-control-steptype = abap_true THEN if_abap_behv=>mk-on ).
      <ls_target>-%control-AIPF7ToolSchemaProvider   = COND #( WHEN <ls_create>-control-toolschemaprovider = abap_true THEN if_abap_behv=>mk-on ).
      <ls_target>-%control-AIPF7ToolInfoProvider     = COND #( WHEN <ls_create>-control-toolinfoprovider = abap_true THEN if_abap_behv=>mk-on ).
      <ls_target>-%control-AIPF7ToolIsBorrowed       = COND #( WHEN <ls_create>-control-toolisborrowed = abap_true THEN if_abap_behv=>mk-on ).
      <ls_target>-%control-AIPF7ToolIsTransient      = COND #( WHEN <ls_create>-control-toolistransient = abap_true THEN if_abap_behv=>mk-on ).
    ENDLOOP.

    MODIFY ENTITIES OF ZR_PRU_AGENT
           ENTITY Agent
           CREATE BY \_tool FROM lt_cba_in
           MAPPED DATA(ls_mapped_eml)
           FAILED DATA(ls_failed_eml)
           REPORTED DATA(ls_reported_eml).

    LOOP AT ls_failed_eml-agent ASSIGNING FIELD-SYMBOL(<ls_failed_parent>).
      APPEND INITIAL LINE TO cs_failed-agent ASSIGNING FIELD-SYMBOL(<ls_failed_agent_target>).
      <ls_failed_agent_target>-agentuuid = <ls_failed_parent>-AIPF7AgentUUID.
      <ls_failed_agent_target>-fail    = CONV #( <ls_failed_parent>-%fail-cause ).
    ENDLOOP.

    LOOP AT ls_failed_eml-agenttool ASSIGNING FIELD-SYMBOL(<ls_failed_tool>).
      APPEND INITIAL LINE TO cs_failed-tool ASSIGNING FIELD-SYMBOL(<ls_failed_target>).
      <ls_failed_target>-agentuuid = <ls_failed_tool>-AIPF7AgentUuid.
      <ls_failed_target>-tooluuid  = <ls_failed_tool>-AIPF7ToolUuid.
      <ls_failed_target>-fail       = CONV #( <ls_failed_tool>-%fail-cause ).
    ENDLOOP.

    LOOP AT ls_reported_eml-agent ASSIGNING FIELD-SYMBOL(<ls_reported_parent>).
      APPEND INITIAL LINE TO cs_reported-agent ASSIGNING FIELD-SYMBOL(<ls_reported_parent_target>).
      <ls_reported_parent_target>-agentuuid = <ls_reported_parent>-AIPF7AgentUUID.
*      <ls_reported_parent_target>-msg      = <ls_reported_parent>-%msg.
    ENDLOOP.

    LOOP AT ls_reported_eml-agenttool ASSIGNING FIELD-SYMBOL(<ls_reported_tool>).
      APPEND INITIAL LINE TO cs_reported-tool ASSIGNING FIELD-SYMBOL(<ls_reported_target>).
      <ls_reported_target>-agentuuid = <ls_reported_tool>-AIPF7AgentUuid.
      <ls_reported_target>-tooluuid  = <ls_reported_tool>-AIPF7ToolUuid.
*      <ls_reported_target>-msg       = <ls_reported_tool>-%msg.
    ENDLOOP.

    LOOP AT ls_mapped_eml-agenttool ASSIGNING FIELD-SYMBOL(<ls_mapped_tool>).
      APPEND INITIAL LINE TO cs_mapped-tool ASSIGNING FIELD-SYMBOL(<ls_mapped_target>).
      <ls_mapped_target>-agentuuid = <ls_mapped_tool>-AIPF7AgentUuid.
      <ls_mapped_target>-tooluuid  = <ls_mapped_tool>-AIPF7ToolUuid.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_adf_service~rba_tool.
    DATA lt_rba_in TYPE TABLE FOR READ IMPORT ZR_PRU_AGENT\_tool.

    CLEAR et_tool.

    IF it_rba_tool_k IS INITIAL.
      RETURN.
    ENDIF.

    precheck_rba_tool( EXPORTING it_rba_tool_k = it_rba_tool_k
                       IMPORTING et_entities   = DATA(lt_entities)
                       CHANGING  cs_reported   = cs_reported
                                 cs_failed     = cs_failed ).

    IF lt_entities IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_read>).
      APPEND INITIAL LINE TO lt_rba_in ASSIGNING FIELD-SYMBOL(<ls_rba_in>).
      <ls_rba_in>-AIPF7AgentUUID = <ls_read>-agentuuid.
      <ls_rba_in>-%control-AIPF7ToolUuid           = <ls_read>-control-tooluuid.
      <ls_rba_in>-%control-AIPF7AgentUuid          = <ls_read>-control-agentuuid.
      <ls_rba_in>-%control-AIPF7ToolName           = <ls_read>-control-toolname.
      <ls_rba_in>-%control-AIPF7ToolProvider       = <ls_read>-control-toolprovider.
      <ls_rba_in>-%control-AIPF7StepType           = <ls_read>-control-steptype.
      <ls_rba_in>-%control-AIPF7ToolSchemaProvider = <ls_read>-control-toolschemaprovider.
      <ls_rba_in>-%control-AIPF7ToolInfoProvider   = <ls_read>-control-toolinfoprovider.
      <ls_rba_in>-%control-AIPF7ToolIsBorrowed     = <ls_read>-control-toolisborrowed.
      <ls_rba_in>-%control-AIPF7ToolIsTransient    = <ls_read>-control-toolistransient.
    ENDLOOP.

    READ ENTITIES OF ZR_PRU_AGENT
         ENTITY Agent
         BY \_tool
         FROM lt_rba_in
         RESULT DATA(lt_result)
         FAILED DATA(ls_failed_eml)
         REPORTED DATA(ls_reported_eml).

    LOOP AT ls_failed_eml-agent ASSIGNING FIELD-SYMBOL(<ls_failed_agent>).
      APPEND INITIAL LINE TO cs_failed-agent ASSIGNING FIELD-SYMBOL(<ls_failed_agent_target>).
      <ls_failed_agent_target>-agentuuid = <ls_failed_agent>-AIPF7AgentUUID.
      <ls_failed_agent_target>-fail    = CONV #( <ls_failed_agent>-%fail-cause ).
    ENDLOOP.

    LOOP AT ls_reported_eml-agent ASSIGNING FIELD-SYMBOL(<ls_reported_agent>).
      APPEND INITIAL LINE TO cs_reported-agent ASSIGNING FIELD-SYMBOL(<ls_reported_agent_target>).
      <ls_reported_agent_target>-agentuuid = <ls_reported_agent>-AIPF7AgentUUID.
*      <ls_reported_agent_target>-msg      = <ls_reported_agent>-%msg.
    ENDLOOP.

    LOOP AT lt_result ASSIGNING FIELD-SYMBOL(<ls_res>).
      APPEND INITIAL LINE TO et_tool ASSIGNING FIELD-SYMBOL(<ls_out>).
      <ls_out>-tooluuid           = <ls_res>-AIPF7ToolUuid.
      <ls_out>-agentuuid          = <ls_res>-AIPF7AgentUuid.
      <ls_out>-toolname           = <ls_res>-AIPF7ToolName.
      <ls_out>-toolprovider       = <ls_res>-AIPF7ToolProvider.
      <ls_out>-steptype           = <ls_res>-AIPF7StepType.
      <ls_out>-toolschemaprovider = <ls_res>-AIPF7ToolSchemaProvider.
      <ls_out>-toolinfoprovider   = <ls_res>-AIPF7ToolInfoProvider.
      <ls_out>-toolisborrowed         = <ls_res>-AIPF7ToolIsBorrowed.
      <ls_out>-toolistransient        = <ls_res>-AIPF7ToolIsTransient.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_adf_service~read_tool.
    DATA lt_read_in TYPE TABLE FOR READ IMPORT ZR_PRU_AGENT\AgentTool.

    CLEAR et_tool.

    IF it_tool_read_k IS INITIAL.
      RETURN.
    ENDIF.

    precheck_read_tool( EXPORTING it_tool_read_k = it_tool_read_k
                        IMPORTING et_entities    = DATA(lt_entities)
                        CHANGING  cs_reported    = cs_reported
                                  cs_failed      = cs_failed ).

    IF lt_entities IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_read>).
      APPEND INITIAL LINE TO lt_read_in ASSIGNING FIELD-SYMBOL(<ls_read_in>).
      <ls_read_in>-AIPF7ToolUuid = <ls_read>-tooluuid.
      <ls_read_in>-%control-AIPF7ToolUuid           = <ls_read>-control-tooluuid.
      <ls_read_in>-%control-AIPF7AgentUuid          = <ls_read>-control-agentuuid.
      <ls_read_in>-%control-AIPF7ToolName           = <ls_read>-control-toolname.
      <ls_read_in>-%control-AIPF7ToolProvider       = <ls_read>-control-toolprovider.
      <ls_read_in>-%control-AIPF7StepType           = <ls_read>-control-steptype.
      <ls_read_in>-%control-AIPF7ToolSchemaProvider = <ls_read>-control-toolschemaprovider.
      <ls_read_in>-%control-AIPF7ToolInfoProvider   = <ls_read>-control-toolinfoprovider.
      <ls_read_in>-%control-AIPF7ToolIsBorrowed     = <ls_read>-control-toolisborrowed.
      <ls_read_in>-%control-AIPF7ToolIsTransient    = <ls_read>-control-toolistransient.
    ENDLOOP.

    READ ENTITIES OF ZR_PRU_AGENT
         ENTITY AgentTool
         FROM lt_read_in
         RESULT DATA(lt_result)
         FAILED DATA(ls_failed_eml)
         REPORTED DATA(ls_reported_eml).

    LOOP AT ls_failed_eml-agenttool ASSIGNING FIELD-SYMBOL(<ls_failed_tool>).
      APPEND INITIAL LINE TO cs_failed-tool ASSIGNING FIELD-SYMBOL(<ls_failed_tool_target>).
      <ls_failed_tool_target>-tooluuid  = <ls_failed_tool>-AIPF7ToolUuid.
      <ls_failed_tool_target>-agentuuid = <ls_failed_tool>-AIPF7AgentUuid.
      <ls_failed_tool_target>-fail       = CONV #( <ls_failed_tool>-%fail-cause ).
    ENDLOOP.

    LOOP AT ls_reported_eml-agenttool ASSIGNING FIELD-SYMBOL(<ls_reported_tool>).
      APPEND INITIAL LINE TO cs_reported-tool ASSIGNING FIELD-SYMBOL(<ls_reported_tool_target>).
      <ls_reported_tool_target>-tooluuid  = <ls_reported_tool>-AIPF7ToolUuid.
      <ls_reported_tool_target>-agentuuid = <ls_reported_tool>-AIPF7AgentUuid.
*      <ls_reported_tool_target>-msg       = <ls_reported_tool>-%msg.
    ENDLOOP.

    LOOP AT lt_result ASSIGNING FIELD-SYMBOL(<ls_res>).
      APPEND INITIAL LINE TO et_tool ASSIGNING FIELD-SYMBOL(<ls_out>).
      <ls_out>-tooluuid           = <ls_res>-AIPF7ToolUuid.
      <ls_out>-agentuuid          = <ls_res>-AIPF7AgentUuid.
      <ls_out>-toolname           = <ls_res>-AIPF7ToolName.
      <ls_out>-toolprovider       = <ls_res>-AIPF7ToolProvider.
      <ls_out>-steptype           = <ls_res>-AIPF7StepType.
      <ls_out>-toolschemaprovider = <ls_res>-AIPF7ToolSchemaProvider.
      <ls_out>-toolinfoprovider   = <ls_res>-AIPF7ToolInfoProvider.
      <ls_out>-toolisborrowed         = <ls_res>-AIPF7ToolIsBorrowed.
      <ls_out>-toolistransient        = <ls_res>-AIPF7ToolIsTransient.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_adf_service~update_tool.
    DATA ls_reported  TYPE zpru_if_agent_frw=>ts_adf_reported.
    DATA ls_failed    TYPE zpru_if_agent_frw=>ts_adf_failed.
    DATA lt_update_in TYPE TABLE FOR UPDATE ZR_PRU_AGENT\AgentTool.

    IF it_tool_update_imp IS INITIAL.
      RETURN.
    ENDIF.

    precheck_update_tool( EXPORTING it_tool_update_imp = it_tool_update_imp
                          IMPORTING et_entities        = DATA(lt_entities)
                          CHANGING  cs_reported        = ls_reported
                                    cs_failed          = ls_failed ).

    cs_failed = CORRESPONDING #( DEEP ls_failed ).
    cs_reported = CORRESPONDING #( DEEP ls_reported ).

    IF    lt_entities       <> it_tool_update_imp
       OR ls_failed-tool IS NOT INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_update>).
      APPEND INITIAL LINE TO lt_update_in ASSIGNING FIELD-SYMBOL(<ls_update_in>).
      <ls_update_in>-AIPF7ToolUuid           = <ls_update>-tooluuid.
      <ls_update_in>-AIPF7AgentUuid          = <ls_update>-agentuuid.
      <ls_update_in>-AIPF7ToolName           = COND #( WHEN <ls_update>-control-toolname = abap_true THEN <ls_update>-toolname ).
      <ls_update_in>-AIPF7ToolProvider       = COND #( WHEN <ls_update>-control-toolprovider = abap_true THEN <ls_update>-toolprovider ).
      <ls_update_in>-AIPF7StepType           = COND #( WHEN <ls_update>-control-steptype = abap_true THEN <ls_update>-steptype ).
      <ls_update_in>-AIPF7ToolSchemaProvider = COND #( WHEN <ls_update>-control-toolschemaprovider = abap_true THEN <ls_update>-toolschemaprovider ).
      <ls_update_in>-AIPF7ToolInfoProvider   = COND #( WHEN <ls_update>-control-toolinfoprovider = abap_true THEN <ls_update>-toolinfoprovider ).
      <ls_update_in>-AIPF7ToolIsBorrowed     = COND #( WHEN <ls_update>-control-toolisborrowed = abap_true THEN <ls_update>-toolisborrowed ).
      <ls_update_in>-AIPF7ToolIsTransient    = COND #( WHEN <ls_update>-control-toolistransient = abap_true THEN <ls_update>-toolistransient ).

      <ls_update_in>-%control-AIPF7ToolName           = COND #( WHEN <ls_update>-control-toolname = abap_true THEN if_abap_behv=>mk-on ).
      <ls_update_in>-%control-AIPF7ToolProvider       = COND #( WHEN <ls_update>-control-toolprovider = abap_true THEN if_abap_behv=>mk-on ).
      <ls_update_in>-%control-AIPF7StepType           = COND #( WHEN <ls_update>-control-steptype = abap_true THEN if_abap_behv=>mk-on ).
      <ls_update_in>-%control-AIPF7ToolSchemaProvider = COND #( WHEN <ls_update>-control-toolschemaprovider = abap_true THEN if_abap_behv=>mk-on ).
      <ls_update_in>-%control-AIPF7ToolInfoProvider   = COND #( WHEN <ls_update>-control-toolinfoprovider = abap_true THEN if_abap_behv=>mk-on ).
      <ls_update_in>-%control-AIPF7ToolIsBorrowed     = COND #( WHEN <ls_update>-control-toolisborrowed = abap_true THEN if_abap_behv=>mk-on ).
      <ls_update_in>-%control-AIPF7ToolIsTransient    = COND #( WHEN <ls_update>-control-toolistransient = abap_true THEN if_abap_behv=>mk-on ).
    ENDLOOP.

    MODIFY ENTITIES OF ZR_PRU_AGENT
           ENTITY AgentTool
           UPDATE FROM lt_update_in
           FAILED DATA(ls_failed_eml)
           REPORTED DATA(ls_reported_eml).

    LOOP AT ls_failed_eml-agenttool ASSIGNING FIELD-SYMBOL(<ls_failed_tool>).
      APPEND INITIAL LINE TO cs_failed-tool ASSIGNING FIELD-SYMBOL(<ls_failed_tool_target>).
      <ls_failed_tool_target>-tooluuid  = <ls_failed_tool>-AIPF7ToolUuid.
      <ls_failed_tool_target>-agentuuid = <ls_failed_tool>-AIPF7AgentUuid.
      <ls_failed_tool_target>-fail       = CONV #( <ls_failed_tool>-%fail-cause ).
    ENDLOOP.

    LOOP AT ls_reported_eml-agenttool ASSIGNING FIELD-SYMBOL(<ls_reported_tool>).
      APPEND INITIAL LINE TO cs_reported-tool ASSIGNING FIELD-SYMBOL(<ls_reported_tool_target>).
      <ls_reported_tool_target>-tooluuid  = <ls_reported_tool>-AIPF7ToolUuid.
      <ls_reported_tool_target>-agentuuid = <ls_reported_tool>-AIPF7AgentUuid.
*      <ls_reported_tool_target>-msg       = <ls_reported_tool>-%msg.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_adf_service~delete_tool.
    DATA lo_util TYPE REF TO zpru_if_agent_util.

    precheck_delete_tool( EXPORTING it_tool_delete_imp = it_tool_delete_imp
                          IMPORTING et_entities        = DATA(lt_entities)
                          CHANGING  cs_reported        = cs_reported
                                    cs_failed          = cs_failed ).

    IF lt_entities IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        lo_util ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_AGENT_UTIL`
                                                            iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RAISE SHORTDUMP NEW zpru_cx_agent_core( ).
    ENDTRY.

    zpru_cl_adf_buffer=>prep_agent_buffer( VALUE #( FOR <ls_k>
                                                    IN     lt_entities
                                                    ( agent_uuid = <ls_k>-agentuuid ) ) ).

    zpru_cl_adf_buffer=>prep_tool_buffer( VALUE #( FOR <ls_q>
                                                   IN     lt_entities
                                                   ( agent_uuid = <ls_q>-agentuuid
                                                     tool_uuid  = <ls_q>-tooluuid
                                                     full_key   = abap_true ) ) ).

    LOOP AT lt_entities ASSIGNING FIELD-SYMBOL(<ls_delete>).

      ASSIGN zpru_cl_adf_buffer=>agent_buffer[ instance-agentuuid = <ls_delete>-agentuuid ] TO FIELD-SYMBOL(<ls_parent>).
      IF sy-subrc = 0 AND <ls_parent>-deleted = abap_true.
        " Parent deleted implies child deleted.
      ELSE.

        ASSIGN zpru_cl_adf_buffer=>tool_buffer[ instance-agentuuid = <ls_delete>-agentuuid
                                                instance-tooluuid  = <ls_delete>-tooluuid
                                                deleted             = abap_false ] TO FIELD-SYMBOL(<ls_buffer>).
        IF sy-subrc = 0.
          <ls_buffer>-deleted = abap_true.
          <ls_buffer>-changed = abap_true.
        ELSE.
          APPEND VALUE #( agentuuid = <ls_delete>-agentuuid
                          tooluuid  = <ls_delete>-tooluuid
                          delete    = abap_true
                          fail      = zpru_if_agent_frw=>cs_fail_cause-not_found )
                 TO cs_failed-tool.

          APPEND VALUE #( agentuuid = <ls_delete>-agentuuid
                          tooluuid  = <ls_delete>-tooluuid
                          delete    = abap_true
                          msg       = lo_util->new_message(
                                          iv_id       = zpru_if_agent_frw=>cs_message_class-zpru_msg_execution
                                          iv_number   = `003`
                                          iv_severity = zpru_if_agent_message=>sc_severity-error ) )
                 TO cs_reported-tool.

        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD zpru_if_adf_service~clean_up.
  ENDMETHOD.

  METHOD zpru_if_adf_service~do_save.
  ENDMETHOD.

  METHOD zpru_if_adf_service~determine.
    " Placeholder for determination logic
  ENDMETHOD.

  METHOD zpru_if_adf_service~validate.
  ENDMETHOD.

ENDCLASS.
