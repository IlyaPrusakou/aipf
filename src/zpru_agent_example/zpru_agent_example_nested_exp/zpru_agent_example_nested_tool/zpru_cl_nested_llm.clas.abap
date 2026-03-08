CLASS zpru_cl_nested_llm DEFINITION
  PUBLIC
  INHERITING FROM zpru_cl_llm_caller
  CREATE PUBLIC .

  PUBLIC SECTION.

  PROTECTED SECTION.
    METHODS: call_large_language_model_int REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zpru_cl_nested_llm IMPLEMENTATION.

  METHOD call_large_language_model_int.
    DATA ls_input  TYPE zpru_s_nested_llm_input.
    DATA ls_output TYPE zpru_s_nested_llm_output.

    ls_input = is_input->*.

    IF ls_input IS INITIAL.
      RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDIF.

    ls_output-nestedllmoutput = `Nested llm has played`.

    ASSIGN es_output->* TO FIELD-SYMBOL(<ls_output>).
    IF sy-subrc <> 0.
      ev_error_flag = abap_true.
    ENDIF.

    <ls_output> = ls_output.

    " tool from this agent
    APPEND INITIAL LINE TO et_additional_step ASSIGNING FIELD-SYMBOL(<ls_add_step>).
    <ls_add_step>-tooluuid = `76D88C8092D91FE186C3A6B9F02AFBDF`. " renew after data replication
    <ls_add_step>-agentuuid = `76D88C8092D91FE186C3A6B9F02A9BDF`. " renew after data replication
    <ls_add_step>-toolname = `NESTED_ABAP`.
    <ls_add_step>-toolprovider = `ZPRU_CL_NESTED_CODE`.
    <ls_add_step>-steptype = `B`.
    <ls_add_step>-toolschemaprovider = `ZPRU_CL_NESTED_CODE_SCHM_PRVDR`.
    <ls_add_step>-toolinfoprovider = `ZPRU_CL_NESTED_CODE_INFO_PRVDR`.

    " borrowed tool
    APPEND INITIAL LINE TO et_additional_step ASSIGNING <ls_add_step>.
    <ls_add_step>-tooluuid = `76D88C8092D91FE186C3A6B9F02B5BDF`. " renew after data replication
    <ls_add_step>-agentuuid = `76D88C8092D91FE186C3A6B9F02ABBDF`. " renew after data replication
    <ls_add_step>-toolname = `DUMMY_CODE`.
    <ls_add_step>-toolprovider = `ZPRU_CL_DUMMY_AGENT_LOGIC`.
    <ls_add_step>-steptype = `B`.
    <ls_add_step>-toolschemaprovider = `ZPRU_CL_DUMMY_AGENT_LOGIC`.
    <ls_add_step>-toolinfoprovider = `ZPRU_CL_DUMMY_AGENT_LOGIC`.

    " transient tool
    APPEND INITIAL LINE TO et_additional_step ASSIGNING <ls_add_step>.
    <ls_add_step>-tooluuid = ``.
    <ls_add_step>-agentuuid = ``.
    <ls_add_step>-toolname = `TRANSIENT_CODE`.
    <ls_add_step>-toolprovider = `ZPRU_CL_TRANSIENT_CODE`.
    <ls_add_step>-steptype = `B`.
    <ls_add_step>-toolschemaprovider = `ZPRU_CL_TRANSIENT_CODE`.
    <ls_add_step>-toolinfoprovider = `ZPRU_CL_TRANSIENT_CODE`.

  ENDMETHOD.

ENDCLASS.
