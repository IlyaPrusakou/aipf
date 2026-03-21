CLASS zpru_cl_nested_llm DEFINITION
  PUBLIC
  INHERITING FROM zpru_cl_llm_caller
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zpru_if_tool_provider.
  PROTECTED SECTION.
    METHODS: call_large_language_model_int REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zpru_cl_nested_llm IMPLEMENTATION.

  METHOD call_large_language_model_int.
    DATA ls_input  TYPE zpru_s_nested_llm_input.
    DATA lt_output TYPE zpru_tt_key_value.
    DATA lv_lgnum  TYPE char4.
    DATA lv_storage_bin TYPE char16.
    DATA lv_resource TYPE char16.

    ls_input = is_input->*.

    IF ls_input IS INITIAL.
      RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDIF.

    APPEND INITIAL LINE TO lt_output ASSIGNING FIELD-SYMBOL(<ls_key_value>).
    <ls_key_value>-name   = 'WAREHOUSE'.
    <ls_key_value>-type  ?= cl_abap_typedescr=>describe_by_data( p_data = lv_lgnum ).
    <ls_key_value>-value  = ls_input-warehouse.

    APPEND INITIAL LINE TO lt_output ASSIGNING <ls_key_value>.
    <ls_key_value>-name   = 'STORAGEBIN'.
    <ls_key_value>-type  ?= cl_abap_typedescr=>describe_by_data( p_data = lv_storage_bin ).
    <ls_key_value>-value  = `MY_BIN6`.

    APPEND INITIAL LINE TO lt_output ASSIGNING <ls_key_value>.
    <ls_key_value>-name   = 'RESOURCE'.
    <ls_key_value>-type  ?= cl_abap_typedescr=>describe_by_data( p_data = lv_resource ).
    <ls_key_value>-value  = `MY_RES6`.

    APPEND INITIAL LINE TO lt_output ASSIGNING <ls_key_value>.
    <ls_key_value>-name   = 'NESTED LLM'.
    <ls_key_value>-type  ?= cl_abap_typedescr=>describe_by_data( p_data = VALUE string( ) ).
    <ls_key_value>-value  = `nested llm code has played`.

    ASSIGN es_output->* TO FIELD-SYMBOL(<lt_output>).
    IF sy-subrc <> 0.
      ev_error_flag = abap_true.
    ENDIF.

    <lt_output> = lt_output.
    et_key_value_pairs = lt_output.

    " tool from this agent
    APPEND INITIAL LINE TO et_additional_step ASSIGNING FIELD-SYMBOL(<ls_add_step>).
    <ls_add_step>-tooluuid           = `1AF42792E6DC1FE187F880BE37E634D7`. " renew after data replication
    <ls_add_step>-agentuuid          = `1AF42792E6DC1FE187F880BE37E5D4D7`. " renew after data replication
    <ls_add_step>-toolname = `NESTED_ABAP`.
    <ls_add_step>-toolprovider = `ZPRU_CL_NESTED_CODE`.
    <ls_add_step>-steptype = `B`.
    <ls_add_step>-toolschemaprovider = `ZPRU_CL_NESTED_CODE_SCHM_PRVDR`.
    <ls_add_step>-toolinfoprovider = `ZPRU_CL_NESTED_CODE_INFO_PRVDR`.

    " borrowed tool
    APPEND INITIAL LINE TO et_additional_step ASSIGNING <ls_add_step>.
    <ls_add_step>-tooluuid = `1AF42792E6DC1FE187F880BE37E694D7`. " renew after data replication
    <ls_add_step>-agentuuid = `1AF42792E6DC1FE187F880BE37E5F4D7`. " renew after data replication
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

  METHOD zpru_if_tool_provider~get_tool.
    ro_executor = me.
  ENDMETHOD.

ENDCLASS.
