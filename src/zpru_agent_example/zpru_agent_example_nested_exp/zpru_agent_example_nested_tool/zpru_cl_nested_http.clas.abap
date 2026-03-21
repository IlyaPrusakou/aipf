CLASS zpru_cl_nested_http DEFINITION
  PUBLIC
  INHERITING FROM zpru_cl_http_request_sender
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zpru_if_tool_provider.
  PROTECTED SECTION.
    METHODS send_http_int REDEFINITION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zpru_cl_nested_http IMPLEMENTATION.
  METHOD send_http_int.
    DATA ls_input  TYPE zpru_s_nested_http_input.
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
    <ls_key_value>-value  = `MY_BIN5`.

    APPEND INITIAL LINE TO lt_output ASSIGNING <ls_key_value>.
    <ls_key_value>-name   = 'RESOURCE'.
    <ls_key_value>-type  ?= cl_abap_typedescr=>describe_by_data( p_data = lv_resource ).
    <ls_key_value>-value  = `MY_RES5`.

    APPEND INITIAL LINE TO lt_output ASSIGNING <ls_key_value>.
    <ls_key_value>-name   = 'NESTED HTTP'.
    <ls_key_value>-type  ?= cl_abap_typedescr=>describe_by_data( p_data = VALUE string( ) ).
    <ls_key_value>-value  = `nested http code has played`.

    ASSIGN es_output->* TO FIELD-SYMBOL(<lt_output>).
    IF sy-subrc <> 0.
      ev_error_flag = abap_true.
    ENDIF.

    <lt_output> = lt_output.
    et_key_value_pairs = lt_output.
  ENDMETHOD.
  METHOD zpru_if_tool_provider~get_tool.
    ro_executor = me.
  ENDMETHOD.

ENDCLASS.
