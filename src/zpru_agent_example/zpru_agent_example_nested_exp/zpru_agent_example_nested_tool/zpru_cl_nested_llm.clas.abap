CLASS zpru_cl_nested_llm DEFINITION
  PUBLIC
  INHERITING FROM zpru_cl_llm_caller
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zpru_if_tool_provider.
      INTERFACES zpru_if_agent_impl.
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
    DATA ls_outbound_header TYPE zpru_s_header_outbound.
    DATA ls_inbound_header  TYPE zpru_s_header_inbound.
    DATA lo_util            TYPE REF TO zpru_if_agent_util.
    DATA lt_outbound_items TYPE zpru_tt_item_outbound.
    DATA lt_inbound_items  TYPE zpru_tt_item_inbound.


    ls_input = is_input->*.

    IF ls_input IS INITIAL.
      RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDIF.

    lo_util ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_AGENT_UTIL`
                                                        iv_context = zpru_if_agent_frw=>cs_context-standard ).

    APPEND INITIAL LINE TO lt_output ASSIGNING FIELD-SYMBOL(<ls_key_value>).
    <ls_key_value>-name   = 'WAREHOUSE'.
    <ls_key_value>-type  = cl_abap_typedescr=>describe_by_data( p_data = lv_lgnum )->absolute_name.
    <ls_key_value>-value  = ls_input-warehouse.

    APPEND INITIAL LINE TO lt_output ASSIGNING <ls_key_value>.
    <ls_key_value>-name   = 'STORAGEBIN'.
    <ls_key_value>-type  = cl_abap_typedescr=>describe_by_data( p_data = lv_storage_bin )->absolute_name.
    <ls_key_value>-value  = `MY_BIN6`.

    APPEND INITIAL LINE TO lt_output ASSIGNING <ls_key_value>.
    <ls_key_value>-name   = 'RESOURCE'.
    <ls_key_value>-type  = cl_abap_typedescr=>describe_by_data( p_data = lv_resource )->absolute_name.
    <ls_key_value>-value  = `MY_RES6`.

    APPEND INITIAL LINE TO lt_output ASSIGNING <ls_key_value>.
    <ls_key_value>-name   = 'NESTED LLM'.
    <ls_key_value>-type  = cl_abap_typedescr=>describe_by_data( p_data = VALUE string( ) )->absolute_name.
    <ls_key_value>-value  = `nested llm code has played`.

    APPEND INITIAL LINE TO lt_output ASSIGNING <ls_key_value>.
    <ls_key_value>-name  = 'OUTBOUNDDELIVERYHEADER'.
    <ls_key_value>-type = cl_abap_typedescr=>describe_by_data( p_data = ls_outbound_header )->absolute_name.

    ls_outbound_header-outboundnumber = 6.
    ls_outbound_header-deliveryname   = 'OUTBOUND_DELIVERY_1'.

    lo_util->convert_to_string( EXPORTING ir_abap   = REF #( ls_outbound_header )
                                CHANGING  cr_string = <ls_key_value>-value ).

    APPEND INITIAL LINE TO lt_output ASSIGNING <ls_key_value>.
    <ls_key_value>-name  = 'INBOUNDDELIVERYHEADER'.
    <ls_key_value>-type = cl_abap_typedescr=>describe_by_data( p_data = ls_inbound_header )->absolute_name.

    ls_inbound_header-inboundnumber = 6.
    ls_inbound_header-deliveryname  = 'INBOUND_DELIVERY_1'.

    lo_util->convert_to_string( EXPORTING ir_abap   = REF #( ls_inbound_header )
                                CHANGING  cr_string = <ls_key_value>-value ).

    ASSIGN COMPONENT 'INBOUNDDELIVERYITEMS' OF STRUCTURE ls_input TO FIELD-SYMBOL(<lt_inbounddeliveryitems>).
    IF sy-subrc = 0.

      lt_inbound_items = <lt_inbounddeliveryitems>.

      APPEND INITIAL LINE TO lt_inbound_items ASSIGNING FIELD-SYMBOL(<ls_inbound_item>).
      <ls_inbound_item>-deliveryname = 'INBOUND_DELIVERY_1'.
      <ls_inbound_item>-inboundnumber = 6.
      <ls_inbound_item>-itemnumber = lines( lt_inbound_items ).
      <ls_inbound_item>-itemname = |INBOUND_ITEM_{ 6 }|.

      APPEND INITIAL LINE TO lt_output ASSIGNING <ls_key_value>.
      <ls_key_value>-name  = 'INBOUNDDELIVERYITEMS'.
      <ls_key_value>-type = cl_abap_typedescr=>describe_by_data( p_data = lt_inbound_items )->absolute_name.
      lo_util->convert_to_string( EXPORTING ir_abap   = REF #( lt_inbound_items )
                                  CHANGING  cr_string = <ls_key_value>-value ).
    ENDIF.

    ASSIGN COMPONENT 'OUTBOUNDDELIVERYITEMS' OF STRUCTURE ls_input TO FIELD-SYMBOL(<lt_outbounddeliveryitems>).
    IF sy-subrc = 0.

      lt_outbound_items = <lt_outbounddeliveryitems>.

      APPEND INITIAL LINE TO lt_outbound_items ASSIGNING FIELD-SYMBOL(<ls_outbound_item>).
      <ls_outbound_item>-deliveryname = 'OUTBOUND_DELIVERY_1'.
      <ls_outbound_item>-outboundnumber = 6.
      <ls_outbound_item>-itemnumber = lines( lt_outbound_items ).
      <ls_outbound_item>-itemname = |OUTBOUND_ITEM_{ 6 }|.

      APPEND INITIAL LINE TO lt_output ASSIGNING <ls_key_value>.
      <ls_key_value>-name  = 'OUTBOUNDDELIVERYITEMS'.
      <ls_key_value>-type = cl_abap_typedescr=>describe_by_data( p_data = lt_outbound_items )->absolute_name.
      lo_util->convert_to_string( EXPORTING ir_abap   = REF #( lt_outbound_items )
                                  CHANGING  cr_string = <ls_key_value>-value ).
    ENDIF.


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
