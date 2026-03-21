
CLASS lcl_adf_abap_executor IMPLEMENTATION.
  METHOD execute_code_int.
    DATA ls_input  TYPE zpru_s_nested_abap_input.
    DATA lt_output          TYPE zpru_tt_key_value.
    DATA lv_lgnum           TYPE char4.
    DATA lv_storage_bin     TYPE char16.
    DATA lv_resource        TYPE char16.
    DATA ls_outbound_header TYPE zpru_s_header_outbound.
    DATA ls_inbound_header  TYPE zpru_s_header_inbound.
    DATA lo_util            TYPE REF TO zpru_if_agent_util.
    DATA lt_outbound_items  TYPE zpru_tt_item_outbound.
    DATA lt_inbound_items   TYPE zpru_tt_item_inbound.

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
    <ls_key_value>-value  = `MY_BIN12`.

    APPEND INITIAL LINE TO lt_output ASSIGNING <ls_key_value>.
    <ls_key_value>-name   = 'RESOURCE'.
    <ls_key_value>-type  = cl_abap_typedescr=>describe_by_data( p_data = lv_resource )->absolute_name.
    <ls_key_value>-value  = `MY_RES12`.

    APPEND INITIAL LINE TO lt_output ASSIGNING <ls_key_value>.
    <ls_key_value>-name   = 'NESTED ABAP'.
    <ls_key_value>-type  = cl_abap_typedescr=>describe_by_data( p_data = VALUE string( ) )->absolute_name.
    <ls_key_value>-value  = `nested abap code has played`.

    APPEND INITIAL LINE TO lt_output ASSIGNING <ls_key_value>.
    <ls_key_value>-name  = 'OUTBOUNDDELIVERYHEADER'.
    <ls_key_value>-type = cl_abap_typedescr=>describe_by_data( p_data = ls_outbound_header )->absolute_name.

    ls_outbound_header-outboundnumber = 4.
    ls_outbound_header-deliveryname   = 'OUTBOUND_DELIVERY_1'.

    lo_util->convert_to_string( EXPORTING ir_abap   = REF #( ls_outbound_header )
                                CHANGING  cr_string = <ls_key_value>-value ).

    APPEND INITIAL LINE TO lt_output ASSIGNING <ls_key_value>.
    <ls_key_value>-name  = 'INBOUNDDELIVERYHEADER'.
    <ls_key_value>-type = cl_abap_typedescr=>describe_by_data( p_data = ls_inbound_header )->absolute_name.

    ls_inbound_header-inboundnumber = 4.
    ls_inbound_header-deliveryname  = 'INBOUND_DELIVERY_1'.

    lo_util->convert_to_string( EXPORTING ir_abap   = REF #( ls_inbound_header )
                                CHANGING  cr_string = <ls_key_value>-value ).

    ASSIGN COMPONENT 'INBOUNDDELIVERYITEMS' OF STRUCTURE ls_input TO FIELD-SYMBOL(<lt_inbounddeliveryitems>).
    IF sy-subrc = 0.

      lt_inbound_items = <lt_inbounddeliveryitems>.

      APPEND INITIAL LINE TO lt_inbound_items ASSIGNING FIELD-SYMBOL(<ls_inbound_item>).
      <ls_inbound_item>-deliveryname  = 'INBOUND_DELIVERY_1'.
      <ls_inbound_item>-inboundnumber = 4.
      <ls_inbound_item>-itemnumber    = lines( lt_inbound_items ).
      <ls_inbound_item>-itemname      = |INBOUND_ITEM_{ 12 }|.

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
      <ls_outbound_item>-deliveryname   = 'OUTBOUND_DELIVERY_1'.
      <ls_outbound_item>-outboundnumber = 4.
      <ls_outbound_item>-itemnumber     = lines( lt_outbound_items ).
      <ls_outbound_item>-itemname       = |OUTBOUND_ITEM_{ 12 }|.

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

  ENDMETHOD.
ENDCLASS.


CLASS lcl_adf_tool_provider IMPLEMENTATION.
  METHOD provide_tool_instance.
    ro_executor = NEW lcl_adf_abap_executor( ).
  ENDMETHOD.
ENDCLASS.


CLASS lcl_adf_tool_info_provider IMPLEMENTATION.
  METHOD get_main_tool_info.
    RETURN.
  ENDMETHOD.

  METHOD set_tool_parameters.
    RETURN.
  ENDMETHOD.

  METHOD set_tool_properties.
    RETURN.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_adf_schema_provider IMPLEMENTATION.
  METHOD get_input_abap_type.
    ro_structure_schema ?= cl_abap_structdescr=>describe_by_name( p_name = `ZPRU_S_NESTED_ABAP_INPUT` ).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
  ENDMETHOD.

  METHOD get_input_json_schema.
    TRY.

        create_json_schema_example( IMPORTING ev_json_schema    = ev_json_schema
                                              es_json_structure = es_json_structure ).

      CATCH zpru_cx_agent_core.
        RETURN.
    ENDTRY.
  ENDMETHOD.

  METHOD get_output_abap_type.
    ro_structure_schema ?= cl_abap_structdescr=>describe_by_name( p_name = `ZPRU_S_NESTED_ABAP_OUTPUT` ).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
  ENDMETHOD.

  METHOD get_output_json_schema.
    TRY.
        create_json_schema_example( IMPORTING ev_json_schema    = ev_json_schema
                                              es_json_structure = es_json_structure ).

      CATCH zpru_cx_agent_core.
        RETURN.
    ENDTRY.
  ENDMETHOD.

  METHOD create_json_schema_example.
    DATA lo_util TYPE REF TO zpru_if_agent_util.

    CLEAR: ev_json_schema,
           es_json_structure.

    " Properties for the nested structure
    DATA(lt_fields_3_4) = VALUE zpru_tt_json_schema_prop(
                                    ( name = 'field3' type = 'string'  description = 'Third field' )
                                    ( name = 'field4' type = 'integer' description = 'Fourth field' ) ).

    " The nested structure itself is an 'object' type
    DATA(lo_nested_struct) = NEW zpru_s_json_schema_prop( type       = 'object'
                                                          properties = REF #( lt_fields_3_4 ) ).

    " Columns for the table row
    DATA(lt_fields_5_6) = VALUE zpru_tt_json_schema_prop(
                                    ( name = 'field5' type = 'boolean' description = 'Fifth field' )
                                    ( name = 'field6' type = 'string'  description = 'Sixth field' ) ).

    " The row template
    DATA(lo_row_template) = NEW zpru_s_json_schema_prop( type       = 'object'
                                                         properties = REF #( lt_fields_5_6 ) ).

    " The actual table property
    DATA(lo_nested_table) = NEW zpru_s_json_schema_prop( type  = 'array'
                                                         items = lo_row_template ).

    " Define root properties (field1, field2, and the two nested objects)
    DATA(lt_root_props) = VALUE zpru_tt_json_schema_prop( type = 'string'
                                                          ( name = 'field1' description = 'First field' )
                                                          ( name = 'field2' description = 'Second field' ) ).

    " Insert the complex types we built above
    INSERT VALUE #( name       = 'nested_structure'
                    type       = 'object'
                    properties = lo_nested_struct->properties )
           INTO TABLE lt_root_props.

    INSERT VALUE #( name  = 'nested_table'
                    type  = 'array'
                    items = lo_nested_table->items )
           INTO TABLE lt_root_props.

    " Final Root Schema Assignment
    DATA(ls_abap_schema) = VALUE zpru_s_json_schema( vschema              = 'http://json-schema.org/draft-07/schema#'
                                                     title                = 'ZPRU_COMPLEX_OUTPUT'
                                                     type                 = 'object'
                                                     properties           = lt_root_props
                                                     additionalproperties = abap_true ).

    es_json_structure = ls_abap_schema.

    lo_util ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_AGENT_UTIL`
                                                        iv_context = zpru_if_agent_frw=>cs_context-standard ).

    ev_json_schema = lo_util->create_json_schema( is_abap_schema = ls_abap_schema ).

    " output

    " {
    "    "$schema":"http://json-schema.org/draft-07/schema#",
    "    "title":"ZPRU_COMPLEX_OUTPUT",
    "    "type":"object",
    "    "properties":{
    "       "field1":{
    "          "type":"string",
    "          "description":"First field"
    "       },
    "       "field2":{
    "          "type":"string",
    "          "description":"Second field"
    "       },
    "       "nested_structure":{
    "          "type":"object",
    "          "properties":{
    "             "field3":{
    "                "type":"string",
    "                "description":"Third field"
    "             },
    "             "field4":{
    "                "type":"integer",
    "                "description":"Fourth field"
    "             }
    "          }
    "       },
    "       "nested_table":{
    "          "type":"array",
    "          "items":{
    "             "type":"object",
    "             "properties":{
    "                "field5":{
    "                   "type":"boolean",
    "                   "description":"Fifth field"
    "                },
    "                "field6":{
    "                   "type":"string",
    "                   "description":"Sixth field"
    "                }
    "             }
    "          }
    "       }
    "    },
    "    "additionalProperties":true
    " }
    "
  ENDMETHOD.
ENDCLASS.
