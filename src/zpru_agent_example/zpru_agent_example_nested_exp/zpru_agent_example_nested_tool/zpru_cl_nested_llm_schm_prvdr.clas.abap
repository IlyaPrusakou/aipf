CLASS zpru_cl_nested_llm_schm_prvdr DEFINITION
  PUBLIC
  INHERITING FROM zpru_cl_tool_schema_provider
  CREATE PUBLIC.

  PUBLIC SECTION.

  PROTECTED SECTION.
    METHODS get_input_abap_type    REDEFINITION.
    METHODS get_input_json_schema  REDEFINITION.
    METHODS get_output_abap_type   REDEFINITION.
    METHODS get_output_json_schema REDEFINITION.

    METHODS create_json_schema_example
      RETURNING VALUE(rv_json_shema) TYPE string
      RAISING   zpru_cx_agent_core.

  PRIVATE SECTION.
ENDCLASS.


CLASS zpru_cl_nested_llm_schm_prvdr IMPLEMENTATION.
  METHOD get_input_abap_type.
    ro_structure_schema ?= cl_abap_structdescr=>describe_by_name( p_name = `ZPRU_S_NESTED_LLM_INPUT` ).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
  ENDMETHOD.

  METHOD get_input_json_schema.
    TRY.
        rv_json_schema = create_json_schema_example( ).
      CATCH zpru_cx_agent_core.
        RETURN.
    ENDTRY.
  ENDMETHOD.

  METHOD get_output_abap_type.
    ro_structure_schema ?= cl_abap_structdescr=>describe_by_name( p_name = `ZPRU_S_NESTED_LLM_OUTPUT` ).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
  ENDMETHOD.

  METHOD get_output_json_schema.
    TRY.
        rv_json_schema = create_json_schema_example( ).
      CATCH zpru_cx_agent_core.
        RETURN.
    ENDTRY.
  ENDMETHOD.

  METHOD create_json_schema_example.
    DATA lo_util TYPE REF TO zpru_if_agent_util.

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

    lo_util ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_AGENT_UTIL`
                                                        iv_context = zpru_if_agent_frw=>cs_context-standard ).

    rv_json_shema = lo_util->create_json_schema( is_abap_schema = ls_abap_schema ).

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
