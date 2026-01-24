CLASS zpru_cl_nested_llm_schm_prvdr DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES ZPRU_IF_INPUT_SCHEMA_PROVIDER.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zpru_cl_nested_llm_schm_prvdr IMPLEMENTATION.

  METHOD zpru_if_input_schema_provider~input_json_schema.

  ENDMETHOD.

  METHOD zpru_if_input_schema_provider~input_rtts_schema.

  ENDMETHOD.

  METHOD zpru_if_input_schema_provider~output_json_schema.

  ENDMETHOD.

  METHOD zpru_if_input_schema_provider~output_rtts_schema.

  ENDMETHOD.

ENDCLASS.
