CLASS zpru_cl_nested_llm DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zpru_if_tool_provider.
    INTERFACES zpru_if_tool_executor.
    INTERFACES zpru_if_llm_caller.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zpru_cl_nested_llm IMPLEMENTATION.
  METHOD zpru_if_llm_caller~call_large_language_model.
    " do some llm
  ENDMETHOD.

  METHOD zpru_if_tool_provider~get_tool.
    ro_executor = me.
  ENDMETHOD.

ENDCLASS.
