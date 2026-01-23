CLASS zpru_cl_nested_code DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zpru_if_tool_provider.
    INTERFACES zpru_if_tool_executor.
    INTERFACES zpru_if_abap_executor.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zpru_cl_nested_code IMPLEMENTATION.
  METHOD zpru_if_tool_provider~get_tool.
    ro_executor = me.
  ENDMETHOD.

  METHOD zpru_if_abap_executor~execute_code.
  " do some code
  ENDMETHOD.

ENDCLASS.
