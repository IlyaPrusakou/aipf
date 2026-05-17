CLASS zpru_cl_nested_llm_info_prvdr DEFINITION
  PUBLIC
  INHERITING FROM zpru_cl_tool_info_provider
  CREATE PUBLIC.

  PUBLIC SECTION.
  INTERFACES zpru_if_agent_impl.
  PROTECTED SECTION.
    METHODS get_main_tool_info  REDEFINITION.
    METHODS set_tool_properties REDEFINITION.
    METHODS set_tool_parameters REDEFINITION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zpru_cl_nested_llm_info_prvdr IMPLEMENTATION.
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
