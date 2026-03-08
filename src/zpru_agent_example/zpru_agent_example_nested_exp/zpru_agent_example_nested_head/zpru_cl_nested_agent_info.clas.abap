CLASS zpru_cl_nested_agent_info DEFINITION INHERITING FROM zpru_cl_agent_info_provider
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  PROTECTED SECTION.
    METHODS: get_agent_main_info REDEFINITION,
      set_agent_goals REDEFINITION,
      prepare_agent_domains REDEFINITION,
      set_agent_restrictions REDEFINITION,
      set_tool_metadata REDEFINITION,
      get_free_text REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zpru_cl_nested_agent_info IMPLEMENTATION.

  METHOD get_agent_main_info.
    RETURN.
  ENDMETHOD.

  METHOD get_free_text.
    RETURN.
  ENDMETHOD.

  METHOD prepare_agent_domains.
    RETURN.
  ENDMETHOD.

  METHOD set_agent_goals.
    RETURN.
  ENDMETHOD.

  METHOD set_agent_restrictions.
    RETURN.
  ENDMETHOD.

  METHOD set_tool_metadata.
    RETURN.
  ENDMETHOD.

ENDCLASS.
