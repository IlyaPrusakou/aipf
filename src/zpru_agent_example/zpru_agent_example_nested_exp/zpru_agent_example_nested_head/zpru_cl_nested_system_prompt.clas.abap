CLASS zpru_cl_nested_system_prompt DEFINITION INHERITING FROM zpru_cl_syst_prmpt_prvdr_base
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  INTERFACES zpru_if_agent_impl.
  PROTECTED SECTION.
    METHODS: set_primary_session_task REDEFINITION,
      set_technical_rules REDEFINITION,
      set_business_rules REDEFINITION,
      set_format_guidelines REDEFINITION,
      set_reasoning_step REDEFINITION,
      set_prompt_restrictions REDEFINITION,
      set_arbitrary_text REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zpru_cl_nested_system_prompt IMPLEMENTATION.

  METHOD set_arbitrary_text.
    RETURN.
  ENDMETHOD.

  METHOD set_business_rules.
    RETURN.
  ENDMETHOD.

  METHOD set_format_guidelines.
    RETURN.
  ENDMETHOD.

  METHOD set_primary_session_task.
    RETURN.
  ENDMETHOD.

  METHOD set_prompt_restrictions.
    RETURN.
  ENDMETHOD.

  METHOD set_reasoning_step.
    RETURN.
  ENDMETHOD.

  METHOD set_technical_rules.
    RETURN.
  ENDMETHOD.

ENDCLASS.
