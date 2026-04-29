CLASS zpru_run_agent DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zpru_if_agent_frw.
    INTERFACES if_oo_adt_classrun.
ENDCLASS.


CLASS zpru_run_agent IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
    DATA lo_cl_unit_agent TYPE REF TO zpru_if_unit_agent.
    DATA ls_prompt        TYPE zpru_s_prompt.

    zpru_cl_logic_switch=>set_logic( iv_dummy_logic = abap_true ).

    ls_prompt-string_content = `Hello, it's my prompt`.

    lo_cl_unit_agent = NEW zpru_cl_unit_agent( ).
    TRY.
        lo_cl_unit_agent->execute_agent( iv_agent_name = 'DUMMY_AGENT'
                                         is_prompt     = ls_prompt ).
      CATCH zpru_cx_agent_core.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
