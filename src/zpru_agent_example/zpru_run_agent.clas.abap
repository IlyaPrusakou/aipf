CLASS zpru_run_agent DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zpru_run_agent IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.

    DATA(lo_agent_run) = NEW zpru_cl_api_agent( ).
    TRY.
        lo_agent_run->zpru_if_api_agent~initialize( iv_agent_name = 'MyFirstAgent' ).
        lo_agent_run->zpru_if_api_agent~set_input_query( iv_input_query = 'TOOL_1' ).
        lo_agent_run->zpru_if_api_agent~build_execution( ).
        lo_agent_run->zpru_if_api_agent~save_execution( iv_do_commit = abap_false ).

        COMMIT WORK.

        lo_agent_run->zpru_if_api_agent~run( ).

      CATCH zpru_cx_agent_core.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
