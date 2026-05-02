CLASS zpru_cl_agent_base DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zpru_if_agent_base.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zpru_cl_agent_base IMPLEMENTATION.
  METHOD zpru_if_agent_base~get_response_content.
    DATA lo_util           TYPE REF TO zpru_if_agent_util.
    DATA ls_final_response TYPE zpru_s_final_response.

    lo_util ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_AGENT_UTIL`
                                                        iv_context = zpru_if_agent_frw=>cs_context-standard ).

    lo_util->convert_to_abap( EXPORTING ir_string = REF #( iv_final_response )
                              CHANGING  cr_abap   = ls_final_response ).

    lo_util->convert_to_abap( EXPORTING ir_string = REF #( ls_final_response-finalresponsebody-responsecontent )
                              CHANGING  cr_abap   = ed_response_body ).
  ENDMETHOD.
ENDCLASS.
