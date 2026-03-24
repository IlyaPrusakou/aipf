CLASS zpru_cl_tool_info_provider DEFINITION
  PUBLIC
   ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zpru_if_agent_frw .
    INTERFACES zpru_if_tool_info_provider .
  PROTECTED SECTION.


    METHODS get_main_tool_info ABSTRACT
      IMPORTING
        is_tool_master_data TYPE zpru_if_adf_type_and_constant=>ts_agent_tool
        is_execution_step   TYPE zpru_if_axc_type_and_constant=>ts_axc_step OPTIONAL
      EXPORTING
        ev_toolname         TYPE char100
        ev_tooldesciption   TYPE char100
        ev_toolexplanation  TYPE string
        ev_tooltype         TYPE zpru_de_adf_step_type.

    METHODS set_tool_properties ABSTRACT
      IMPORTING
                is_tool_master_data     TYPE zpru_if_adf_type_and_constant=>ts_agent_tool
                is_execution_step       TYPE zpru_if_axc_type_and_constant=>ts_axc_step OPTIONAL
      RETURNING VALUE(rt_tool_property) TYPE zpru_tt_tool_property.

    METHODS set_tool_parameters ABSTRACT
      IMPORTING
                is_tool_master_data  TYPE zpru_if_adf_type_and_constant=>ts_agent_tool
                is_execution_step    TYPE zpru_if_axc_type_and_constant=>ts_axc_step OPTIONAL
      RETURNING VALUE(rt_perameters) TYPE zpru_tt_param_info.


  PRIVATE SECTION.
ENDCLASS.



CLASS zpru_cl_tool_info_provider IMPLEMENTATION.


  METHOD zpru_if_tool_info_provider~get_abap_tool_info.

    DATA ls_tool_info TYPE zpru_s_tool_info.

    get_main_tool_info(
      EXPORTING
        is_tool_master_data = is_tool_master_data
        is_execution_step   = is_execution_step
      IMPORTING
        ev_toolname         = ls_tool_info-toolname
        ev_tooldesciption   = ls_tool_info-tooldesciption
        ev_toolexplanation  = ls_tool_info-toolexplanation
        ev_tooltype         = ls_tool_info-tooltype  ).

    ls_tool_info-toolproperty = set_tool_properties(
      EXPORTING
        is_tool_master_data = is_tool_master_data
        is_execution_step   = is_execution_step ).

    ls_tool_info-parameterinfo = set_tool_parameters(
      EXPORTING
        is_tool_master_data = is_tool_master_data
        is_execution_step   = is_execution_step ).

    rs_abap_tool_info = ls_tool_info.

  ENDMETHOD.


  METHOD zpru_if_tool_info_provider~get_tool_info.
    DATA lo_util TYPE REF TO zpru_if_agent_util.

    lo_util ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_AGENT_UTIL`
                                                        iv_context = zpru_if_agent_frw=>cs_context-standard ).

    DATA(ls_abap_tool_info) = zpru_if_tool_info_provider~get_abap_tool_info(
                                 is_tool_master_data = is_tool_master_data
                                 is_execution_step   = is_execution_step ).

    lo_util->convert_to_string( EXPORTING ir_abap          = REF #( ls_abap_tool_info )
                                          iv_compress      = abap_true
                                CHANGING  cr_string        = rv_tool_info ).

  ENDMETHOD.
ENDCLASS.
