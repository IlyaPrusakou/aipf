CLASS zpru_cl_tool_provider DEFINITION
  PUBLIC ABSTRACT
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zpru_if_agent_frw.
    INTERFACES zpru_if_tool_provider.

  PROTECTED SECTION.
    METHODS provide_tool_instance ABSTRACT
      IMPORTING is_agent            TYPE zpru_if_adf_type_and_constant=>ts_agent
                io_controller       TYPE REF TO zpru_if_agent_controller
                io_input            TYPE REF TO zpru_if_payload
                is_tool_master_data TYPE zpru_if_adf_type_and_constant=>ts_agent_tool
                is_execution_step   TYPE zpru_if_axc_type_and_constant=>ts_axc_step
      RETURNING VALUE(ro_executor)  TYPE REF TO zpru_if_tool_executor
      RAISING   zpru_cx_agent_core.

  PRIVATE SECTION.
ENDCLASS.


CLASS zpru_cl_tool_provider IMPLEMENTATION.
  METHOD zpru_if_tool_provider~get_tool.
    DATA lo_tool TYPE REF TO zpru_if_tool_executor.

    lo_tool = provide_tool_instance( is_agent            = is_agent
                                     io_controller       = io_controller
                                     io_input            = io_input
                                     is_tool_master_data = is_tool_master_data
                                     is_execution_step   = is_execution_step ).

    ro_executor = lo_tool.
  ENDMETHOD.
ENDCLASS.
