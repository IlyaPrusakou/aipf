CLASS zpru_cl_ml_model_inference DEFINITION
  PUBLIC
INHERITING FROM zpru_cl_tool_executor ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zpru_if_ml_model_inference .
  PROTECTED SECTION.

    METHODS get_ml_inference_int
      ABSTRACT
      IMPORTING io_controller           TYPE REF TO zpru_if_agent_controller
                io_input                TYPE REF TO data
                io_tool_schema_provider TYPE REF TO zpru_if_tool_schema_provider OPTIONAL
                io_tool_info_provider   TYPE REF TO zpru_if_tool_info_provider   OPTIONAL
      EXPORTING eo_output               TYPE REF TO data
                ev_error_flag           TYPE abap_boolean
                et_additional_step      TYPE zpru_tt_additional_step
      RAISING   zpru_cx_agent_core.

  PRIVATE SECTION.
ENDCLASS.



CLASS zpru_cl_ml_model_inference IMPLEMENTATION.


  METHOD zpru_if_ml_model_inference~get_machine_learning_inference.
  ENDMETHOD.
ENDCLASS.
