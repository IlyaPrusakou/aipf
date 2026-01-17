CLASS zpru_cl_aic_completion_param DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
INTERFACES zpru_if_agent_frw.
    INTERFACES if_aic_completion_parameters .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zpru_cl_aic_completion_param IMPLEMENTATION.


  METHOD if_aic_completion_parameters~set_any_parameter.
  ENDMETHOD.


  METHOD if_aic_completion_parameters~set_maximum_tokens.
  ENDMETHOD.


  METHOD if_aic_completion_parameters~set_temperature.
  ENDMETHOD.
ENDCLASS.
