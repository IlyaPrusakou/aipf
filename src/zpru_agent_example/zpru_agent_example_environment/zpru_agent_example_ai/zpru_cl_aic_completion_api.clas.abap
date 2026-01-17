CLASS zpru_cl_aic_completion_api DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
INTERFACES zpru_if_agent_frw.
    INTERFACES if_aic_completion_api .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zpru_cl_aic_completion_api IMPLEMENTATION.


  METHOD if_aic_completion_api~create_message_container.
  ENDMETHOD.


  METHOD if_aic_completion_api~define_response_format.
  ENDMETHOD.


  METHOD if_aic_completion_api~execute_for_messages.
  ENDMETHOD.


  METHOD if_aic_completion_api~execute_for_string.
  ENDMETHOD.


  METHOD if_aic_completion_api~get_parameter_setter.
  ENDMETHOD.


  METHOD if_aic_completion_api~register_function.
  ENDMETHOD.
ENDCLASS.
