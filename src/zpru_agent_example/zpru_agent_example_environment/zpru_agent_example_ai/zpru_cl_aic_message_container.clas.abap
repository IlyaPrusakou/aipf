CLASS zpru_cl_aic_message_container DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
INTERFACES zpru_if_agent_frw.
    INTERFACES if_aic_message_container .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zpru_cl_aic_message_container IMPLEMENTATION.


  METHOD if_aic_message_container~add_assistant_message.
  ENDMETHOD.


  METHOD if_aic_message_container~add_tool_results.
  ENDMETHOD.


  METHOD if_aic_message_container~add_user_media_message.
  ENDMETHOD.


  METHOD if_aic_message_container~add_user_message.
  ENDMETHOD.


  METHOD if_aic_message_container~get_messages.
  ENDMETHOD.


  METHOD if_aic_message_container~set_system_role.
  ENDMETHOD.
ENDCLASS.
