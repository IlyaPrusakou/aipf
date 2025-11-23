CLASS zpru_cl_short_memory_base DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zpru_if_agent_frw .
    INTERFACES zpru_if_short_memory_provider .
  PROTECTED SECTION.

    DATA mt_agent_message TYPE zpru_if_short_memory_provider=>tt_agent_message.

  PRIVATE SECTION.
ENDCLASS.

CLASS zpru_cl_short_memory_base IMPLEMENTATION.


  METHOD zpru_if_short_memory_provider~clear_history.
    CLEAR: mt_agent_message.
  ENDMETHOD.


  METHOD zpru_if_short_memory_provider~get_history.
    rt_history = mt_agent_message.
  ENDMETHOD.


  METHOD zpru_if_short_memory_provider~save_message.

    IF is_message IS INITIAL.
      RETURN.
    ENDIF.

    DATA(ls_source) = is_message.

    ls_source-history_sequence = lines( mt_agent_message ) + 1.

    GET TIME STAMP FIELD DATA(lv_now).
    ls_source-timestamp = lv_now.

    IF ls_source-message_type IS INITIAL.
      ls_source-message_type = zpru_if_short_memory_provider=>info.
    ENDIF.

    IF ls_source-message_json IS INITIAL AND ls_source-message_tuple IS NOT INITIAL.
      ls_source-message_json = /ui2/cl_json=>serialize( ls_source-message_tuple ).
    ENDIF.

    APPEND INITIAL LINE TO mt_agent_message ASSIGNING FIELD-SYMBOL(<ls_target>).
    <ls_target> = ls_source.

  ENDMETHOD.
ENDCLASS.
