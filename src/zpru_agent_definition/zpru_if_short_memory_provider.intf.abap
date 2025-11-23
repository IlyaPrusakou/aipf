INTERFACE zpru_if_short_memory_provider
  PUBLIC .

  INTERFACES zpru_if_agent_frw.

  TYPES: BEGIN OF ENUM es_message_type,
           query       VALUE IS INITIAL,
           step_input  VALUE 1,
           step_output VALUE 2,
           repsonse    VALUE 3,
           info        VALUE 4,
         END OF ENUM es_message_type.

  TYPES: BEGIN OF ts_agent_message,
           agent_uuid       TYPE xstring,
           run_uuid         TYPE xstring,
           step_uuid        TYPE xstring,
           history_sequence TYPE i,
           timestamp        TYPE timestampl,
           message_type     TYPE zpru_if_short_memory_provider=>es_message_type,
           message_json     TYPE zpru_if_agent_frw=>ts_json,
           message_tuple    TYPE zpru_tt_key_value_tuple,
         END OF ts_agent_message.

  TYPES: tt_agent_message TYPE STANDARD TABLE OF ts_agent_message WITH EMPTY KEY.

  METHODS save_message
    IMPORTING is_message TYPE zpru_if_short_memory_provider=>ts_agent_message.
  METHODS get_history
    RETURNING VALUE(rt_history) TYPE zpru_if_short_memory_provider=>tt_agent_message.
  METHODS clear_history.


ENDINTERFACE.
