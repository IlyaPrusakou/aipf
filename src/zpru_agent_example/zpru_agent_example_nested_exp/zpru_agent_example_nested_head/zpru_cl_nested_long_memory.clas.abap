CLASS zpru_cl_nested_long_memory DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  inteRFACES ZPRU_IF_LONG_MEMORY_PROVIDER.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zpru_cl_nested_long_memory IMPLEMENTATION.
  METHOD zpru_if_long_memory_provider~get_msg_persistence.

  ENDMETHOD.

  METHOD zpru_if_long_memory_provider~get_summarization.

  ENDMETHOD.

  METHOD zpru_if_long_memory_provider~get_sum_persistence.

  ENDMETHOD.

  METHOD zpru_if_long_memory_provider~retrieve_message.

  ENDMETHOD.

  METHOD zpru_if_long_memory_provider~retrieve_summary.

  ENDMETHOD.

  METHOD zpru_if_long_memory_provider~save_messages.

  ENDMETHOD.

  METHOD zpru_if_long_memory_provider~save_summary.

  ENDMETHOD.

  METHOD zpru_if_long_memory_provider~set_msg_persistence.

  ENDMETHOD.

  METHOD zpru_if_long_memory_provider~set_summarization.

  ENDMETHOD.

  METHOD zpru_if_long_memory_provider~set_sum_persistence.

  ENDMETHOD.

  METHOD zpru_if_long_memory_provider~summarize_conversation.

  ENDMETHOD.

ENDCLASS.
