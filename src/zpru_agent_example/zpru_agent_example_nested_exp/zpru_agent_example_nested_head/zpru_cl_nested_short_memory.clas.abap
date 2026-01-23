CLASS zpru_cl_nested_short_memory DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
  intERFACES ZPRU_IF_SHORT_MEMORY_PROVIDER.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zpru_cl_nested_short_memory IMPLEMENTATION.
  METHOD zpru_if_short_memory_provider~clear_history.

  ENDMETHOD.

  METHOD zpru_if_short_memory_provider~get_discard_strategy.

  ENDMETHOD.

  METHOD zpru_if_short_memory_provider~get_history.

  ENDMETHOD.

  METHOD zpru_if_short_memory_provider~get_long_memory.

  ENDMETHOD.

  METHOD zpru_if_short_memory_provider~get_mem_volume.

  ENDMETHOD.

  METHOD zpru_if_short_memory_provider~save_message.

  ENDMETHOD.

  METHOD zpru_if_short_memory_provider~set_discard_strategy.

  ENDMETHOD.

  METHOD zpru_if_short_memory_provider~set_long_memory.

  ENDMETHOD.

  METHOD zpru_if_short_memory_provider~set_mem_volume.

  ENDMETHOD.

ENDCLASS.
