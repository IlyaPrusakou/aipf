INTERFACE zpru_if_agent_singleton_meth
  PUBLIC .
    CLASS-METHODS get_short_memory
      RETURNING VALUE(ro_instance) TYPE REF TO zpru_if_short_memory_provider.

    CLASS-METHODS get_long_memory
      RETURNING VALUE(ro_instance) TYPE REF TO zpru_if_long_memory_provider.
ENDINTERFACE.
