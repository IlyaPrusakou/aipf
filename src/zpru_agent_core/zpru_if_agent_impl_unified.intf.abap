INTERFACE zpru_if_agent_impl_unified
  PUBLIC .

  INTERFACES zpru_if_agent_impl.
  INTERFACES if_serializable_object.
  INTERFACES zpru_if_decision_provider.
  INTERFACES zpru_if_short_memory_provider.
  INTERFACES zpru_if_long_memory_provider.
  INTERFACES zpru_if_agent_info_provider.
  INTERFACES zpru_if_prompt_provider.
  INTERFACES zpru_if_agent_mapper.
  INTERFACES zpru_if_tool_provider.
  INTERFACES zpru_if_tool_schema_provider.
  INTERFACES zpru_if_tool_info_provider.
  INTERFACES zpru_if_agent_singleton_meth.

ENDINTERFACE.
