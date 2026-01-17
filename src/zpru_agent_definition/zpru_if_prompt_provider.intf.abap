INTERFACE zpru_if_prompt_provider
  PUBLIC .


  METHODS get_system_prompt
    RETURNING VALUE(rv_system_prompt) TYPE zpru_if_agent_frw=>ts_json.
ENDINTERFACE.
