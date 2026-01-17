CLASS zpru_cl_aic_completion_api_res DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
INTERFACES zpru_if_agent_frw.
    INTERFACES if_aic_completion_api_result .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zpru_cl_aic_completion_api_res IMPLEMENTATION.


  METHOD if_aic_completion_api_result~get_choices.
  ENDMETHOD.


  METHOD if_aic_completion_api_result~get_completion.
  ENDMETHOD.


  METHOD if_aic_completion_api_result~get_completion_token_count.
  ENDMETHOD.


  METHOD if_aic_completion_api_result~get_finish_reason.
  ENDMETHOD.


  METHOD if_aic_completion_api_result~get_original_finish_reason.
  ENDMETHOD.


  METHOD if_aic_completion_api_result~get_prompt_token_count.
  ENDMETHOD.


  METHOD if_aic_completion_api_result~get_response_format_refusal.
  ENDMETHOD.


  METHOD if_aic_completion_api_result~get_runtime_ms.
  ENDMETHOD.


  METHOD if_aic_completion_api_result~get_tool_calls.
  ENDMETHOD.


  METHOD if_aic_completion_api_result~get_total_token_count.
  ENDMETHOD.
ENDCLASS.
