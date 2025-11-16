INTERFACE zpru_if_tool_executor
  PUBLIC .

  METHODS execute_tool
    IMPORTING
      io_controller type ref to zpru_if_agent_controller
      io_request  TYPE REF TO zpru_if_request
    EXPORTING
      eo_response TYPE REF TO zpru_if_response.

ENDINTERFACE.
