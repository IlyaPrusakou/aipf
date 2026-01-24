INTERFACE zpru_if_knowledge_provider
  PUBLIC .
  METHODS lookup_knowledge
    IMPORTING
      io_controller            TYPE REF TO zpru_if_agent_controller
      io_request               TYPE REF TO zpru_if_payload
      io_tool_schema_provider TYPE REF TO zpru_if_tool_schema_provider OPTIONAL
      io_tool_info_provider   TYPE REF TO zpru_if_tool_info_provider  OPTIONAL
    EXPORTING
      eo_response              TYPE REF TO zpru_if_payload
      ev_error_flag            TYPE abap_boolean.
ENDINTERFACE.
