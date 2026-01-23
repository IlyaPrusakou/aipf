CLASS zpru_cl_nested_http DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zpru_if_tool_provider.
    INTERFACES zpru_if_tool_executor.
    INTERFACES zpru_if_http_request_sender.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zpru_cl_nested_http IMPLEMENTATION.


  METHOD zpru_if_tool_provider~get_tool.
    ro_executor = me.
  ENDMETHOD.

  METHOD zpru_if_http_request_sender~send_http.
    " do some http
  ENDMETHOD.

ENDCLASS.
