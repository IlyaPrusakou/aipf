CLASS zpru_cl_web_http_response DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
INTERFACES zpru_if_agent_frw.
    INTERFACES if_web_http_response .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zpru_cl_web_http_response IMPLEMENTATION.


  METHOD if_web_http_response~add_multipart.
  ENDMETHOD.


  METHOD if_web_http_response~delete_cookie_at_client.
  ENDMETHOD.


  METHOD if_web_http_response~delete_header_field.
  ENDMETHOD.


  METHOD if_web_http_response~from_xstring.
  ENDMETHOD.


  METHOD if_web_http_response~get_available_sse_messages.
  ENDMETHOD.


  METHOD if_web_http_response~get_binary.
  ENDMETHOD.


  METHOD if_web_http_response~get_content_type.
  ENDMETHOD.


  METHOD if_web_http_response~get_cookie.
  ENDMETHOD.


  METHOD if_web_http_response~get_cookies.
  ENDMETHOD.


  METHOD if_web_http_response~get_data_length.
  ENDMETHOD.


  METHOD if_web_http_response~get_header_field.
  ENDMETHOD.


  METHOD if_web_http_response~get_header_fields.
  ENDMETHOD.


  METHOD if_web_http_response~get_last_error.
  ENDMETHOD.


  METHOD if_web_http_response~get_multipart.
  ENDMETHOD.


  METHOD if_web_http_response~get_next_sse_message.
  ENDMETHOD.


  METHOD if_web_http_response~get_status.
  ENDMETHOD.


  METHOD if_web_http_response~get_text.
  ENDMETHOD.


  METHOD if_web_http_response~num_multiparts.
  ENDMETHOD.


  METHOD if_web_http_response~redirect.
  ENDMETHOD.


  METHOD if_web_http_response~server_cache_expire_rel.
  ENDMETHOD.


  METHOD if_web_http_response~set_binary.
  ENDMETHOD.


  METHOD if_web_http_response~set_compression.
  ENDMETHOD.


  METHOD if_web_http_response~set_content_type.
  ENDMETHOD.


  METHOD if_web_http_response~set_cookie.
  ENDMETHOD.


  METHOD if_web_http_response~set_formfield_encoding.
  ENDMETHOD.


  METHOD if_web_http_response~set_header_field.
  ENDMETHOD.


  METHOD if_web_http_response~set_header_fields.
  ENDMETHOD.


  METHOD if_web_http_response~set_status.
  ENDMETHOD.


  METHOD if_web_http_response~set_text.
  ENDMETHOD.


  METHOD if_web_http_response~suppress_content_type.
  ENDMETHOD.


  METHOD if_web_http_response~to_xstring.
  ENDMETHOD.
ENDCLASS.
