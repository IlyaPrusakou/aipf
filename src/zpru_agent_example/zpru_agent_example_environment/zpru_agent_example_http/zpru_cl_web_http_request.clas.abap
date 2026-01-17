CLASS zpru_cl_web_http_request DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
INTERFACES zpru_if_agent_frw.
    INTERFACES if_web_http_request .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zpru_cl_web_http_request IMPLEMENTATION.


  METHOD if_web_http_request~add_multipart.
  ENDMETHOD.


  METHOD if_web_http_request~append_binary.
  ENDMETHOD.


  METHOD if_web_http_request~append_text.
  ENDMETHOD.


  METHOD if_web_http_request~delete_header_field.
  ENDMETHOD.


  METHOD if_web_http_request~from_xstring.
  ENDMETHOD.


  METHOD if_web_http_request~get_binary.
  ENDMETHOD.


  METHOD if_web_http_request~get_content_type.
  ENDMETHOD.


  METHOD if_web_http_request~get_cookie.
  ENDMETHOD.


  METHOD if_web_http_request~get_cookies.
  ENDMETHOD.


  METHOD if_web_http_request~get_data_length.
  ENDMETHOD.


  METHOD if_web_http_request~get_form_field.
  ENDMETHOD.


  METHOD if_web_http_request~get_form_fields.
  ENDMETHOD.


  METHOD if_web_http_request~get_form_fields_cs.
  ENDMETHOD.


  METHOD if_web_http_request~get_form_field_cs.
  ENDMETHOD.


  METHOD if_web_http_request~get_header_field.
  ENDMETHOD.


  METHOD if_web_http_request~get_header_fields.
  ENDMETHOD.


  METHOD if_web_http_request~get_last_error.
  ENDMETHOD.


  METHOD if_web_http_request~get_method.
  ENDMETHOD.


  METHOD if_web_http_request~get_multipart.
  ENDMETHOD.


  METHOD if_web_http_request~get_text.
  ENDMETHOD.


  METHOD if_web_http_request~num_multiparts.
  ENDMETHOD.


  METHOD if_web_http_request~set_authorization_basic.
  ENDMETHOD.


  METHOD if_web_http_request~set_authorization_bearer.
  ENDMETHOD.


  METHOD if_web_http_request~set_binary.
  ENDMETHOD.


  METHOD if_web_http_request~set_content_type.
  ENDMETHOD.


  METHOD if_web_http_request~set_cookie.
  ENDMETHOD.


  METHOD if_web_http_request~set_formfield_encoding.
  ENDMETHOD.


  METHOD if_web_http_request~set_form_field.
  ENDMETHOD.


  METHOD if_web_http_request~set_form_fields.
  ENDMETHOD.


  METHOD if_web_http_request~set_header_field.
  ENDMETHOD.


  METHOD if_web_http_request~set_header_fields.
  ENDMETHOD.


  METHOD if_web_http_request~set_query.
  ENDMETHOD.


  METHOD if_web_http_request~set_text.
  ENDMETHOD.


  METHOD if_web_http_request~set_uri_path.
  ENDMETHOD.


  METHOD if_web_http_request~set_version.
  ENDMETHOD.


  METHOD if_web_http_request~to_xstring.
  ENDMETHOD.
ENDCLASS.
