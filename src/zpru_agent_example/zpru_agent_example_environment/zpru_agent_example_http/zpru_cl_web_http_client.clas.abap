CLASS zpru_cl_web_http_client DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
INTERFACES zpru_if_agent_frw.
    INTERFACES if_web_http_client .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zpru_cl_web_http_client IMPLEMENTATION.


  METHOD if_web_http_client~accept_cookies.
  ENDMETHOD.


  METHOD if_web_http_client~close.
  ENDMETHOD.


  METHOD if_web_http_client~enable_path_prefix.
  ENDMETHOD.


  METHOD if_web_http_client~execute.
    r_response = NEW zpru_cl_web_http_response( ).
    TRY.
        r_response->set_status( i_code   = 200
                                i_reason = `SUCCESS` ).
        r_response->set_text( `{ "http_result" : "SUCESS" }` ).
      CATCH cx_web_message_error INTO DATA(lo_web_message_error).
        RAISE EXCEPTION NEW cx_web_http_client_error( previous = lo_web_message_error ).
    ENDTRY.
  ENDMETHOD.


  METHOD if_web_http_client~get_distributed_tracing_status.
  ENDMETHOD.


  METHOD if_web_http_client~get_form_field_policy.
  ENDMETHOD.


  METHOD if_web_http_client~get_http_request.
    r_http_request = NEW zpru_cl_web_http_request( ).
  ENDMETHOD.


  METHOD if_web_http_client~get_logon_popup_policy.
  ENDMETHOD.


  METHOD if_web_http_client~get_redirect_policy.
  ENDMETHOD.


  METHOD if_web_http_client~retry_execute.

  ENDMETHOD.


  METHOD if_web_http_client~set_authn_mode.
  ENDMETHOD.


  METHOD if_web_http_client~set_csrf_token.
  ENDMETHOD.


  METHOD if_web_http_client~set_distributed_tracing_status.
  ENDMETHOD.


  METHOD if_web_http_client~set_form_field_policy.
  ENDMETHOD.


  METHOD if_web_http_client~set_logon_popup_policy.
  ENDMETHOD.


  METHOD if_web_http_client~set_redirect_policy.
  ENDMETHOD.
ENDCLASS.
