CLASS zpru_cl_bcs_mail_message DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    TYPES ty_address         TYPE zpru_de_msg_sender.
    TYPES ty_subject         TYPE zpru_de_msg_subject.
    TYPES ty_status          TYPE zpru_de_msg_status.
    TYPES ty_status_response TYPE zpru_de_msg_response.

    TYPES: BEGIN OF tys_status,
             recipient       TYPE ty_address,
             status          TYPE ty_status,
             status_response TYPE ty_status_response,
           END OF tys_status.

    TYPES tyt_status TYPE STANDARD TABLE OF tys_status.

    DATA mv_sender    TYPE zpru_cl_bcs_mail_message=>ty_address.
    DATA mv_recipient TYPE zpru_cl_bcs_mail_message=>ty_address.
    DATA mv_subject   TYPE zpru_cl_bcs_mail_message=>ty_subject.
    DATA mo_main      TYPE REF TO zpru_cl_mail_bodypart.

    CLASS-METHODS create_instance
      RETURNING VALUE(ro_mail_message) TYPE REF TO zpru_cl_bcs_mail_message.

    METHODS set_sender
      IMPORTING iv_address TYPE ty_address.

    METHODS add_recipient
      IMPORTING iv_address TYPE ty_address.

    METHODS set_subject
      IMPORTING iv_subject TYPE ty_subject.

    METHODS set_main
      IMPORTING io_main TYPE REF TO zpru_cl_mail_bodypart.

    METHODS send
      EXPORTING et_status      TYPE tyt_status
                ev_mail_status TYPE ty_status.

ENDCLASS.


CLASS zpru_cl_bcs_mail_message IMPLEMENTATION.
  METHOD create_instance.
    ro_mail_message = NEW zpru_cl_bcs_mail_message( ).
  ENDMETHOD.

  METHOD set_sender.
    mv_sender = iv_address.
  ENDMETHOD.

  METHOD add_recipient.
    mv_recipient = iv_address.
  ENDMETHOD.

  METHOD set_subject.
    mv_subject = iv_subject.
  ENDMETHOD.

  METHOD set_main.
    mo_main = io_main.
  ENDMETHOD.

  METHOD send.
    ev_mail_status = 'S'.
  ENDMETHOD.
ENDCLASS.
