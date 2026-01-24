CLASS zpru_cl_mail_bodypart DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS create_instance
      IMPORTING iv_content         TYPE string  OPTIONAL
                iv_content_type    TYPE char128 OPTIONAL
                iv_filename        TYPE string  OPTIONAL
      RETURNING VALUE(ro_instance) TYPE REF TO zpru_cl_mail_bodypart.

    DATA mv_content      TYPE string.
    DATA mv_content_type TYPE char128.
    DATA mv_filename     TYPE string.

ENDCLASS.


CLASS zpru_cl_mail_bodypart IMPLEMENTATION.
  METHOD create_instance.
    ro_instance = NEW zpru_cl_mail_bodypart( ).

    ro_instance->mv_content      = iv_content.
    ro_instance->mv_content_type = iv_content_type.
    ro_instance->mv_filename     = iv_filename.
  ENDMETHOD.
ENDCLASS.
