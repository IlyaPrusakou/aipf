CLASS zpru_cl_agent_service_mngr DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zpru_if_agent_frw.

    CLASS-METHODS get_service
      IMPORTING iv_service        TYPE zpru_de_seoclname
                iv_context        TYPE char100
      RETURNING VALUE(ro_service) TYPE REF TO zpru_if_agent_frw
      RAISING   zpru_cx_agent_core.

  PROTECTED SECTION.
    CLASS-DATA st_agent_serv TYPE STANDARD TABLE OF zpru_s_agent_serv WITH EMPTY KEY.
ENDCLASS.


CLASS zpru_cl_agent_service_mngr IMPLEMENTATION.
  METHOD get_service.
    DATA ls_service TYPE zpru_s_agent_serv.
    DATA lv_source  TYPE string.
    DATA lv_fields  TYPE string.
    DATA lv_where   TYPE string.

    ASSIGN st_agent_serv[ service = iv_service
                          context = iv_context ] TO FIELD-SYMBOL(<ls_agent_serv>).
    IF sy-subrc = 0.
      ls_service = <ls_agent_serv>.
    ELSE.

      lv_source = `zpru_agent_serv`.
      lv_fields = |service AS service,| &&
                  | context AS context,| &&
                  | class AS class,| &&
                  | created_by AS createdby,| &&
                  | created_at AS createdat,| &&
                  | changed_by AS changedby,| &&
                  | last_changed AS lastchanged|.
      lv_where = `service = @iv_service AND context = @iv_context`.

      SELECT SINGLE (lv_fields)
        FROM (lv_source)
        WHERE (lv_where)
        INTO CORRESPONDING FIELDS OF @ls_service.
      IF sy-subrc <> 0.
        RAISE EXCEPTION NEW zpru_cx_agent_core( ).
      ENDIF.

      INSERT ls_service INTO TABLE st_agent_serv.

    ENDIF.

    CREATE OBJECT ro_service TYPE (ls_service-class).
    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
