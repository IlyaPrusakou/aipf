CLASS zpru_cl_agent_service_mngr DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES zpru_if_agent_frw.
    CLASS-METHODS set_context
      IMPORTING
        iv_context TYPE char100.
    CLASS-METHODS get_context
      RETURNING VALUE(rv_context) TYPE char100.
    CLASS-METHODS get_service
      IMPORTING
                iv_service        TYPE zpru_de_seoclname
      RETURNING VALUE(ro_service) TYPE REF TO zpru_if_agent_frw
      RAISING   zpru_cx_agent_core.
  PROTECTED SECTION.
    CLASS-DATA: sv_context TYPE char100 VALUE `STANDARD`.
  PRIVATE SECTION.
ENDCLASS.



CLASS zpru_cl_agent_service_mngr IMPLEMENTATION.
  METHOD get_context.
  " STANDARD_PERSISTENCE_MESSAGE
    " STANDARD_PERSISTENCE_SUMMARIZE
      " STANDARD_SUMMARIZE
      " STANDARD_DISCARD_STRATEGY_DELETE
      " STANDARD_DISCARD_STRATEGY_SUMMARIZE
      " STANDARD_DISCARD_STRATEGY_SAVE
    rv_context =   sv_context.
  ENDMETHOD.

  METHOD get_service.

    SELECT SINGLE * FROM zpru_agent_serv
    WHERE service = @iv_service AND
    context = @sv_context
    INTO @DATA(ls_service).
    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDIF.

    CREATE OBJECT ro_service TYPE (ls_service-class).
    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDIF.

  ENDMETHOD.

  METHOD set_context.
    sv_context = iv_context.
  ENDMETHOD.

ENDCLASS.
