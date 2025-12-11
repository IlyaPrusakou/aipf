INTERFACE zpru_if_axc_factory
  PUBLIC.

  CLASS-METHODS get_zpru_if_axc_service
    RETURNING VALUE(ro_obj) TYPE REF TO zpru_if_axc_service.

  CLASS-METHODS get_zpru_if_axc_precheck
    RETURNING VALUE(ro_obj) TYPE REF TO zpru_if_axc_precheck.

  CLASS-METHODS get_zpru_if_axc_db_access
    RETURNING VALUE(ro_obj) TYPE REF TO zpru_if_axc_database_access.



ENDINTERFACE.
