INTERFACE zpru_if_agsrv_precheck
  PUBLIC .


  INTERFACES zpru_if_agent_frw .

  METHODS precheck_create_agent_service
    IMPORTING it_agsrv_create_imp TYPE zpru_if_agsrv_crud=>tt_agsrv_create_imp
    EXPORTING et_entities         TYPE zpru_if_agsrv_crud=>tt_agsrv_create_imp
    CHANGING  cs_reported         TYPE zpru_if_agent_frw=>ts_agsrv_bndl_reported
              cs_failed           TYPE zpru_if_agent_frw=>ts_agsrv_bndl_failed.

  METHODS precheck_update_agent_service
    IMPORTING it_agsrv_update_imp TYPE zpru_if_agsrv_crud=>tt_agsrv_update_imp
    EXPORTING et_entities         TYPE zpru_if_agsrv_crud=>tt_agsrv_update_imp
    CHANGING  cs_reported         TYPE zpru_if_agent_frw=>ts_agsrv_bndl_reported
              cs_failed           TYPE zpru_if_agent_frw=>ts_agsrv_bndl_failed.

  METHODS precheck_delete_agent_service
    IMPORTING it_agsrv_delete_imp TYPE zpru_if_agsrv_crud=>tt_agsrv_delete_imp
    EXPORTING et_entities         TYPE zpru_if_agsrv_crud=>tt_agsrv_delete_imp
    CHANGING  cs_reported         TYPE zpru_if_agent_frw=>ts_agsrv_bndl_reported
              cs_failed           TYPE zpru_if_agent_frw=>ts_agsrv_bndl_failed.

  METHODS precheck_read_agent_service
    IMPORTING it_agsrv_read_k TYPE zpru_if_agsrv_crud=>tt_agsrv_read_k
    EXPORTING et_entities    TYPE zpru_if_agsrv_crud=>tt_agsrv_read_k
    CHANGING  cs_reported    TYPE zpru_if_agent_frw=>ts_agsrv_bndl_reported
              cs_failed      TYPE zpru_if_agent_frw=>ts_agsrv_bndl_failed.

ENDINTERFACE.
