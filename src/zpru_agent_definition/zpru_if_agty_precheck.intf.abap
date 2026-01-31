INTERFACE zpru_if_agty_precheck
  PUBLIC .


  INTERFACES zpru_if_agent_frw .

  METHODS precheck_create_agent_type
    IMPORTING it_agty_create_imp TYPE zpru_if_agty_crud=>tt_agty_create_imp
    EXPORTING et_entities        TYPE zpru_if_agty_crud=>tt_agty_create_imp
    CHANGING  cs_reported        TYPE zpru_if_agent_frw=>ts_agty_bndl_reported
              cs_failed          TYPE zpru_if_agent_frw=>ts_agty_bndl_failed.

  METHODS precheck_update_agent_type
    IMPORTING it_agty_update_imp TYPE zpru_if_agty_crud=>tt_agty_update_imp
    EXPORTING et_entities        TYPE zpru_if_agty_crud=>tt_agty_update_imp
    CHANGING  cs_reported        TYPE zpru_if_agent_frw=>ts_agty_bndl_reported
              cs_failed          TYPE zpru_if_agent_frw=>ts_agty_bndl_failed.

  METHODS precheck_delete_agent_type
    IMPORTING it_agty_delete_imp TYPE zpru_if_agty_crud=>tt_agty_delete_imp
    EXPORTING et_entities        TYPE zpru_if_agty_crud=>tt_agty_delete_imp
    CHANGING  cs_reported        TYPE zpru_if_agent_frw=>ts_agty_bndl_reported
              cs_failed          TYPE zpru_if_agent_frw=>ts_agty_bndl_failed.

  METHODS precheck_read_agent_type
    IMPORTING it_agty_read_k TYPE zpru_if_agty_crud=>tt_agty_read_k
    EXPORTING et_entities    TYPE zpru_if_agty_crud=>tt_agty_read_k
    CHANGING  cs_reported    TYPE zpru_if_agent_frw=>ts_agty_bndl_reported
              cs_failed      TYPE zpru_if_agent_frw=>ts_agty_bndl_failed.
ENDINTERFACE.
