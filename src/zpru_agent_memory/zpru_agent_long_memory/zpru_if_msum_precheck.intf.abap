INTERFACE zpru_if_msum_precheck
  PUBLIC.

  INTERFACES zpru_if_agent_frw.

  METHODS precheck_create_msum
    IMPORTING it_msum_create_imp TYPE zpru_if_msum_crud=>tt_msum_create_imp
    EXPORTING et_entities        TYPE zpru_if_msum_crud=>tt_msum_create_imp
    CHANGING  cs_reported        TYPE zpru_if_agent_frw=>ts_msum_bndl_reported
              cs_failed          TYPE zpru_if_agent_frw=>ts_msum_bndl_failed.

  METHODS precheck_update_msum
    IMPORTING it_msum_update_imp TYPE zpru_if_msum_crud=>tt_msum_update_imp
    EXPORTING et_entities        TYPE zpru_if_msum_crud=>tt_msum_update_imp
    CHANGING  cs_reported        TYPE zpru_if_agent_frw=>ts_msum_bndl_reported
              cs_failed          TYPE zpru_if_agent_frw=>ts_msum_bndl_failed.

  METHODS precheck_delete_msum
    IMPORTING it_msum_delete_imp TYPE zpru_if_msum_crud=>tt_msum_delete_imp
    EXPORTING et_entities        TYPE zpru_if_msum_crud=>tt_msum_delete_imp
    CHANGING  cs_reported        TYPE zpru_if_agent_frw=>ts_msum_bndl_reported
              cs_failed          TYPE zpru_if_agent_frw=>ts_msum_bndl_failed.

  METHODS precheck_read_msum
    IMPORTING it_msum_read_k TYPE zpru_if_msum_crud=>tt_msum_read_k
    EXPORTING et_entities    TYPE zpru_if_msum_crud=>tt_msum_read_k
    CHANGING  cs_reported    TYPE zpru_if_agent_frw=>ts_msum_bndl_reported
              cs_failed      TYPE zpru_if_agent_frw=>ts_msum_bndl_failed.

ENDINTERFACE.
