INTERFACE zpru_if_agty_service
  PUBLIC .


  INTERFACES zpru_if_agent_frw .

  TYPES tt_agent_type       TYPE RANGE OF zpru_de_agent_type.
  TYPES tt_short_mem_volume TYPE RANGE OF zpru_de_mem_volume.
  TYPES tt_discard_strategy TYPE RANGE OF zpru_de_discard_strategy.
  TYPES tt_summary_strategy TYPE RANGE OF zpru_de_summary_strategy.
  TYPES tt_max_numb_loop    TYPE RANGE OF int4.
  TYPES tt_created_by       TYPE RANGE OF syuname.
  TYPES tt_created_at       TYPE RANGE OF timestampl.
  TYPES tt_changed_by       TYPE RANGE OF syuname.
  TYPES tt_last_changed     TYPE RANGE OF abp_lastchange_tstmpl.

  METHODS query_agent_type
    IMPORTING it_agent_type       TYPE tt_agent_type       OPTIONAL
              it_short_mem_volume TYPE tt_short_mem_volume OPTIONAL
              it_discard_strategy TYPE tt_discard_strategy OPTIONAL
              it_summary_strategy TYPE tt_summary_strategy OPTIONAL
              it_max_numb_loop    TYPE tt_max_numb_loop    OPTIONAL
              it_created_by       TYPE tt_created_by       OPTIONAL
              it_created_at       TYPE tt_created_at       OPTIONAL
              it_changed_by       TYPE tt_changed_by       OPTIONAL
              it_last_changed     TYPE tt_last_changed     OPTIONAL
    EXPORTING et_agty_k           TYPE zpru_if_agty_crud=>tt_agty_k.

  METHODS read_agent_type
    IMPORTING it_agty_read_k TYPE zpru_if_agty_crud=>tt_agty_read_k
    EXPORTING et_agty        TYPE zpru_if_agty_crud=>tt_agty
    CHANGING  cs_reported    TYPE zpru_if_agent_frw=>ts_agty_bndl_reported OPTIONAL
              cs_failed      TYPE zpru_if_agent_frw=>ts_agty_bndl_failed   OPTIONAL.

  METHODS create_agent_type
    IMPORTING it_agty_create_imp TYPE zpru_if_agty_crud=>tt_agty_create_imp
    CHANGING  cs_reported        TYPE zpru_if_agent_frw=>ts_agty_bndl_reported OPTIONAL
              cs_failed          TYPE zpru_if_agent_frw=>ts_agty_bndl_failed   OPTIONAL
              cs_mapped          TYPE zpru_if_agent_frw=>ts_agty_bndl_mapped   OPTIONAL.

  METHODS update_agent_type
    IMPORTING it_agty_update_imp TYPE zpru_if_agty_crud=>tt_agty_update_imp
    CHANGING  cs_reported        TYPE zpru_if_agent_frw=>ts_agty_bndl_reported OPTIONAL
              cs_failed          TYPE zpru_if_agent_frw=>ts_agty_bndl_failed   OPTIONAL.

  METHODS delete_agent_type
    IMPORTING it_agty_delete_imp TYPE zpru_if_agty_crud=>tt_agty_delete_imp
    CHANGING  cs_reported        TYPE zpru_if_agent_frw=>ts_agty_bndl_reported OPTIONAL
              cs_failed          TYPE zpru_if_agent_frw=>ts_agty_bndl_failed   OPTIONAL.

  METHODS determine
    CHANGING cs_reported TYPE zpru_if_agent_frw=>ts_agty_bndl_reported OPTIONAL
             cs_failed   TYPE zpru_if_agent_frw=>ts_agty_bndl_failed   OPTIONAL
             cs_mapped   TYPE zpru_if_agent_frw=>ts_agty_bndl_mapped   OPTIONAL.

  METHODS validate
    IMPORTING it_check_agent_type_v TYPE zpru_if_agty_crud=>tt_agty_read_k OPTIONAL
    CHANGING  cs_reported           TYPE zpru_if_agent_frw=>ts_agty_bndl_reported OPTIONAL
              cs_failed             TYPE zpru_if_agent_frw=>ts_agty_bndl_failed   OPTIONAL.

  METHODS clean_up
    CHANGING cs_mapped TYPE zpru_if_agent_frw=>ts_agty_bndl_mapped OPTIONAL.

  METHODS do_save
    IMPORTING iv_do_commit TYPE abap_boolean                              DEFAULT abap_true
    CHANGING  cs_reported  TYPE zpru_if_agent_frw=>ts_agty_bndl_reported OPTIONAL
              cs_failed    TYPE zpru_if_agent_frw=>ts_agty_bndl_failed   OPTIONAL
              cs_mapped    TYPE zpru_if_agent_frw=>ts_agty_bndl_mapped   OPTIONAL.

ENDINTERFACE.
