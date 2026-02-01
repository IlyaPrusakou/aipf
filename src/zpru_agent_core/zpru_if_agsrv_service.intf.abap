INTERFACE zpru_if_agsrv_service
  PUBLIC .


  INTERFACES zpru_if_agent_frw .

  TYPES tt_service            TYPE RANGE OF zpru_de_seoclname.
  TYPES tt_context            TYPE RANGE OF char100.
  TYPES tt_class              TYPE RANGE OF zpru_de_seoclname.
  TYPES tt_created_by         TYPE RANGE OF syuname.
  TYPES tt_created_at         TYPE RANGE OF timestampl.
  TYPES tt_changed_by         TYPE RANGE OF syuname.
  TYPES tt_last_changed       TYPE RANGE OF abp_lastchange_tstmpl.
  TYPES tt_local_last_changed TYPE RANGE OF abp_locinst_lastchange_tstmpl.

  METHODS query_agent_service
    IMPORTING it_service            TYPE tt_service            OPTIONAL
              it_context            TYPE tt_context            OPTIONAL
              it_class              TYPE tt_class              OPTIONAL
              it_created_by         TYPE tt_created_by         OPTIONAL
              it_created_at         TYPE tt_created_at         OPTIONAL
              it_changed_by         TYPE tt_changed_by         OPTIONAL
              it_last_changed       TYPE tt_last_changed       OPTIONAL
              it_local_last_changed TYPE tt_local_last_changed OPTIONAL
    EXPORTING et_agsrv_k            TYPE zpru_if_agsrv_crud=>tt_agsrv_k.

  METHODS read_agent_service
    IMPORTING it_agsrv_read_k TYPE zpru_if_agsrv_crud=>tt_agsrv_read_k
    EXPORTING et_agsrv        TYPE zpru_if_agsrv_crud=>tt_agsrv
    CHANGING  cs_reported     TYPE zpru_if_agent_frw=>ts_agsrv_bndl_reported OPTIONAL
              cs_failed       TYPE zpru_if_agent_frw=>ts_agsrv_bndl_failed   OPTIONAL.

  METHODS create_agent_service
    IMPORTING it_agsrv_create_imp TYPE zpru_if_agsrv_crud=>tt_agsrv_create_imp
    CHANGING  cs_reported         TYPE zpru_if_agent_frw=>ts_agsrv_bndl_reported OPTIONAL
              cs_failed           TYPE zpru_if_agent_frw=>ts_agsrv_bndl_failed   OPTIONAL
              cs_mapped           TYPE zpru_if_agent_frw=>ts_agsrv_bndl_mapped   OPTIONAL.

  METHODS update_agent_service
    IMPORTING it_agsrv_update_imp TYPE zpru_if_agsrv_crud=>tt_agsrv_update_imp
    CHANGING  cs_reported         TYPE zpru_if_agent_frw=>ts_agsrv_bndl_reported OPTIONAL
              cs_failed           TYPE zpru_if_agent_frw=>ts_agsrv_bndl_failed   OPTIONAL.

  METHODS delete_agent_service
    IMPORTING it_agsrv_delete_imp TYPE zpru_if_agsrv_crud=>tt_agsrv_delete_imp
    CHANGING  cs_reported         TYPE zpru_if_agent_frw=>ts_agsrv_bndl_reported OPTIONAL
              cs_failed           TYPE zpru_if_agent_frw=>ts_agsrv_bndl_failed   OPTIONAL.

  METHODS determine
    CHANGING cs_reported TYPE zpru_if_agent_frw=>ts_agsrv_bndl_reported OPTIONAL
             cs_failed   TYPE zpru_if_agent_frw=>ts_agsrv_bndl_failed   OPTIONAL
             cs_mapped   TYPE zpru_if_agent_frw=>ts_agsrv_bndl_mapped   OPTIONAL.

  METHODS validate
    IMPORTING it_check_agent_service_v TYPE zpru_if_agsrv_crud=>tt_agsrv_read_k OPTIONAL
    CHANGING  cs_reported              TYPE zpru_if_agent_frw=>ts_agsrv_bndl_reported OPTIONAL
              cs_failed                TYPE zpru_if_agent_frw=>ts_agsrv_bndl_failed   OPTIONAL.

  METHODS clean_up
    CHANGING cs_mapped TYPE zpru_if_agent_frw=>ts_agsrv_bndl_mapped OPTIONAL.

  METHODS do_save
    IMPORTING iv_do_commit TYPE abap_boolean                              DEFAULT abap_true
    CHANGING  cs_reported  TYPE zpru_if_agent_frw=>ts_agsrv_bndl_reported OPTIONAL
              cs_failed    TYPE zpru_if_agent_frw=>ts_agsrv_bndl_failed   OPTIONAL
              cs_mapped    TYPE zpru_if_agent_frw=>ts_agsrv_bndl_mapped   OPTIONAL.

ENDINTERFACE.
