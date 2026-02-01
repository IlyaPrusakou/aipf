INTERFACE zpru_if_agsrv_crud
  PUBLIC .


  INTERFACES zpru_if_agent_frw .

  TYPES: BEGIN OF ts_agsrv_control,
           service            TYPE abap_boolean,
           context            TYPE abap_boolean,
           class              TYPE abap_boolean,
           created_by         TYPE abap_boolean,
           created_at         TYPE abap_boolean,
           changed_by         TYPE abap_boolean,
           last_changed       TYPE abap_boolean,
           local_last_changed TYPE abap_boolean,
         END OF ts_agsrv_control.

  TYPES: BEGIN OF ts_agsrv_k,
           service TYPE zpru_de_seoclname,
           context TYPE char100,
         END OF ts_agsrv_k.

  TYPES tt_agsrv_k TYPE STANDARD TABLE OF ts_agsrv_k WITH EMPTY KEY.

  TYPES ts_agsrv        TYPE zpru_agent_serv.
  TYPES tt_agsrv        TYPE STANDARD TABLE OF ts_agsrv WITH EMPTY KEY.

  TYPES: BEGIN OF ts_agsrv_create_imp.
           INCLUDE TYPE zpru_agent_serv.
  TYPES:   control TYPE ts_agsrv_control.
  TYPES: END OF ts_agsrv_create_imp.

  TYPES tt_agsrv_create_imp TYPE STANDARD TABLE OF ts_agsrv_create_imp WITH EMPTY KEY.

  TYPES: BEGIN OF ts_agsrv_update_imp.
           INCLUDE TYPE zpru_agent_serv.
  TYPES:   control TYPE ts_agsrv_control.
  TYPES: END OF ts_agsrv_update_imp.

  TYPES tt_agsrv_update_imp TYPE STANDARD TABLE OF ts_agsrv_update_imp WITH EMPTY KEY.

  TYPES: BEGIN OF ts_agsrv_read_k,
           service TYPE zpru_de_seoclname,
           context TYPE char100,
           control TYPE ts_agsrv_control,
         END OF ts_agsrv_read_k.

  TYPES tt_agsrv_read_k TYPE STANDARD TABLE OF ts_agsrv_read_k WITH EMPTY KEY.

  TYPES: BEGIN OF ts_agsrv_delete_imp,
           service TYPE zpru_de_seoclname,
           context TYPE char100,
         END OF ts_agsrv_delete_imp.

  TYPES tt_agsrv_delete_imp TYPE STANDARD TABLE OF ts_agsrv_delete_imp WITH EMPTY KEY.

ENDINTERFACE.
