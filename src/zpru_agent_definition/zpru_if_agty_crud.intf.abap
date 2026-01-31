INTERFACE zpru_if_agty_crud
  PUBLIC .


  INTERFACES zpru_if_agent_frw .

  TYPES: BEGIN OF ts_agty_control,
           agent_type         TYPE abap_boolean,
           short_mem_volume   TYPE abap_boolean,
           discard_strategy   TYPE abap_boolean,
           summary_strategy   TYPE abap_boolean,
           max_numb_loop      TYPE abap_boolean,
           created_by         TYPE abap_boolean,
           created_at         TYPE abap_boolean,
           changed_by         TYPE abap_boolean,
           last_changed       TYPE abap_boolean,
           local_last_changed TYPE abap_boolean,
         END OF ts_agty_control.

  TYPES: BEGIN OF ts_agty_k,
           agent_type TYPE zpru_de_agent_type,
         END OF ts_agty_k.

  TYPES tt_agty_k TYPE STANDARD TABLE OF ts_agty_k WITH EMPTY KEY.

  TYPES ts_agty        TYPE zpru_agent_type.
  TYPES tt_agty        TYPE STANDARD TABLE OF ts_agty WITH EMPTY KEY.

  TYPES: BEGIN OF ts_agty_create_imp.
           INCLUDE TYPE zpru_agent_type.
  TYPES:   control TYPE ts_agty_control.
  TYPES: END OF ts_agty_create_imp.

  TYPES tt_agty_create_imp TYPE STANDARD TABLE OF ts_agty_create_imp WITH EMPTY KEY.

  TYPES: BEGIN OF ts_agty_update_imp.
           INCLUDE TYPE zpru_agent_type.
  TYPES:   control TYPE ts_agty_control.
  TYPES: END OF ts_agty_update_imp.

  TYPES tt_agty_update_imp TYPE STANDARD TABLE OF ts_agty_update_imp WITH EMPTY KEY.

  TYPES: BEGIN OF ts_agty_read_k,
           agent_type TYPE zpru_de_agent_type,
           control    TYPE ts_agty_control,
         END OF ts_agty_read_k.

  TYPES tt_agty_read_k TYPE STANDARD TABLE OF ts_agty_read_k WITH EMPTY KEY.

  TYPES: BEGIN OF ts_agty_delete_imp,
           agent_type TYPE zpru_de_agent_type,
         END OF ts_agty_delete_imp.

  TYPES tt_agty_delete_imp TYPE STANDARD TABLE OF ts_agty_delete_imp WITH EMPTY KEY.

ENDINTERFACE.
