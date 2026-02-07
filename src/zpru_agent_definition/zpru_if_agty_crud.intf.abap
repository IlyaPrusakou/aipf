INTERFACE zpru_if_agty_crud
  PUBLIC .


  INTERFACES zpru_if_agent_frw .

  TYPES: BEGIN OF ts_agty_control,
           agenttype         TYPE abap_boolean,
           shortmemvolume   TYPE abap_boolean,
           discardstrategy   TYPE abap_boolean,
           summarystrategy   TYPE abap_boolean,
           maxnumbloop      TYPE abap_boolean,
           createdby         TYPE abap_boolean,
           createdat         TYPE abap_boolean,
           changedby         TYPE abap_boolean,
           lastchanged       TYPE abap_boolean,
           locallastchanged TYPE abap_boolean,
         END OF ts_agty_control.

  TYPES: BEGIN OF ts_agty_k,
           agenttype TYPE zpru_de_agent_type,
         END OF ts_agty_k.

  TYPES tt_agty_k TYPE STANDARD TABLE OF ts_agty_k WITH EMPTY KEY.

  TYPES ts_agty        TYPE zpru_s_agent_type.
  TYPES tt_agty        TYPE STANDARD TABLE OF ts_agty WITH EMPTY KEY.

  TYPES: BEGIN OF ts_agty_create_imp.
           INCLUDE TYPE zpru_s_agent_type.
  TYPES:   control TYPE ts_agty_control.
  TYPES: END OF ts_agty_create_imp.

  TYPES tt_agty_create_imp TYPE STANDARD TABLE OF ts_agty_create_imp WITH EMPTY KEY.

  TYPES: BEGIN OF ts_agty_update_imp.
           INCLUDE TYPE zpru_s_agent_type.
  TYPES:   control TYPE ts_agty_control.
  TYPES: END OF ts_agty_update_imp.

  TYPES tt_agty_update_imp TYPE STANDARD TABLE OF ts_agty_update_imp WITH EMPTY KEY.

  TYPES: BEGIN OF ts_agty_read_k,
           agenttype TYPE zpru_de_agent_type,
           control    TYPE ts_agty_control,
         END OF ts_agty_read_k.

  TYPES tt_agty_read_k TYPE STANDARD TABLE OF ts_agty_read_k WITH EMPTY KEY.

  TYPES: BEGIN OF ts_agty_delete_imp,
           agenttype TYPE zpru_de_agent_type,
         END OF ts_agty_delete_imp.

  TYPES tt_agty_delete_imp TYPE STANDARD TABLE OF ts_agty_delete_imp WITH EMPTY KEY.

ENDINTERFACE.
