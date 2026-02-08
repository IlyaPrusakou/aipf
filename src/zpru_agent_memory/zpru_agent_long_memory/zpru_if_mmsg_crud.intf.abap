INTERFACE zpru_if_mmsg_crud
  PUBLIC .

  INTERFACES zpru_if_agent_frw .

  TYPES: BEGIN OF ts_mmsg_control,
           messageuuid TYPE abap_boolean,
           content      TYPE abap_boolean,
           messagetype TYPE abap_boolean,
           MessageContentId  TYPE abap_boolean,
           stage        TYPE abap_boolean,
           substage    TYPE abap_boolean,
           namespace    TYPE abap_boolean,
           username    TYPE abap_boolean,
           agentuuid   TYPE abap_boolean,
           runuuid     TYPE abap_boolean,
           queryuuid   TYPE abap_boolean,
           stepuuid    TYPE abap_boolean,
           MessageDateTime TYPE abap_boolean,
           createdby   TYPE abap_boolean,
           createdat   TYPE abap_boolean,
           changedby   TYPE abap_boolean,
           changedat   TYPE abap_boolean,
         END OF ts_mmsg_control.

  TYPES: BEGIN OF ts_mmsg_k,
           messageuuid TYPE sysuuid_x16,
         END OF ts_mmsg_k.

  TYPES tt_mmsg_k TYPE STANDARD TABLE OF ts_mmsg_k WITH EMPTY KEY.

  TYPES ts_mmsg        TYPE zpru_s_api_mem_msg.
  TYPES tt_mmsg        TYPE STANDARD TABLE OF ts_mmsg WITH EMPTY KEY.

  TYPES: BEGIN OF ts_mmsg_create_imp.
           INCLUDE TYPE zpru_s_api_mem_msg.
  TYPES:   control TYPE ts_mmsg_control.
  TYPES: END OF ts_mmsg_create_imp.

  TYPES tt_mmsg_create_imp TYPE STANDARD TABLE OF ts_mmsg_create_imp WITH EMPTY KEY.

  TYPES: BEGIN OF ts_mmsg_update_imp.
           INCLUDE TYPE zpru_s_api_mem_msg.
  TYPES:   control TYPE ts_mmsg_control.
  TYPES: END OF ts_mmsg_update_imp.

  TYPES tt_mmsg_update_imp TYPE STANDARD TABLE OF ts_mmsg_update_imp WITH EMPTY KEY.

  TYPES: BEGIN OF ts_mmsg_read_k,
           messageuuid TYPE sysuuid_x16,
           control      TYPE ts_mmsg_control,
         END OF ts_mmsg_read_k.

  TYPES tt_mmsg_read_k TYPE STANDARD TABLE OF ts_mmsg_read_k WITH EMPTY KEY.

  TYPES: BEGIN OF ts_mmsg_delete_imp,
           messageuuid TYPE sysuuid_x16,
         END OF ts_mmsg_delete_imp.

  TYPES tt_mmsg_delete_imp TYPE STANDARD TABLE OF ts_mmsg_delete_imp WITH EMPTY KEY.

ENDINTERFACE.
