INTERFACE zpru_if_msum_crud
  PUBLIC.

  INTERFACES zpru_if_agent_frw.

  TYPES: BEGIN OF ts_msum_control,
           summaryuuid TYPE abap_boolean,
           content      TYPE abap_boolean,
           SummaryContentId  TYPE abap_boolean,
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
         END OF ts_msum_control.

  TYPES: BEGIN OF ts_msum_k,
           summaryuuid TYPE sysuuid_x16,
         END OF ts_msum_k.

  TYPES tt_msum_k TYPE STANDARD TABLE OF ts_msum_k WITH EMPTY KEY.

  TYPES ts_msum   TYPE zpru_S_API_mem_sum.
  TYPES tt_msum   TYPE STANDARD TABLE OF ts_msum WITH EMPTY KEY.

  TYPES: BEGIN OF ts_msum_create_imp.
           INCLUDE TYPE zpru_S_API_mem_sum.
  TYPES:   control TYPE ts_msum_control.
  TYPES: END OF ts_msum_create_imp.

  TYPES tt_msum_create_imp TYPE STANDARD TABLE OF ts_msum_create_imp WITH EMPTY KEY.

  TYPES: BEGIN OF ts_msum_update_imp.
           INCLUDE TYPE zpru_S_API_mem_sum.
  TYPES:   control TYPE ts_msum_control.
  TYPES: END OF ts_msum_update_imp.

  TYPES tt_msum_update_imp TYPE STANDARD TABLE OF ts_msum_update_imp WITH EMPTY KEY.

  TYPES: BEGIN OF ts_msum_read_k,
           summaryuuid TYPE sysuuid_x16,
           control      TYPE ts_msum_control,
         END OF ts_msum_read_k.

  TYPES tt_msum_read_k TYPE STANDARD TABLE OF ts_msum_read_k WITH EMPTY KEY.

  TYPES: BEGIN OF ts_msum_delete_imp,
           summaryuuid TYPE sysuuid_x16,
         END OF ts_msum_delete_imp.

  TYPES tt_msum_delete_imp TYPE STANDARD TABLE OF ts_msum_delete_imp WITH EMPTY KEY.

ENDINTERFACE.
