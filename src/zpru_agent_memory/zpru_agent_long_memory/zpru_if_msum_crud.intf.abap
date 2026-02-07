INTERFACE zpru_if_msum_crud
  PUBLIC.

  INTERFACES zpru_if_agent_frw.

  TYPES: BEGIN OF ts_msum_control,
           summary_uuid TYPE abap_boolean,
           content      TYPE abap_boolean,
           summary_cid  TYPE abap_boolean,
           stage        TYPE abap_boolean,
           sub_stage    TYPE abap_boolean,
           namespace    TYPE abap_boolean,
           user_name    TYPE abap_boolean,
           agent_uuid   TYPE abap_boolean,
           run_uuid     TYPE abap_boolean,
           query_uuid   TYPE abap_boolean,
           step_uuid    TYPE abap_boolean,
           message_time TYPE abap_boolean,
           created_by   TYPE abap_boolean,
           created_at   TYPE abap_boolean,
           changed_by   TYPE abap_boolean,
           changed_at   TYPE abap_boolean,
         END OF ts_msum_control.

  TYPES: BEGIN OF ts_msum_k,
           summary_uuid TYPE sysuuid_x16,
         END OF ts_msum_k.

  TYPES tt_msum_k TYPE STANDARD TABLE OF ts_msum_k WITH EMPTY KEY.

  TYPES ts_msum   TYPE zpru_mem_sum.
  TYPES tt_msum   TYPE STANDARD TABLE OF ts_msum WITH EMPTY KEY.

  TYPES: BEGIN OF ts_msum_create_imp.
           INCLUDE TYPE zpru_mem_sum.
  TYPES:   control TYPE ts_msum_control.
  TYPES: END OF ts_msum_create_imp.

  TYPES tt_msum_create_imp TYPE STANDARD TABLE OF ts_msum_create_imp WITH EMPTY KEY.

  TYPES: BEGIN OF ts_msum_update_imp.
           INCLUDE TYPE zpru_mem_sum.
  TYPES:   control TYPE ts_msum_control.
  TYPES: END OF ts_msum_update_imp.

  TYPES tt_msum_update_imp TYPE STANDARD TABLE OF ts_msum_update_imp WITH EMPTY KEY.

  TYPES: BEGIN OF ts_msum_read_k,
           summary_uuid TYPE sysuuid_x16,
           control      TYPE ts_msum_control,
         END OF ts_msum_read_k.

  TYPES tt_msum_read_k TYPE STANDARD TABLE OF ts_msum_read_k WITH EMPTY KEY.

  TYPES: BEGIN OF ts_msum_delete_imp,
           summary_uuid TYPE sysuuid_x16,
         END OF ts_msum_delete_imp.

  TYPES tt_msum_delete_imp TYPE STANDARD TABLE OF ts_msum_delete_imp WITH EMPTY KEY.

ENDINTERFACE.
