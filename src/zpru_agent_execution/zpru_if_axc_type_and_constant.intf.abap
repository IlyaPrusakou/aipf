INTERFACE zpru_if_axc_type_and_constant
  PUBLIC.

  CONSTANTS:
    BEGIN OF sc_step_status,
      new      TYPE zpru_de_axc_step_status VALUE 'N',
      error    TYPE zpru_de_axc_step_status VALUE 'E',
      complete TYPE zpru_de_axc_step_status VALUE 'C',
    END OF sc_step_status.

  CONSTANTS:
    BEGIN OF sc_query_status,
      new      TYPE zpru_de_axc_query_status VALUE 'N',
      error    TYPE zpru_de_axc_query_status VALUE 'E',
      complete TYPE zpru_de_axc_query_status VALUE 'C',
    END OF sc_query_status.

  TYPES: BEGIN OF ts_json_parameter,
           name  TYPE zpru_de_seopnampas,
           type  TYPE string,
           value TYPE string,
         END OF ts_json_parameter.

  TYPES tt_json_parameters TYPE STANDARD TABLE OF ts_json_parameter WITH EMPTY KEY.

  TYPES: BEGIN OF ts_tool_invocation,
           logarea    TYPE char20,
           classname  TYPE zpru_de_seoclname,
           methodname TYPE zpru_de_seomtdname,
           parameters TYPE tt_json_parameters,
         END OF ts_tool_invocation.

  TYPES: BEGIN OF ts_calculate_query_status_k,
           queryuuid TYPE sysuuid_x16,
         END OF ts_calculate_query_status_k.

  TYPES tt_calculate_query_status_k TYPE STANDARD TABLE OF ts_calculate_query_status_k WITH EMPTY KEY.

  TYPES: BEGIN OF ts_axc_head_k,
           runuuid TYPE sysuuid_x16,
         END OF ts_axc_head_k.

  TYPES: BEGIN OF ts_axc_query_k,
           queryuuid TYPE sysuuid_x16,
         END OF ts_axc_query_k.

  TYPES: BEGIN OF ts_axc_step_k,
           stepuuid TYPE sysuuid_x16,
         END OF ts_axc_step_k.

  TYPES: BEGIN OF ts_axc_head_query_link,
           runuuid   TYPE sysuuid_x16,
           queryuuid TYPE sysuuid_x16,
         END OF ts_axc_head_query_link.

  TYPES tt_axc_head_k          TYPE STANDARD TABLE OF ts_axc_head_k WITH EMPTY KEY.
  TYPES tt_axc_query_k         TYPE STANDARD TABLE OF ts_axc_query_k WITH EMPTY KEY.
  TYPES tt_axc_step_k          TYPE STANDARD TABLE OF ts_axc_step_k WITH EMPTY KEY.
  TYPES tt_axc_head_query_link TYPE STANDARD TABLE OF ts_axc_head_query_link WITH EMPTY KEY.

  TYPES ts_axc_head            TYPE zpru_s_axc_head.
  TYPES ts_axc_query           TYPE zpru_s_axc_query.
  TYPES ts_axc_step            TYPE zpru_s_axc_step.
  TYPES tt_axc_head            TYPE STANDARD TABLE OF ts_axc_head WITH EMPTY KEY.
  TYPES tt_axc_query           TYPE STANDARD TABLE OF ts_axc_query WITH EMPTY KEY.
  TYPES tt_axc_step            TYPE STANDARD TABLE OF ts_axc_step WITH EMPTY KEY.

  TYPES: BEGIN OF ts_head_control,
           runuuid          TYPE abap_boolean,
           runid            TYPE abap_boolean,
           agentuuid        TYPE abap_boolean,
           userid           TYPE abap_boolean,
           starttimestamp   TYPE abap_boolean,
           endtimestamp     TYPE abap_boolean,
           createdby        TYPE abap_boolean,
           createdat        TYPE abap_boolean,
           changedby        TYPE abap_boolean,
           lastchanged      TYPE abap_boolean,
           locallastchanged TYPE abap_boolean,
         END OF ts_head_control.

  TYPES: BEGIN OF ts_head_create_imp.
           INCLUDE TYPE zpru_s_axc_head.
  TYPES:   control TYPE ts_head_control.
  TYPES: END OF ts_head_create_imp.

  TYPES tt_head_create_imp TYPE STANDARD TABLE OF ts_head_create_imp WITH EMPTY KEY.

  TYPES: BEGIN OF ts_head_update_imp.
           INCLUDE TYPE zpru_s_axc_head.
  TYPES:   control TYPE ts_head_control.
  TYPES: END OF ts_head_update_imp.

  TYPES tt_head_update_imp TYPE STANDARD TABLE OF ts_head_update_imp WITH EMPTY KEY.

  TYPES: BEGIN OF ts_head_read_k,
           runuuid TYPE sysuuid_x16,
           control TYPE ts_head_control,
         END OF ts_head_read_k.

  TYPES tt_head_read_k TYPE STANDARD TABLE OF ts_head_read_k WITH EMPTY KEY.

  TYPES: BEGIN OF ts_query_control,
           runuuid         TYPE abap_boolean,
           querynumber     TYPE abap_boolean,
           queryuuid       TYPE abap_boolean,
           language        TYPE abap_boolean,
           executionstatus TYPE abap_boolean,
           starttimestamp  TYPE abap_boolean,
           endtimestamp    TYPE abap_boolean,
           inputprompt     TYPE abap_boolean,
           decisionlog     TYPE abap_boolean,
           outputresponse  TYPE abap_boolean,
         END OF ts_query_control.

  TYPES: BEGIN OF ts_rba_query_k,
           runuuid TYPE sysuuid_x16,
           control TYPE ts_query_control,
         END OF ts_rba_query_k.

  TYPES tt_rba_query_k TYPE STANDARD TABLE OF ts_rba_query_k WITH EMPTY KEY.

  TYPES: BEGIN OF ts_query_create_imp.
           INCLUDE TYPE zpru_s_axc_query.
  TYPES:   control TYPE ts_query_control.
  TYPES: END OF ts_query_create_imp.

  TYPES tt_query_create_imp TYPE STANDARD TABLE OF ts_query_create_imp WITH EMPTY KEY.

  TYPES: BEGIN OF ts_query_read_k,
           queryuuid TYPE sysuuid_x16,
           control   TYPE ts_query_control,
         END OF ts_query_read_k.

  TYPES tt_query_read_k TYPE STANDARD TABLE OF ts_query_read_k WITH EMPTY KEY.

  TYPES: BEGIN OF ts_query_update_imp.
           INCLUDE TYPE zpru_s_axc_query.
  TYPES:   control TYPE ts_query_control,
         END OF ts_query_update_imp.

  TYPES tt_query_update_imp TYPE STANDARD TABLE OF ts_query_update_imp WITH EMPTY KEY.

  TYPES: BEGIN OF ts_header_delete_imp,
           runuuid TYPE sysuuid_x16,
         END OF ts_header_delete_imp.

  TYPES tt_header_delete_imp TYPE STANDARD TABLE OF ts_header_delete_imp WITH EMPTY KEY.

  TYPES: BEGIN OF ts_query_delete_imp,
           queryuuid TYPE sysuuid_x16,
         END OF ts_query_delete_imp.

  TYPES tt_query_delete_imp TYPE STANDARD TABLE OF ts_query_delete_imp WITH EMPTY KEY.

  TYPES: BEGIN OF ts_step_control,
           stepuuid       TYPE abap_boolean,
           stepnumber     TYPE abap_boolean,
           queryuuid      TYPE abap_boolean,
           runuuid        TYPE abap_boolean,
           tooluuid       TYPE abap_boolean,
           executionseq   TYPE abap_boolean,
           stepstatus     TYPE abap_boolean,
           starttimestamp TYPE abap_boolean,
           endtimestamp   TYPE abap_boolean,
           inputprompt    TYPE abap_boolean,
           outputprompt   TYPE abap_boolean,
         END OF ts_step_control.

  TYPES: BEGIN OF ts_step_create_imp.
           INCLUDE TYPE zpru_s_axc_step.
  TYPES:   control TYPE ts_step_control.
  TYPES: END OF ts_step_create_imp.

  TYPES tt_step_create_imp TYPE STANDARD TABLE OF ts_step_create_imp WITH EMPTY KEY.

  TYPES: BEGIN OF ts_rba_step_k,
           queryuuid TYPE sysuuid_x16,
           control   TYPE ts_step_control,
         END OF ts_rba_step_k.

  TYPES tt_rba_step_k TYPE STANDARD TABLE OF ts_rba_step_k WITH EMPTY KEY.

  TYPES: BEGIN OF ts_step_read_k,
           stepuuid TYPE sysuuid_x16,
           control  TYPE ts_step_control,
         END OF ts_step_read_k.

  TYPES tt_step_read_k TYPE STANDARD TABLE OF ts_step_read_k WITH EMPTY KEY.

  TYPES: BEGIN OF ts_step_update_imp.
           INCLUDE TYPE zpru_s_axc_step.
  TYPES    control TYPE ts_step_control.
  TYPES  END OF ts_step_update_imp.

  TYPES tt_step_update_imp TYPE STANDARD TABLE OF ts_step_update_imp WITH EMPTY KEY.


  TYPES: BEGIN OF ts_step_delete_imp,
           stepuuid TYPE sysuuid_x16,
         END OF ts_step_delete_imp.

  TYPES tt_step_delete_imp TYPE STANDARD TABLE OF ts_step_delete_imp WITH EMPTY KEY.

ENDINTERFACE.
