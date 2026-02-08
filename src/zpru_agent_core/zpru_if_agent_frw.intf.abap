INTERFACE zpru_if_agent_frw
  PUBLIC.

  CONSTANTS: BEGIN OF cs_context,
               standard                      TYPE char100 VALUE 'STANDARD',
               st_persistence_message        TYPE char100 VALUE 'STANDARD_PERSISTENCE_MESSAGE',
               st_persistence_summarize      TYPE char100 VALUE 'STANDARD_PERSISTENCE_SUMMARIZE',
               st_summarize                  TYPE char100 VALUE 'STANDARD_SUMMARIZE',
               st_discard_strategy_delete    TYPE char100 VALUE 'STANDARD_DISCARD_STRATEGY_DELETE',
               st_discard_strategy_summarize TYPE char100 VALUE 'STANDARD_DISCARD_STRATEGY_SUMMARIZE',
               st_discard_strategy_save      TYPE char100 VALUE 'STANDARD_DISCARD_STRATEGY_SAVE',
               st_agent_execution            TYPE char100 VALUE 'STANDARD_AGENT_EXECUTION',
               st_agent_DEFINITION           TYPE char100 VALUE 'STANDARD_AGENT_DEFINITION',
             END OF cs_context.

  CONSTANTS:
    BEGIN OF  cs_fail_cause,
      unspecific   TYPE i VALUE 0,
      unauthorized TYPE i VALUE 401,
      not_found    TYPE i VALUE 404,
      conflict     TYPE i VALUE 409,
      locked       TYPE i VALUE 423,
      dependency   TYPE i VALUE 424,
      disabled     TYPE i VALUE 4221,
      readonly     TYPE i VALUE 4222,
    END OF  cs_fail_cause.

  CONSTANTS: BEGIN OF cs_message_class,
               zpru_msg_execution  TYPE symsgid VALUE `ZPRU_MSG_EXECUTION`,
               zpru_msg_definition TYPE symsgid VALUE `ZPRU_MSG_DEFINITION`,
             END OF cs_message_class.

  CONSTANTS: BEGIN OF cs_sign,
               include TYPE char1 VALUE 'I',
               exclude TYPE char1 VALUE 'E',
             END OF cs_sign.

  CONSTANTS: BEGIN OF cs_option,
               equal TYPE char2 VALUE 'EQ',
             END OF cs_option.

  " ADF (Agent Definition) types
  TYPES: BEGIN OF ts_agent_reported,
           agentuuid TYPE sysuuid_x16,
           msg       TYPE REF TO zpru_if_agent_message,
           create    TYPE abap_boolean,
           update    TYPE abap_boolean,
           delete    TYPE abap_boolean,
         END OF ts_agent_reported.

  TYPES tt_agent_reported TYPE STANDARD TABLE OF ts_agent_reported WITH EMPTY KEY.

  TYPES: BEGIN OF ts_tool_reported,
           agentuuid TYPE sysuuid_x16,
           tooluuid  TYPE sysuuid_x16,
           msg       TYPE REF TO zpru_if_agent_message,
           create    TYPE abap_boolean,
           update    TYPE abap_boolean,
           delete    TYPE abap_boolean,
         END OF ts_tool_reported.

  TYPES tt_tool_reported TYPE STANDARD TABLE OF ts_tool_reported WITH EMPTY KEY.

  TYPES: BEGIN OF ts_agent_failed,
           agentuuid TYPE sysuuid_x16,
           fail      TYPE i,
           create    TYPE abap_boolean,
           update    TYPE abap_boolean,
           delete    TYPE abap_boolean,
         END OF ts_agent_failed.

  TYPES tt_agent_failed TYPE STANDARD TABLE OF ts_agent_failed WITH EMPTY KEY.

  TYPES: BEGIN OF ts_tool_failed,
           agentuuid TYPE sysuuid_x16,
           tooluuid  TYPE sysuuid_x16,
           fail      TYPE i,
           create    TYPE abap_boolean,
           update    TYPE abap_boolean,
           delete    TYPE abap_boolean,
         END OF ts_tool_failed.

  TYPES tt_tool_failed TYPE STANDARD TABLE OF ts_tool_failed WITH EMPTY KEY.

  TYPES: BEGIN OF ts_agent_mapped,
           agentuuid TYPE sysuuid_x16,
         END OF ts_agent_mapped.

  TYPES tt_agent_mapped TYPE STANDARD TABLE OF ts_agent_mapped WITH EMPTY KEY.

  TYPES: BEGIN OF ts_tool_mapped,
           agentuuid TYPE sysuuid_x16,
           tooluuid  TYPE sysuuid_x16,
         END OF ts_tool_mapped.

  TYPES tt_tool_mapped TYPE STANDARD TABLE OF ts_tool_mapped WITH EMPTY KEY.

  TYPES: BEGIN OF ts_adf_reported,
           agent TYPE tt_agent_reported,
           tool  TYPE tt_tool_reported,
         END OF ts_adf_reported.

  TYPES: BEGIN OF ts_adf_failed,
           agent TYPE tt_agent_failed,
           tool  TYPE tt_tool_failed,
         END OF ts_adf_failed.

  TYPES: BEGIN OF ts_adf_mapped,
           agent TYPE tt_agent_mapped,
           tool  TYPE tt_tool_mapped,
         END OF ts_adf_mapped.

  " AGTY (Agent Type) types
  TYPES: BEGIN OF ts_agty_reported,
           agenttype TYPE zpru_de_agent_type,
           msg       TYPE REF TO zpru_if_agent_message,
           create    TYPE abap_boolean,
           update    TYPE abap_boolean,
           delete    TYPE abap_boolean,
         END OF ts_agty_reported.

  TYPES tt_agty_reported TYPE STANDARD TABLE OF ts_agty_reported WITH EMPTY KEY.

  TYPES: BEGIN OF ts_agty_failed,
           agenttype TYPE zpru_de_agent_type,
           fail      TYPE i,
           create    TYPE abap_boolean,
           update    TYPE abap_boolean,
           delete    TYPE abap_boolean,
         END OF ts_agty_failed.

  TYPES tt_agty_failed TYPE STANDARD TABLE OF ts_agty_failed WITH EMPTY KEY.

  TYPES: BEGIN OF ts_agty_mapped,
           agenttype TYPE zpru_de_agent_type,
         END OF ts_agty_mapped.

  TYPES tt_agty_mapped TYPE STANDARD TABLE OF ts_agty_mapped WITH EMPTY KEY.

  TYPES: BEGIN OF ts_agty_bndl_reported,
           agent_type TYPE tt_agty_reported,
         END OF ts_agty_bndl_reported.

  TYPES: BEGIN OF ts_agty_bndl_failed,
           agent_type TYPE tt_agty_failed,
         END OF ts_agty_bndl_failed.

  TYPES: BEGIN OF ts_agty_bndl_mapped,
           agent_type TYPE tt_agty_mapped,
         END OF ts_agty_bndl_mapped.

  " AGSRV (Agent Service) types
  TYPES: BEGIN OF ts_agsrv_reported,
           service TYPE zpru_de_seoclname,
           context TYPE char100,
           msg     TYPE REF TO zpru_if_agent_message,
           create  TYPE abap_boolean,
           update  TYPE abap_boolean,
           delete  TYPE abap_boolean,
         END OF ts_agsrv_reported.

  TYPES tt_agsrv_reported TYPE STANDARD TABLE OF ts_agsrv_reported WITH EMPTY KEY.

  TYPES: BEGIN OF ts_agsrv_failed,
           service TYPE zpru_de_seoclname,
           context TYPE char100,
           fail    TYPE i,
           create  TYPE abap_boolean,
           update  TYPE abap_boolean,
           delete  TYPE abap_boolean,
         END OF ts_agsrv_failed.

  TYPES tt_agsrv_failed TYPE STANDARD TABLE OF ts_agsrv_failed WITH EMPTY KEY.

  TYPES: BEGIN OF ts_agsrv_mapped,
           service TYPE zpru_de_seoclname,
           context TYPE char100,
         END OF ts_agsrv_mapped.

  TYPES tt_agsrv_mapped TYPE STANDARD TABLE OF ts_agsrv_mapped WITH EMPTY KEY.

  TYPES: BEGIN OF ts_agsrv_bndl_reported,
           agsrv TYPE tt_agsrv_reported,
         END OF ts_agsrv_bndl_reported.

  TYPES: BEGIN OF ts_agsrv_bndl_failed,
           agsrv TYPE tt_agsrv_failed,
         END OF ts_agsrv_bndl_failed.

  TYPES: BEGIN OF ts_agsrv_bndl_mapped,
           agsrv TYPE tt_agsrv_mapped,
         END OF ts_agsrv_bndl_mapped.

  " AXC (Agent eXecution Context) types
  TYPES: BEGIN OF ts_header_reported,
           runuuid TYPE sysuuid_x16,
           msg     TYPE REF TO zpru_if_agent_message,
           create  TYPE abap_boolean,
           update  TYPE abap_boolean,
           delete  TYPE abap_boolean,
         END OF ts_header_reported.

  TYPES tt_header_reported TYPE STANDARD TABLE OF ts_header_reported WITH EMPTY KEY.

  TYPES: BEGIN OF ts_query_reported,
           queryuuid TYPE sysuuid_x16,
           msg       TYPE REF TO zpru_if_agent_message,
           create    TYPE abap_boolean,
           update    TYPE abap_boolean,
           delete    TYPE abap_boolean,
         END OF ts_query_reported.

  TYPES tt_query_reported TYPE STANDARD TABLE OF ts_query_reported WITH EMPTY KEY.

  TYPES: BEGIN OF ts_step_reported,
           stepuuid TYPE sysuuid_x16,
           msg      TYPE REF TO zpru_if_agent_message,
           create   TYPE abap_boolean,
           update   TYPE abap_boolean,
           delete   TYPE abap_boolean,
         END OF ts_step_reported.

  TYPES tt_step_reported TYPE STANDARD TABLE OF ts_step_reported WITH EMPTY KEY.

  TYPES: BEGIN OF ts_header_failed,
           runuuid TYPE sysuuid_x16,
           fail    TYPE i,
           create  TYPE abap_boolean,
           update  TYPE abap_boolean,
           delete  TYPE abap_boolean,
         END OF ts_header_failed.

  TYPES tt_header_failed TYPE STANDARD TABLE OF ts_header_failed WITH EMPTY KEY.

  TYPES: BEGIN OF ts_query_failed,
           queryuuid TYPE sysuuid_x16,
           fail      TYPE i,
           create    TYPE abap_boolean,
           update    TYPE abap_boolean,
           delete    TYPE abap_boolean,
         END OF ts_query_failed.

  TYPES tt_query_failed TYPE STANDARD TABLE OF ts_query_failed WITH EMPTY KEY.

  TYPES: BEGIN OF ts_step_failed,
           stepuuid TYPE sysuuid_x16,
           fail     TYPE i,
           create   TYPE abap_boolean,
           update   TYPE abap_boolean,
           delete   TYPE abap_boolean,
         END OF ts_step_failed.

  TYPES tt_step_failed TYPE STANDARD TABLE OF ts_step_failed WITH EMPTY KEY.

  TYPES: BEGIN OF ts_header_mapped,
           runuuid TYPE sysuuid_x16,
         END OF ts_header_mapped.

  TYPES tt_header_mapped TYPE STANDARD TABLE OF ts_header_mapped WITH EMPTY KEY.

  TYPES: BEGIN OF ts_query_mapped,
           queryuuid TYPE sysuuid_x16,
         END OF ts_query_mapped.

  TYPES tt_query_mapped TYPE STANDARD TABLE OF ts_query_mapped WITH EMPTY KEY.

  TYPES: BEGIN OF ts_step_mapped,
           stepuuid TYPE sysuuid_x16,
         END OF ts_step_mapped.

  TYPES tt_step_mapped TYPE STANDARD TABLE OF ts_step_mapped WITH EMPTY KEY.

  TYPES: BEGIN OF ts_axc_reported,
           header TYPE tt_header_reported,
           query  TYPE tt_query_reported,
           step   TYPE tt_step_reported,
         END OF ts_axc_reported.

  TYPES: BEGIN OF ts_axc_failed,
           header TYPE tt_header_failed,
           query  TYPE tt_query_failed,
           step   TYPE tt_step_failed,
         END OF ts_axc_failed.

  TYPES: BEGIN OF ts_axc_mapped,
           header TYPE tt_header_mapped,
           query  TYPE tt_query_mapped,
           step   TYPE tt_step_mapped,
         END OF ts_axc_mapped.

  " MMSG (Memory Message) types
  TYPES: BEGIN OF ts_mmsg_reported,
           messageuuid TYPE sysuuid_x16,
           msg          TYPE REF TO zpru_if_agent_message,
           create       TYPE abap_boolean,
           update       TYPE abap_boolean,
           delete       TYPE abap_boolean,
         END OF ts_mmsg_reported.

  TYPES tt_mmsg_reported TYPE STANDARD TABLE OF ts_mmsg_reported WITH EMPTY KEY.

  TYPES: BEGIN OF ts_mmsg_failed,
           messageuuid TYPE sysuuid_x16,
           fail         TYPE i,
           create       TYPE abap_boolean,
           update       TYPE abap_boolean,
           delete       TYPE abap_boolean,
         END OF ts_mmsg_failed.

  TYPES tt_mmsg_failed TYPE STANDARD TABLE OF ts_mmsg_failed WITH EMPTY KEY.

  TYPES: BEGIN OF ts_mmsg_mapped,
           messageuuid TYPE sysuuid_x16,
         END OF ts_mmsg_mapped.

  TYPES tt_mmsg_mapped TYPE STANDARD TABLE OF ts_mmsg_mapped WITH EMPTY KEY.

  TYPES: BEGIN OF ts_mmsg_bndl_reported,
           mmsg TYPE tt_mmsg_reported,
         END OF ts_mmsg_bndl_reported.

  TYPES: BEGIN OF ts_mmsg_bndl_failed,
           mmsg TYPE tt_mmsg_failed,
         END OF ts_mmsg_bndl_failed.

  TYPES: BEGIN OF ts_mmsg_bndl_mapped,
           mmsg TYPE tt_mmsg_mapped,
         END OF ts_mmsg_bndl_mapped.

  " MSUM (Memory Summary) types
  TYPES: BEGIN OF ts_msum_reported,
           summaryuuid TYPE sysuuid_x16,
           msg          TYPE REF TO zpru_if_agent_message,
           create       TYPE abap_boolean,
           update       TYPE abap_boolean,
           delete       TYPE abap_boolean,
         END OF ts_msum_reported.

  TYPES tt_msum_reported TYPE STANDARD TABLE OF ts_msum_reported WITH EMPTY KEY.

  TYPES: BEGIN OF ts_msum_failed,
           summaryuuid TYPE sysuuid_x16,
           fail         TYPE i,
           create       TYPE abap_boolean,
           update       TYPE abap_boolean,
           delete       TYPE abap_boolean,
         END OF ts_msum_failed.

  TYPES tt_msum_failed TYPE STANDARD TABLE OF ts_msum_failed WITH EMPTY KEY.

  TYPES: BEGIN OF ts_msum_mapped,
           summaryuuid TYPE sysuuid_x16,
         END OF ts_msum_mapped.

  TYPES tt_msum_mapped TYPE STANDARD TABLE OF ts_msum_mapped WITH EMPTY KEY.

  TYPES: BEGIN OF ts_msum_bndl_reported,
           msum TYPE tt_msum_reported,
         END OF ts_msum_bndl_reported.

  TYPES: BEGIN OF ts_msum_bndl_failed,
           msum TYPE tt_msum_failed,
         END OF ts_msum_bndl_failed.

  TYPES: BEGIN OF ts_msum_bndl_mapped,
           msum TYPE tt_msum_mapped,
         END OF ts_msum_bndl_mapped.

  TYPES ts_json TYPE string.

ENDINTERFACE.
