INTERFACE zpru_if_adf_type_and_constant
  PUBLIC.

  CONSTANTS: BEGIN OF cs_agent_status,
               new      TYPE zpru_de_adf_agent_status VALUE 'N',
               active   TYPE zpru_de_adf_agent_status VALUE 'A',
               inactive TYPE zpru_de_adf_agent_status VALUE 'I',
             END OF cs_agent_status.

  CONSTANTS: BEGIN OF cs_step_type,
               nested_agent              TYPE zpru_de_adf_step_type VALUE 'A', " Nested Agent
               knowledge_source          TYPE zpru_de_adf_step_type VALUE 'K', " Knowledge Source
               abap_code                 TYPE zpru_de_adf_step_type VALUE 'B', " ABAP Code
               http_request              TYPE zpru_de_adf_step_type VALUE 'H', " HTTP Request
               service_consumption_model TYPE zpru_de_adf_step_type VALUE 'S', " Service Consumption Model
               call_llm                  TYPE zpru_de_adf_step_type VALUE 'L', " Call LLM
               dynamic_abap_code         TYPE zpru_de_adf_step_type VALUE 'D', " Dynamic ABAP Code
               infer_ml_model            TYPE zpru_de_adf_step_type VALUE 'M', " Infer ML model
               user_tool                 TYPE zpru_de_adf_step_type VALUE 'Z', " User Tool
             END OF cs_step_type.

  TYPES: BEGIN OF ts_agent_control,
           agentuuid             TYPE abap_boolean,
           agentname             TYPE abap_boolean,
           agenttype             TYPE abap_boolean,
           decisionprovider      TYPE abap_boolean,
           shortmemoryprovider   TYPE abap_boolean,
           longmemoryprovider    TYPE abap_boolean,
           agentinfoprovider     TYPE abap_boolean,
           systempromptprovider TYPE abap_boolean,
           agentstatus                TYPE abap_boolean,
           createdby             TYPE abap_boolean,
           createdat             TYPE abap_boolean,
           changedby             TYPE abap_boolean,
           lastchanged           TYPE abap_boolean,
           locallastchanged      TYPE abap_boolean,
         END OF ts_agent_control.

  TYPES: BEGIN OF ts_agent_k,
           agentuuid TYPE sysuuid_x16,
         END OF ts_agent_k.

  TYPES tt_agent_k TYPE STANDARD TABLE OF ts_agent_k WITH EMPTY KEY.

  TYPES: BEGIN OF ts_agent_tool_k,
           tooluuid TYPE sysuuid_x16,
         END OF ts_agent_tool_k.

  TYPES tt_agent_tool_k TYPE STANDARD TABLE OF ts_agent_tool_k WITH EMPTY KEY.

  TYPES ts_agent        TYPE zpru_s_agent.
  TYPES tt_agent        TYPE STANDARD TABLE OF ts_agent WITH EMPTY KEY.

  TYPES ts_agent_tool   TYPE zpru_s_agent_tool.
  TYPES tt_agent_tool   TYPE STANDARD TABLE OF ts_agent_tool WITH EMPTY KEY.

  TYPES: BEGIN OF ts_agent_create_imp.
           INCLUDE TYPE zpru_s_agent.
  TYPES:   control TYPE ts_agent_control.
  TYPES: END OF ts_agent_create_imp.

  TYPES tt_agent_create_imp TYPE STANDARD TABLE OF ts_agent_create_imp WITH EMPTY KEY.

  TYPES: BEGIN OF ts_agent_update_imp.
           INCLUDE TYPE zpru_s_agent.
  TYPES:   control TYPE ts_agent_control.
  TYPES: END OF ts_agent_update_imp.

  TYPES tt_agent_update_imp TYPE STANDARD TABLE OF ts_agent_update_imp WITH EMPTY KEY.

  TYPES: BEGIN OF ts_agent_read_k,
           agentuuid TYPE sysuuid_x16,
           control    TYPE ts_agent_control,
         END OF ts_agent_read_k.

  TYPES tt_agent_read_k TYPE STANDARD TABLE OF ts_agent_read_k WITH EMPTY KEY.

  TYPES: BEGIN OF ts_agent_delete_imp,
           agentuuid TYPE sysuuid_x16,
         END OF ts_agent_delete_imp.

  TYPES tt_agent_delete_imp TYPE STANDARD TABLE OF ts_agent_delete_imp WITH EMPTY KEY.

  TYPES: BEGIN OF ts_tool_control,
           tooluuid           TYPE abap_boolean,
           agentuuid          TYPE abap_boolean,
           toolname           TYPE abap_boolean,
           toolprovider       TYPE abap_boolean,
           steptype           TYPE abap_boolean,
           toolschemaprovider TYPE abap_boolean,
           toolinfoprovider   TYPE abap_boolean,
           toolisborrowed         TYPE abap_boolean,
           toolistransient        TYPE abap_boolean,
         END OF ts_tool_control.

  TYPES: BEGIN OF ts_rba_tool_k,
           agentuuid TYPE sysuuid_x16,
           control    TYPE ts_tool_control,
         END OF ts_rba_tool_k.

  TYPES tt_rba_tool_k TYPE STANDARD TABLE OF ts_rba_tool_k WITH EMPTY KEY.

  TYPES: BEGIN OF ts_tool_create_imp.
           INCLUDE TYPE zpru_s_agent_tool.
  TYPES:   control TYPE ts_tool_control.
  TYPES: END OF ts_tool_create_imp.

  TYPES tt_tool_create_imp TYPE STANDARD TABLE OF ts_tool_create_imp WITH EMPTY KEY.

  TYPES: BEGIN OF ts_tool_read_k,
           agentuuid TYPE sysuuid_x16,
           tooluuid  TYPE sysuuid_x16,
           control    TYPE ts_tool_control,
         END OF ts_tool_read_k.

  TYPES tt_tool_read_k TYPE STANDARD TABLE OF ts_tool_read_k WITH EMPTY KEY.

  TYPES: BEGIN OF ts_tool_update_imp.
           INCLUDE TYPE zpru_s_agent_tool.
  TYPES:   control TYPE ts_tool_control.
  TYPES: END OF ts_tool_update_imp.

  TYPES tt_tool_update_imp TYPE STANDARD TABLE OF ts_tool_update_imp WITH EMPTY KEY.

  TYPES: BEGIN OF ts_tool_delete_imp,
           agentuuid TYPE sysuuid_x16,
           tooluuid  TYPE sysuuid_x16,
         END OF ts_tool_delete_imp.

  TYPES tt_tool_delete_imp TYPE STANDARD TABLE OF ts_tool_delete_imp WITH EMPTY KEY.

ENDINTERFACE.
