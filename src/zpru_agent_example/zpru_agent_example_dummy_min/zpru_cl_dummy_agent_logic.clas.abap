CLASS zpru_cl_dummy_agent_logic DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zpru_if_agent_frw.
    INTERFACES zpru_if_dummy_agent_logic.
    INTERFACES zpru_if_decision_provider.
    INTERFACES zpru_if_short_memory_provider.
    INTERFACES zpru_if_long_memory_provider.
    INTERFACES zpru_if_agent_info_provider.
    INTERFACES zpru_if_prompt_provider.
    INTERFACES zpru_if_tool_provider.
    INTERFACES zpru_if_input_schema_provider.
    INTERFACES zpru_if_tool_info_provider.

    TYPES: BEGIN OF ts_gate_pass_assessment,
             " --- Critical Question Answers ---
             is_expected        TYPE abap_bool, " Question: Is the vehicle in the schedule?
             is_on_time         TYPE abap_bool, " Question: Is it within the time slot?
             is_carrier_allowed TYPE abap_bool, " Question: Is the vendor blocked/blacklisted?
             is_driver_verified TYPE abap_bool, " Question: Does Driver ID match the record?
             " --- Routing Data ---
             assigned_gate      TYPE string,    " Answer: Which gate should they go to? (e.g., 'GATE_04')
             " --- The Explanation ---
             explanation        TYPE string,    " Human-readable summary of the tool's result
             risk_score         TYPE string,         " 0 = Green, 1 = Yellow, 2 = Red
           END OF ts_gate_pass_assessment.

    TYPES: BEGIN OF ts_context,
             query                     TYPE string,
             system_prompt             TYPE string,
             gate_pass_assessment      TYPE ts_gate_pass_assessment,
             gate_pass_assessment_json TYPE string,
           END OF ts_context.

    TYPES: BEGIN OF ts_rule,
             rule_number      TYPE i,
             rule_name        TYPE string,
             rule_explanation TYPE string,
           END OF ts_rule.

    TYPES: tt_rules TYPE STANDARD TABLE OF ts_rule WITH EMPTY KEY.

    TYPES: BEGIN OF ts_safety_protocol,
             name               TYPE string,
             description        TYPE string,   " Main purpose of the protocol
             date_updated       TYPE d,
             responsible_person TYPE string,
             rules              TYPE tt_rules, " Safety rules
           END OF ts_safety_protocol.

    TYPES: tt_safety_knowledge TYPE STANDARD TABLE OF ts_safety_protocol WITH EMPTY KEY.

    " technical methods and attributes made for the sake of example
    TYPES: BEGIN OF ts_method_registr,
             call_decision_engine   TYPE abap_boolean,
             prepare_final_response TYPE abap_boolean,
             get_agent_info         TYPE abap_boolean,
             get_prompt_language    TYPE abap_boolean,
             get_system_prompt      TYPE abap_boolean,
             get_tool               TYPE abap_boolean,
             get_input_schema       TYPE abap_boolean,
             simple_tool            TYPE abap_boolean,
             knowledge              TYPE abap_boolean,
             nested_agent           TYPE abap_boolean,
           END OF ts_method_registr.

    CLASS-DATA ms_method_registr TYPE ts_method_registr.

    CLASS-METHODS get_input_prompt
      RETURNING VALUE(rv_prompt) TYPE string.

    " I recommend to read body of this method just for your information
    CLASS-METHODS read_me.

  PROTECTED SECTION.

  PRIVATE SECTION.
    CLASS-DATA so_short_memory TYPE REF TO lcl_short_memory_provider.
    CLASS-DATA so_long_memory TYPE REF TO lcl_long_memory_provider.

    CLASS-METHODS get_short_memory
      RETURNING VALUE(ro_instance) TYPE REF TO lcl_short_memory_provider.

    CLASS-METHODS get_long_memory
      RETURNING VALUE(ro_instance) TYPE REF TO lcl_long_memory_provider.

ENDCLASS.


CLASS zpru_cl_dummy_agent_logic IMPLEMENTATION.

  METHOD read_me.
    " 1 INTRODUCTION

*    Start there.
*    The class implements all interfaces from agent framework.
*    Specifically, these are zpru_if_decision_provider,zpru_if_short_memory_provider, zpru_if_long_memory_provider,
*    zpru_if_agent_info_provider, zpru_if_prompt_provider, zpru_if_tool_provider, zpru_if_input_schema_provider.

    " Static methods like get_input_prompt, read_me and attribute ms_method_registr are technical methods to support
    " example, debugging or make example clear to users

    " The class is minified implementation of dummy agent. Minified means that instead of creation separate global class for
    " each agent framework roles (decision engine, short/long memory providers, tools' providers etc.) I have created separate local
    " classes in zpru_cl_dummy_agent_logic. These are lcl_decision_provider, lcl_short_memory_provider, lcl_tool_provider, lcl_simple_too etc.
    " Having many local classes and the only one global class is just my preferable flavor of implementation of agent. If user wants, he can
    " implement all stuff in global class.

    " As the name 'DUMMY' supposes the agent don't use any LLM to make decisions, the logic is hardcoded via if - else statement. I made
    " this dummy agent for testing purposes and for simple demo activities.

    " 2 AGENT DEFINITION AND STARTUP

    " Agent definition is in table zpru_agent. Tool definition is in table zpru_agent_tool.
    " To start agent you can refer to class zpru_run_agent.

*    METHOD if_oo_adt_classrun~main.
*      DATA lo_cl_unit_agent TYPE REF TO zpru_if_unit_agent.
*
*      lo_cl_unit_agent = NEW zpru_cl_unit_agent( ).
*      lo_cl_unit_agent->execute_agent( iv_agent_name  = 'DUMMY_AGENT'
*                                       iv_input_query = zpru_cl_dummy_agent_logic=>get_input_prompt( ) ).
*    ENDMETHOD.

    " 3 TECHNICAL LOGIC

    "  +------------------------------------------------------------+
    "  |        User Input: Prompt String + Agent Name              |
    "  +------------------------------------------------------------+
    "                             |
    "                             v
    "  +------------------------------------------------------------+
    "  |           Initialize Agent and Query Object                |
    "  +------------------------------------------------------------+
    "                             |
    "                             v
    "  +------------------------------------------------------------+
    "  |   Invoke Decision Engine (LLM, ML, IF-ELSE) -> Step Plan   |
    "  +------------------------------------------------------------+
    "                             |
    "                             v
    "  +------------------------------------------------------------+
    "  |      Build Execution Run + Query + Steps (per Plan)        |
    "  +------------------------------------------------------------+
    "                             |
    "                             v
    "  +------------------------------------------------------------+
    "  |       Execute Steps in Loop According to Step Plan         |
    "  +------------------------------------------------------------+
    "                             |
    "                             v
    "  +------------------------------------------------------------+
    "  |   Invoke Decision Engine -> Generate Final Response        |
    "  +------------------------------------------------------------+
    "                             |
    "                             v
    "  +------------------------------------------------------------+
    "  |          Return Final Response String to User              |
    "  +------------------------------------------------------------+

    " 4 BUSINESS CASE Inbound Logistics Agent

    " We will simulate an Inbound Logistics Agent. Its goal is to process a CMR document and
    " ensure the warehouse is ready to receive the goods.

    " The Scenario Workflow:
    " Validate Vehicle: Is the truck authorized to enter the plant?
    " Verify Inventory: Do we have space for the goods listed on the CMR?
    " Exception Handling: If there is a discrepancy (e.g., hazardous materials), call a specialist.

    " The Toolset
    " Tool A: VERIFY_GATE_PASS (Simple Tool)
    " Purpose: Integration with the Yard Management system.
    " Input: plate_number, driver_id.
    "
*Tool B: HAZMAT_KNOWLEDGE_BASE (The "Dummy RAG")
*This tool acts as your Knowledge Source. Instead of checking capacity, it provides the Standard Operating Procedure (SOP) based on the CMR data.
*The Procedure
*Search Trigger: The tool receives the UN_Number or Hazard_Class from the Agent.
*Lookup: It scans a "Knowledge Table" (internal dummy list) for matching safety protocols.
*Instruction Retrieval: It fetches the specific handling requirements (e.g., fire safety, PPE).
*Response: Returns a plain text string containing the legal and safety instructions.

*Tool C: SUCCESSOR_DOC_GENERATOR (The "Action" Tool)
*This tool is responsible for the transition from the CMR (Inbound) to the internal Warehouse process.
*1. The Procedure
*Preliminary Validation: * Does the CMR have a valid signature/status?
*Is the Gross_Weight_KG within acceptable tolerances of the PO?
*If validation fails, return an error (preventing document creation).
*Document Mapping: Map CMR fields to the Successor Document (e.g., Plate_Number -> Vehicle ID in the Warehouse Task).
*Creation: Simulate the call to an SAP BAPI (like BAPI_WHSE_TO_CREATE).
*Confirmation: Return the new Document Number (e.g., Inbound Delivery or Warehouse Task ID).

    " Tool D: HAZMAT_ASSESSOR (Nested Agent)
    " Purpose: This is a sub-agent specialized in Dangerous Goods. The Main Agent only calls this if the CMR hazard_class field is NOT empty.
    " Internal Tools of the Nested Agent:
    "     GET_SAFETY_PROTOCOL: Fetches PDF instructions for a specific UN Number.
    "     ALERT_SAFETY_OFFICER: Sends an SAP Office Express message or triggers a workflow.

*Tool F: WEATHER_TRAFFIC_AUDIT (HTTP Request - 'H')
*Purpose: External validation. If the driver claims the goods were damaged due to extreme heat/moisture during transit,
*the agent verifies the route conditions.
*
*The Procedure: Calls an external REST API (e.g., OpenWeather or a Logistics Map API).
*
*Input: departure_city, arrival_city, transit_timestamp.
*
*Response: Returns a JSON with the weather history for that route to validate the driver's claim on the CMR.

*Tool G: STORAGE_BIN_LOCATOR (Service Consumption Model - 'S')
*Purpose: Finding a physical spot in the warehouse using modern S/4HANA OData services.
*
*The Procedure: Uses an OData Client Proxy (generated via a Service Consumption Model) to call the Warehouse Management API.
*
*Input: material_id, quantity, storage_type.
*
*Response: Returns the exact coordinates of an empty storage bin (e.g., BIN-04-A-12).

*Tool H: CMR_REMARK_INTERPRETER (Call LLM - 'L')
*Purpose: The CMR often has handwritten-style digital notes like "Pallet 4 damaged but seal intact" or "Driver waited 2 hours at border".
*
*The Procedure: Sends the "Remarks" string to the LLM with a system prompt.
*
*Input: cmr_notes_string.
*
*Action: Prompt: "Identify if there is a claim of damage or a delay reason in this text."
*
*Response: Returns structured JSON: { "damage_reported": true, "delay_reason": "border_customs" }.

*Tool J: RISK_SCORE_PREDICTOR (Infer ML Model - 'M')
*Purpose: Assessing the probability that this specific delivery will result in a "Shortage Claim" based on the CMR data and historical vendor reliability.
*
*The Procedure: Calls an SAP AI Core model or a local ML scenario.
*
*Input: vendor_id, material_type, total_pallets.
*
*Response: Returns a risk percentage (e.g., "High Risk: 82% probability of quantity discrepancy").


*Tool K: PHYSICAL_SEAL_VERIFICATION (User Tool - 'Z')
*Purpose: The Agent stops and waits for a "Human-in-the-loop" to confirm the security seal on the truck matches the CMR number.
*
*The Procedure: The agent pauses execution and sends a "Work Item" to a Fiori App.
*
*Input: cmr_seal_number.
*
*Action: A human worker looks at the truck, compares the seal, and clicks "Match" or "Mismatch" in the UI.
*
*Response: The agent resumes only after the user provides the boolean input.


    " 5 STOCHASTIC BEHAVIOR

    " to imitate stochastic I made technical local class lcl_stochastic_producer. This is technical class for current agent implementation.
    " It use random number if it even - one branch if odd - another branch


*VALUE #(
*( CLIENT = '100' SERVICE = 'ZPRU_IF_UNIT_AGENT' CONTEXT = 'STANDARD' CLASS = 'ZPRU_CL_UNIT_AGENT' CREATED_BY = '' CREATED_AT = '0.0000000' CHANGED_BY = '' LAST_CHANGED = '20260117150518.3887310' LOCAL_LAST_CHANGED = '20260117150518.3887310'  )
*( CLIENT = '100' SERVICE = 'ZPRU_IF_API_AGENT' CONTEXT = 'STANDARD' CLASS = 'ZPRU_CL_API_AGENT' CREATED_BY = '' CREATED_AT = '0.0000000' CHANGED_BY = '' LAST_CHANGED = '20260117150619.0368210' LOCAL_LAST_CHANGED = '20260117150619.0368210'  )
*( CLIENT = '100' SERVICE = 'ZPRU_IF_AGENT_CONTROLLER' CONTEXT = 'STANDARD' CLASS = 'ZPRU_CL_AGENT_CONTROLLER' CREATED_BY = '' CREATED_AT = '0.0000000' CHANGED_BY = '' LAST_CHANGED = '20260117150738.0236420' LOCAL_LAST_CHANGED = '20260117150738.0236420'
    " )
*( CLIENT = '100' SERVICE = 'ZPRU_IF_AGENT_UTIL' CONTEXT = 'STANDARD' CLASS = 'ZPRU_CL_AGENT_UTIL' CREATED_BY = '' CREATED_AT = '0.0000000' CHANGED_BY = '' LAST_CHANGED = '20260117150935.4567820' LOCAL_LAST_CHANGED = '20260117150935.4567820'  )
*( CLIENT = '100' SERVICE = 'ZPRU_IF_PAYLOAD' CONTEXT = 'STANDARD' CLASS = 'ZPRU_CL_PAYLOAD' CREATED_BY = '' CREATED_AT = '0.0000000' CHANGED_BY = '' LAST_CHANGED = '20260117151018.7600370' LOCAL_LAST_CHANGED = '20260117151018.7600370'  )
*( CLIENT = '100' SERVICE = 'ZPRU_IF_ADF_SERVICE' CONTEXT = 'STANDARD' CLASS = 'ZPRU_CL_ADF_SERVICE' CREATED_BY = '' CREATED_AT = '0.0000000' CHANGED_BY = '' LAST_CHANGED = '20260117151237.1192170' LOCAL_LAST_CHANGED = '20260117151237.1192170'  )
*( CLIENT = '100' SERVICE = 'ZPRU_IF_DUMMY_AGENT_LOGIC' CONTEXT = 'STANDARD' CLASS = 'ZPRU_CL_DUMMY_AGENT_LOGIC' CREATED_BY = '' CREATED_AT = '0.0000000' CHANGED_BY = '' LAST_CHANGED = '20260117152144.6419870' LOCAL_LAST_CHANGED = '20260117152144.6419870
    "'  )
*( CLIENT = '100' SERVICE = 'IF_AIC_COMPLETION_API' CONTEXT = 'STANDARD' CLASS = 'ZPRU_CL_AIC_COMPLETION_API' CREATED_BY = '' CREATED_AT = '0.0000000' CHANGED_BY = '' LAST_CHANGED = '20260117152251.0551680' LOCAL_LAST_CHANGED = '20260117152251.0551680'
    ")
*( CLIENT = '100' SERVICE = 'IF_AIC_COMPLETION_API_RESULT' CONTEXT = 'STANDARD' CLASS = 'ZPRU_CL_AIC_COMPLETION_API_RES' CREATED_BY = '' CREATED_AT = '0.0000000' CHANGED_BY = '' LAST_CHANGED = '20260117152343.0309600'
*LOCAL_LAST_CHANGED = '20260117152343.0309600'  )
*( CLIENT = '100' SERVICE = 'IF_AIC_COMPLETION_PARAMETERS' CONTEXT = 'STANDARD' CLASS = 'ZPRU_CL_AIC_COMPLETION_PARAM' CREATED_BY = '' CREATED_AT = '0.0000000' CHANGED_BY = '' LAST_CHANGED = '20260117152423.9709400'
*LOCAL_LAST_CHANGED = '20260117152423.9709400'  )
*( CLIENT = '100' SERVICE = 'IF_AIC_MESSAGE_CONTAINER' CONTEXT = 'STANDARD' CLASS = 'ZPRU_CL_AIC_MESSAGE_CONTAINER' CREATED_BY = '' CREATED_AT = '0.0000000' CHANGED_BY = '' LAST_CHANGED = '20260117152547.0539920'
*LOCAL_LAST_CHANGED = '20260117152547.0539920'  )
*( CLIENT = '100' SERVICE = 'IF_AIC_ISLM_COMPL_API_FACTORY' CONTEXT = 'STANDARD' CLASS = 'ZPRU_CL_ISLM_COMPL_API_FACTORY' CREATED_BY = '' CREATED_AT = '0.0000000' CHANGED_BY = '' LAST_CHANGED = '20260117152659.4183900'
*LOCAL_LAST_CHANGED = '20260117152659.4183900'  )
*( CLIENT = '100' SERVICE = 'IF_WEB_HTTP_CLIENT' CONTEXT = 'STANDARD' CLASS = 'ZPRU_CL_WEB_HTTP_CLIENT' CREATED_BY = '' CREATED_AT = '0.0000000' CHANGED_BY = '' LAST_CHANGED = '20260117152753.5715620' LOCAL_LAST_CHANGED = '20260117152753.5715620'  )
*( CLIENT = '100' SERVICE = 'IF_WEB_HTTP_REQUEST' CONTEXT = 'STANDARD' CLASS = 'ZPRU_CL_WEB_HTTP_REQUEST' CREATED_BY = '' CREATED_AT = '0.0000000' CHANGED_BY = '' LAST_CHANGED = '20260117152903.9660210' LOCAL_LAST_CHANGED = '20260117152903.9660210'  )
*( CLIENT = '100' SERVICE = 'IF_WEB_HTTP_RESPONSE' CONTEXT = 'STANDARD' CLASS = 'ZPRU_CL_WEB_HTTP_RESPONSE' CREATED_BY = '' CREATED_AT = '0.0000000' CHANGED_BY = '' LAST_CHANGED = '20260117153008.0378680' LOCAL_LAST_CHANGED = '20260117153008.0378680'  )
*( CLIENT = '100' SERVICE = 'ZPRU_IF_AXC_SERVICE' CONTEXT = 'STANDARD' CLASS = 'ZPRU_CL_AXC_SERVICE' CREATED_BY = '' CREATED_AT = '0.0000000' CHANGED_BY = '' LAST_CHANGED = '20260117153230.0427800' LOCAL_LAST_CHANGED = '20260117153230.0427800'  )
*( CLIENT = '100' SERVICE = 'ZPRU_IF_LONG_MEMORY_PROVIDER' CONTEXT = 'STANDARD' CLASS = 'ZPRU_CL_LONG_MEMORY_BASE' CREATED_BY = '' CREATED_AT = '0.0000000' CHANGED_BY = '' LAST_CHANGED = '20260117153709.0139100'
*LOCAL_LAST_CHANGED = '20260117153709.0139100'  )
*( CLIENT = '100' SERVICE = 'ZPRU_IF_LONG_MEM_PERSISTENCE' CONTEXT = 'STANDARD_PERSISTENCE_MESSAGE' CLASS = 'ZPRU_CL_PERSISTENCE_MSG' CREATED_BY = '' CREATED_AT = '0.0000000' CHANGED_BY = '' LAST_CHANGED = '20260117154416.9645010'
*LOCAL_LAST_CHANGED = '20260117154416.9645010'  )
*( CLIENT = '100' SERVICE = 'ZPRU_IF_LONG_MEM_PERSISTENCE' CONTEXT = 'STANDARD_PERSISTENCE_SUMMARIZE' CLASS = 'ZPRU_CL_PERSISTENCE_SUM' CREATED_BY = '' CREATED_AT = '0.0000000' CHANGED_BY = '' LAST_CHANGED = '20260117154604.0443640'
*LOCAL_LAST_CHANGED = '20260117154604.0443640'  )
*( CLIENT = '100' SERVICE = 'ZPRU_IF_SUMMARIZATION' CONTEXT = 'STANDARD_SUMMARIZE' CLASS = 'ZPRU_CL_PERSISTENCE_SUM' CREATED_BY = '' CREATED_AT = '0.0000000' CHANGED_BY = '' LAST_CHANGED = '20260117154729.8684840'
*LOCAL_LAST_CHANGED = '20260117154729.8684840'  )
*( CLIENT = '100' SERVICE = 'ZPRU_IF_SHORT_MEMORY_PROVIDER' CONTEXT = 'STANDARD' CLASS = 'ZPRU_CL_SHORT_MEMORY_BASE' CREATED_BY = '' CREATED_AT = '0.0000000' CHANGED_BY = '' LAST_CHANGED = '20260117154917.7603470'
*LOCAL_LAST_CHANGED = '20260117154917.7603470'  )
*( CLIENT = '100' SERVICE = 'ZPRU_IF_DISCARD_STRATEGY' CONTEXT = 'STANDARD_DISCARD_STRATEGY_DELETE' CLASS = 'ZPRU_CL_DISCARD_DELETE' CREATED_BY = '' CREATED_AT = '0.0000000' CHANGED_BY = '' LAST_CHANGED = '20260117155051.9175870'
*LOCAL_LAST_CHANGED = '20260117155051.9175870'  )
*( CLIENT = '100' SERVICE = 'ZPRU_IF_DISCARD_STRATEGY' CONTEXT = 'STANDARD_DISCARD_STRATEGY_SAVE' CLASS = 'ZPRU_CL_DISCARD_SAVE' CREATED_BY = '' CREATED_AT = '0.0000000' CHANGED_BY = '' LAST_CHANGED = '20260117155223.7925380'
*LOCAL_LAST_CHANGED = '20260117155223.7925380'  )
*( CLIENT = '100' SERVICE = 'ZPRU_IF_DISCARD_STRATEGY' CONTEXT = 'STANDARD_DISCARD_STRATEGY_SUMMARIZE' CLASS = 'ZPRU_CL_DISCARD_SUMMARIZE' CREATED_BY = '' CREATED_AT = '0.0000000' CHANGED_BY = '' LAST_CHANGED = '20260117155325.9377550'
*LOCAL_LAST_CHANGED = '20260117155325.9377550'  )
*( CLIENT = '100' SERVICE = 'ZPRU_IF_AXC_PRECHECK' CONTEXT = 'STANDARD_AGENT_EXECUTION' CLASS = 'ZPRU_CL_AXC_PRECHECK' CREATED_BY = '' CREATED_AT = '0.0000000' CHANGED_BY = '' LAST_CHANGED = '20260118160211.0649630'
*LOCAL_LAST_CHANGED = '20260118160211.0649630'  )
*( CLIENT = '100' SERVICE = 'ZPRU_IF_ADF_PRECHECK' CONTEXT = 'STANDARD_AGENT_DEFINITION' CLASS = 'ZPRU_CL_ADF_PRECHECK' CREATED_BY = '' CREATED_AT = '0.0000000' CHANGED_BY = '' LAST_CHANGED = '20260118160851.3804580'
*LOCAL_LAST_CHANGED = '20260118160851.3804580'  )
* )




  ENDMETHOD.

  METHOD zpru_if_decision_provider~call_decision_engine.
    DATA lo_decision_provider TYPE REF TO zpru_if_decision_provider.

    lo_decision_provider = NEW lcl_decision_provider( ).

    lo_decision_provider->call_decision_engine( EXPORTING is_agent               = is_agent
                                                          it_tool                = it_tool
                                                          io_controller          = io_controller
                                                          io_input               = io_input
                                                          io_system_prompt       = io_system_prompt
                                                          io_short_memory        = io_short_memory
                                                          io_long_memory         = io_long_memory
                                                          io_agent_info_provider = io_agent_info_provider
                                                IMPORTING eo_execution_plan      = eo_execution_plan
                                                          eo_first_tool_input    = eo_first_tool_input
                                                          eo_langu               = eo_langu
                                                          eo_decision_log        = eo_decision_log ).
  ENDMETHOD.

  METHOD zpru_if_short_memory_provider~clear_history.
    DATA lo_short_memory TYPE REF TO zpru_if_short_memory_provider.

    lo_short_memory = zpru_cl_dummy_agent_logic=>get_short_memory( ).
    lo_short_memory->clear_history( ).
  ENDMETHOD.

  METHOD zpru_if_agent_info_provider~get_agent_info.
    DATA lo_agent_info_provider TYPE REF TO zpru_if_agent_info_provider.

    lo_agent_info_provider = NEW lcl_agent_info_provider( ).
    rv_agent_info = lo_agent_info_provider->get_agent_info( ).
  ENDMETHOD.

  METHOD zpru_if_short_memory_provider~get_discard_strategy.
    DATA lo_short_memory TYPE REF TO zpru_if_short_memory_provider.

    lo_short_memory = zpru_cl_dummy_agent_logic=>get_short_memory( ).
    ro_discard_strategy = lo_short_memory->get_discard_strategy( ).
  ENDMETHOD.

  METHOD zpru_if_short_memory_provider~get_history.
    DATA lo_short_memory TYPE REF TO zpru_if_short_memory_provider.

    lo_short_memory = zpru_cl_dummy_agent_logic=>get_short_memory( ).
    rt_history = lo_short_memory->get_history( ).
  ENDMETHOD.

  METHOD zpru_if_short_memory_provider~get_long_memory.
    DATA lo_short_memory TYPE REF TO zpru_if_short_memory_provider.

    lo_short_memory = zpru_cl_dummy_agent_logic=>get_short_memory( ).
    ro_long_memory = lo_short_memory->get_long_memory( ).
  ENDMETHOD.

  METHOD zpru_if_long_memory_provider~get_msg_persistence.
    DATA lo_long_memory TYPE REF TO zpru_if_long_memory_provider.

    lo_long_memory = zpru_cl_dummy_agent_logic=>get_long_memory( ).
    ro_msg_persistence = lo_long_memory->get_msg_persistence( ).
  ENDMETHOD.

  METHOD zpru_if_long_memory_provider~get_summarization.
    DATA lo_long_memory TYPE REF TO zpru_if_long_memory_provider.

    lo_long_memory = zpru_cl_dummy_agent_logic=>get_long_memory( ).
    ro_summarization = lo_long_memory->get_summarization( ).
  ENDMETHOD.

  METHOD zpru_if_long_memory_provider~get_sum_persistence.
    DATA lo_long_memory TYPE REF TO zpru_if_long_memory_provider.

    lo_long_memory = zpru_cl_dummy_agent_logic=>get_long_memory( ).
    ro_sum_persistence = lo_long_memory->get_sum_persistence( ).
  ENDMETHOD.

  METHOD zpru_if_prompt_provider~get_system_prompt.
    DATA lo_prompt_provider TYPE REF TO zpru_if_prompt_provider.

    lo_prompt_provider = NEW lcl_prompt_provider( ).
    rv_system_prompt = lo_prompt_provider->get_system_prompt( ).
  ENDMETHOD.

  METHOD zpru_if_decision_provider~prepare_final_response.
    DATA lo_decision_provider TYPE REF TO zpru_if_decision_provider.

    lo_decision_provider = NEW lcl_decision_provider( ).
    lo_decision_provider->prepare_final_response( EXPORTING iv_run_uuid       = iv_run_uuid
                                                            iv_query_uuid     = iv_query_uuid
                                                            io_last_output    = io_last_output
                                                  IMPORTING eo_final_response = eo_final_response
                                                  CHANGING  cs_axc_reported   = cs_axc_reported
                                                            cs_axc_failed     = cs_axc_failed
                                                            cs_adf_reported   = cs_adf_reported
                                                            cs_adf_failed     = cs_adf_failed ).
  ENDMETHOD.

  METHOD zpru_if_long_memory_provider~retrieve_message.
    DATA lo_long_memory TYPE REF TO zpru_if_long_memory_provider.

    lo_long_memory = zpru_cl_dummy_agent_logic=>get_long_memory( ).
    lo_long_memory->retrieve_message( ).
  ENDMETHOD.

  METHOD zpru_if_long_memory_provider~retrieve_summary.
    DATA lo_long_memory TYPE REF TO zpru_if_long_memory_provider.

    lo_long_memory = zpru_cl_dummy_agent_logic=>get_long_memory( ).
    lo_long_memory->retrieve_summary( ).
  ENDMETHOD.

  METHOD zpru_if_short_memory_provider~save_message.
    DATA lo_short_memory TYPE REF TO zpru_if_short_memory_provider.

    lo_short_memory = zpru_cl_dummy_agent_logic=>get_short_memory( ).
    lo_short_memory->save_message( it_message = it_message ).
  ENDMETHOD.

  METHOD zpru_if_long_memory_provider~save_messages.
    DATA lo_long_memory TYPE REF TO zpru_if_long_memory_provider.

    lo_long_memory = zpru_cl_dummy_agent_logic=>get_long_memory( ).
    lo_long_memory->save_messages( EXPORTING io_input  = io_input
                                   IMPORTING eo_output = eo_output
                                             ev_error  = ev_error ).
  ENDMETHOD.

  METHOD zpru_if_long_memory_provider~save_summary.
    DATA lo_long_memory TYPE REF TO zpru_if_long_memory_provider.

    lo_long_memory = zpru_cl_dummy_agent_logic=>get_long_memory( ).
    lo_long_memory->save_summary( EXPORTING io_input  = io_input
                                  IMPORTING eo_output = eo_output
                                            ev_error  = ev_error ).
  ENDMETHOD.

  METHOD zpru_if_short_memory_provider~set_discard_strategy.
    DATA lo_short_memory TYPE REF TO zpru_if_short_memory_provider.

    lo_short_memory = zpru_cl_dummy_agent_logic=>get_short_memory( ).
    lo_short_memory->set_discard_strategy( io_discard_strategy = io_discard_strategy ).
  ENDMETHOD.

  METHOD zpru_if_short_memory_provider~set_long_memory.
    DATA lo_short_memory TYPE REF TO zpru_if_short_memory_provider.

    lo_short_memory = zpru_cl_dummy_agent_logic=>get_short_memory( ).
    lo_short_memory->set_long_memory( io_long_memory = io_long_memory ).
  ENDMETHOD.

  METHOD zpru_if_long_memory_provider~set_msg_persistence.
    DATA lo_long_memory TYPE REF TO zpru_if_long_memory_provider.

    lo_long_memory = zpru_cl_dummy_agent_logic=>get_long_memory( ).
    lo_long_memory->set_msg_persistence( io_msg_persistence = io_msg_persistence ).
  ENDMETHOD.

  METHOD zpru_if_long_memory_provider~set_summarization.
    DATA lo_long_memory TYPE REF TO zpru_if_long_memory_provider.

    lo_long_memory = zpru_cl_dummy_agent_logic=>get_long_memory( ).
    lo_long_memory->set_summarization( io_summarization = io_summarization ).
  ENDMETHOD.

  METHOD zpru_if_long_memory_provider~set_sum_persistence.
    DATA lo_long_memory TYPE REF TO zpru_if_long_memory_provider.

    lo_long_memory = zpru_cl_dummy_agent_logic=>get_long_memory( ).
    lo_long_memory->set_sum_persistence( io_sum_persistence = io_sum_persistence ).
  ENDMETHOD.

  METHOD zpru_if_long_memory_provider~summarize_conversation.
    DATA lo_long_memory TYPE REF TO zpru_if_long_memory_provider.

    lo_long_memory = zpru_cl_dummy_agent_logic=>get_long_memory( ).
    lo_long_memory->summarize_conversation( EXPORTING io_input  = io_input
                                            IMPORTING eo_output = eo_output
                                                      ev_error  = ev_error ).
  ENDMETHOD.

  METHOD zpru_if_tool_provider~get_tool.
    DATA lo_tool_provider TYPE REF TO zpru_if_tool_provider.

    lo_tool_provider = NEW lcl_tool_provider( ).
    ro_executor = lo_tool_provider->get_tool( is_tool_master_data = is_tool_master_data
                                              is_execution_step   = is_execution_step ).
  ENDMETHOD.

  METHOD zpru_if_input_schema_provider~get_input_schema.
    DATA lo_input_schema_provider TYPE REF TO zpru_if_input_schema_provider.

    lo_input_schema_provider = NEW lcl_input_schema_provider( ).
    ro_input_schema = lo_input_schema_provider->get_input_schema( is_tool_master_data = is_tool_master_data
                                                                  is_execution_step   = is_execution_step ).
  ENDMETHOD.

  METHOD zpru_if_tool_info_provider~get_tool_info.
    DATA lo_tool_info_provider TYPE REF TO zpru_if_tool_info_provider.

    lo_tool_info_provider = NEW lcl_tool_info_provider( ).
    rv_tool_info = lo_tool_info_provider->get_tool_info( is_tool_master_data = is_tool_master_data
                                               is_execution_step   = is_execution_step ).
  ENDMETHOD.

  METHOD zpru_if_short_memory_provider~get_mem_volume.
    DATA lo_short_memory TYPE REF TO zpru_if_short_memory_provider.

    lo_short_memory = zpru_cl_dummy_agent_logic=>get_short_memory( ).
    rv_mem_volume = lo_short_memory->get_mem_volume( ).

  ENDMETHOD.

  METHOD zpru_if_short_memory_provider~set_mem_volume.
    DATA lo_short_memory TYPE REF TO zpru_if_short_memory_provider.

    lo_short_memory = zpru_cl_dummy_agent_logic=>get_short_memory( ).
    lo_short_memory->set_mem_volume( iv_mem_volume = iv_mem_volume ).

  ENDMETHOD.

  METHOD get_long_memory.
    IF zpru_cl_dummy_agent_logic=>so_long_memory IS BOUND.
      ro_instance = zpru_cl_dummy_agent_logic=>so_long_memory.
      RETURN.
    ENDIF.

    zpru_cl_dummy_agent_logic=>so_long_memory = NEW lcl_long_memory_provider( ).
    ro_instance = zpru_cl_dummy_agent_logic=>so_long_memory.

  ENDMETHOD.

  METHOD get_short_memory.
    IF zpru_cl_dummy_agent_logic=>so_short_memory IS BOUND.
      ro_instance = zpru_cl_dummy_agent_logic=>so_short_memory.
      RETURN.
    ENDIF.

    zpru_cl_dummy_agent_logic=>so_short_memory = NEW lcl_short_memory_provider( ).
    ro_instance = zpru_cl_dummy_agent_logic=>so_short_memory.
  ENDMETHOD.

  METHOD get_input_prompt.
    rv_prompt = | CMR Consignment Note Data { cl_abap_char_utilities=>newline }| &&
                | Chapter 1: Points of Contact { cl_abap_char_utilities=>newline }| &&
                | Sender_Name: SteelCorp Industrial Solutions { cl_abap_char_utilities=>newline }| &&
                | Sender_Address: 123 Factory Lane, Stuttgart, Germany { cl_abap_char_utilities=>newline }| &&
                | Consignee_Name: Global Logistics Hub { cl_abap_char_utilities=>newline }| &&
                | Consignee_Address: Port Terminal 4, Rotterdam, Netherlands { cl_abap_char_utilities=>newline }| &&
                | Successive_Carriers: None { cl_abap_char_utilities=>newline }| &&
                | Chapter 2: Transport Identifiers { cl_abap_char_utilities=>newline }| &&
                | Plate_Number: XYZ-99-AB { cl_abap_char_utilities=>newline }| &&
                | Driver_Name: Hans Schneider { cl_abap_char_utilities=>newline }| &&
                | Vehicle_Type: Heavy Duty Trailer { cl_abap_char_utilities=>newline }| &&
                | Carrier_ID: DHL-LOG-552 { cl_abap_char_utilities=>newline }| &&
                | Chapter 3: Goods Specification { cl_abap_char_utilities=>newline }| &&
                | Goods_Description: Drums of Industrial Solvent { cl_abap_char_utilities=>newline }| &&
                | UN_Number: UN 1203 { cl_abap_char_utilities=>newline }| &&
                | Hazard_Class: 3 { cl_abap_char_utilities=>newline }| &&
                | Number_of_Packages: 50 { cl_abap_char_utilities=>newline }| &&
                | Gross_Weight_KG: 10500 { cl_abap_char_utilities=>newline }| &&
                | Volume_M3: 15.5 { cl_abap_char_utilities=>newline }| &&
                | Chapter 4: Logistics Requirements { cl_abap_char_utilities=>newline }| &&
                | Customs_Status: Cleared { cl_abap_char_utilities=>newline }| &&
                | Temperature_Control: Not Required { cl_abap_char_utilities=>newline }| &&
                | Special_Instructions: Fragile loading { cl_abap_char_utilities=>newline }| &&
                | Payment_Terms: Prepaid |.
  ENDMETHOD.

ENDCLASS.
