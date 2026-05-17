# How to Build Your AIPF Agent — Developer Guide

> **Framework:** AIPF — Agent Integration Processing Framework  
> **Pattern:** Class Facade + Local Classes (all-in-one ABAP class)  
> **Based on:** `aipf-demo-visual-recognition` (Visual Recognition Agent)

---

## Table of Contents

1. [Architecture Overview](#1-architecture-overview)
2. [Step 1 — Create the Interface](#2-step-1--create-the-interface)
3. [Step 2 — Create the Main Facade Class](#3-step-2--create-the-main-facade-class)
4. [Step 3 — Implement Local Classes (Decision Provider)](#4-step-3--implement-local-classes-decision-provider)
5. [Step 4 — Implement Agent Info Provider](#5-step-4--implement-agent-info-provider)
6. [Step 5 — Implement System Prompt Provider](#6-step-5--implement-system-prompt-provider)
7. [Step 6 — Implement Tools (ABAP Code Executors)](#7-step-6--implement-tools-abap-code-executors)
8. [Step 7 — Implement Tool Provider, Tool Info & Schema Providers](#8-step-7--implement-tool-provider-tool-info--schema-providers)
9. [Step 8 — Create Agent Type](#9-step-8--create-agent-type)
10. [Step 9 — Create Agent Definition](#10-step-9--create-agent-definition)
11. [Step 10 — Create Agent Tool Entries](#11-step-10--create-agent-tool-entries)
12. [Step 11 — Run Your Agent](#12-step-11--run-your-agent)
13. [Complete File Structure](#13-complete-file-structure)
14. [Appendix — Template Reference](#14-appendix--template-reference)

---

## 1. Architecture Overview

The AIPF framework expects your agent to be a single ABAP class that acts as a **facade**. This class implements **all** required framework interfaces and delegates each one to a dedicated **local class** (defined in `locals_def` / `locals_imp` includes).

### Benefits of the Facade Pattern

- **Single ABAP class** in the database (`ZPRU_CL_YOUR_AGENT`)
- **Clean separation** via local classes inside the same source
- **All interfaces** mapped in one place
- **Easy to install** — one abapGit import per agent

### Required Interfaces

The facade class implements `ZPRU_IF_AGENT_IMPL_UNIFIED` (or each interface individually), which bundles these roles:

| Role | Interface | Local Class |
|------|-----------|-------------|
| Decision Engine | `ZPRU_IF_DECISION_PROVIDER` | `lcl_adf_decision_provider` |
| Short Memory | `ZPRU_IF_SHORT_MEMORY_PROVIDER` | `lcl_adf_short_memory_provider` |
| Long Memory | `ZPRU_IF_LONG_MEMORY_PROVIDER` | `lcl_adf_long_memory_provider` |
| Agent Info | `ZPRU_IF_AGENT_INFO_PROVIDER` | `lcl_adf_agent_info_provider` |
| System Prompt | `ZPRU_IF_PROMPT_PROVIDER` | `lcl_adf_syst_prompt_provider` |
| Tool Mapper | `ZPRU_IF_AGENT_MAPPER` | `lcl_adf_agent_mapper` |
| Tool Provider | `ZPRU_IF_TOOL_PROVIDER` | `lcl_adf_tool_provider` |
| Tool Info | `ZPRU_IF_TOOL_INFO_PROVIDER` | `lcl_adf_tool_info_provider` |
| Tool Schema | `ZPRU_IF_TOOL_SCHEMA_PROVIDER` | `lcl_adf_schema_provider` |
| Singleton | `ZPRU_IF_AGENT_SINGLETON_METH` | *(in facade itself)* |

> **Note:** The memory providers inherit from framework base classes (`ZPRU_CL_SHORT_MEMORY_BASE`, `ZPRU_CL_LONG_MEMORY_BASE`) and need no custom code unless you need bespoke memory logic.

---

## 2. Step 1 — Create the Interface

Create a global ABAP interface that defines **all types, context field constants, and tool schemas** for your agent.

### What to Define

1. **Tool name constants** — one constant per tool
2. **Context field constants** — each data field that flows between tools
3. **Input tool structures** — input ABAP structure for each tool
4. **Input/Output context field mappings** — which fields each tool consumes/produces
5. **Context field types** — actual ABAP types for each context field

### Example Naming Convention

| Object | Name |
|--------|------|
| Interface | `ZPRU_IF_YOUR_AGENT` |
| Main class | `ZPRU_CL_YOUR_AGENT` |
| Test data | `ZPRU_CL_YOUR_AGENT_TEST_DATA` |

### Template for Interface

```abap
INTERFACE zpru_if_your_agent
  PUBLIC.
  INTERFACES zpru_if_agent_impl_unified.

  " ── Tool Name Constants ──────────────────────────
  CONSTANTS: BEGIN OF cs_tools,
               my_tool_a TYPE string VALUE `MY_TOOL_A`,
               my_tool_b TYPE string VALUE `MY_TOOL_B`,
             END OF cs_tools.

  " ── Context Field Constants ─────────────────────
  CONSTANTS: BEGIN OF cs_context_field,
               BEGIN OF output_data,
                 field_name    TYPE string VALUE `OUTPUTDATA`,
                 absolute_name TYPE string VALUE `\INTERF=ZPRU_IF_YOUR_AGENT\TYPE=TT_OUTPUT_DATA`,
               END OF output_data,
             END OF cs_context_field.

  " ── Input Structure for Tool A ──────────────────
  TYPES:
    BEGIN OF ts_my_tool_a_input,
      inputdata TYPE string,
    END OF ts_my_tool_a_input,
    tt_my_tool_a_input TYPE STANDARD TABLE OF ts_my_tool_a_input WITH EMPTY KEY.

  " ── Context Field Types ─────────────────────────
  TYPES:
    BEGIN OF ts_output_data,
      field1 TYPE string,
      field2 TYPE int4,
    END OF ts_output_data,
    tt_output_data TYPE STANDARD TABLE OF ts_output_data WITH EMPTY KEY.

ENDINTERFACE.
```

---

## 3. Step 2 — Create the Main Facade Class

The main class is the **public face** of your agent. It implements all interfaces and delegates to local classes.

### File 1: Main Class (`zpru_cl_your_agent.clas.abap`)

```abap
CLASS zpru_cl_your_agent DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zpru_if_your_agent.              " Your custom interface
    INTERFACES zpru_if_agent_impl_unified.      " Or implement individual interfaces

  PRIVATE SECTION.
    CLASS-DATA so_adf_short_memory TYPE REF TO lcl_adf_short_memory_provider.
    CLASS-DATA so_adf_long_memory  TYPE REF TO lcl_adf_long_memory_provider.

ENDCLASS.


CLASS zpru_cl_your_agent IMPLEMENTATION.

  " ═══════════════════════════════════════════════
  " DECISION PROVIDER — Delegates to local class
  " ═══════════════════════════════════════════════
  METHOD zpru_if_decision_provider~call_decision_engine.
    DATA(lo_provider) = NEW lcl_adf_decision_provider( ).
    lo_provider->call_decision_engine(
      EXPORTING is_agent               = is_agent
                it_tool                = it_tool
                io_controller          = io_controller
                io_input               = io_input
                is_input_prompt        = is_input_prompt
                io_system_prompt       = io_system_prompt
                io_short_memory        = io_short_memory
                io_long_memory         = io_long_memory
                io_agent_info_provider = io_agent_info_provider
      IMPORTING eo_execution_plan      = eo_execution_plan
                eo_first_tool_input    = eo_first_tool_input
                eo_langu               = eo_langu
                eo_decision_log        = eo_decision_log ).
  ENDMETHOD.

  METHOD zpru_if_decision_provider~prepare_final_response.
    DATA(lo_provider) = NEW lcl_adf_decision_provider( ).
    lo_provider->prepare_final_response(
      EXPORTING iv_run_uuid       = iv_run_uuid
                iv_query_uuid     = iv_query_uuid
                io_controller     = io_controller
                io_last_output    = io_last_output
      IMPORTING eo_final_response = eo_final_response
      CHANGING  cs_axc_reported   = cs_axc_reported
                cs_axc_failed     = cs_axc_failed
                cs_adf_reported   = cs_adf_reported
                cs_adf_failed     = cs_adf_failed ).
  ENDMETHOD.

  " ═══════════════════════════════════════════════
  " SHORT MEMORY — Delegates with singleton pattern
  " ═══════════════════════════════════════════════

  METHOD zpru_if_agent_singleton_meth~get_short_memory.
    IF zpru_cl_your_agent=>so_adf_short_memory IS BOUND.
      ro_instance = zpru_cl_your_agent=>so_adf_short_memory.
      RETURN.
    ENDIF.
    zpru_cl_your_agent=>so_adf_short_memory = NEW lcl_adf_short_memory_provider( ).
    ro_instance = zpru_cl_your_agent=>so_adf_short_memory.
  ENDMETHOD.

  METHOD zpru_if_short_memory_provider~clear_history.
    DATA(lo_mem) = zpru_cl_your_agent=>zpru_if_agent_singleton_meth~get_short_memory( ).
    lo_mem->clear_history( ).
  ENDMETHOD.

  " ... (same delegation pattern for ALL short memory methods)

  " ═══════════════════════════════════════════════
  " LONG MEMORY — Delegates with singleton pattern
  " ═══════════════════════════════════════════════

  METHOD zpru_if_agent_singleton_meth~get_long_memory.
    IF zpru_cl_your_agent=>so_adf_long_memory IS BOUND.
      ro_instance = zpru_cl_your_agent=>so_adf_long_memory.
      RETURN.
    ENDIF.
    zpru_cl_your_agent=>so_adf_long_memory = NEW lcl_adf_long_memory_provider( ).
    ro_instance = zpru_cl_your_agent=>so_adf_long_memory.
  ENDMETHOD.

  " ═══════════════════════════════════════════════
  " AGENT INFO PROVIDER
  " ═══════════════════════════════════════════════
  METHOD zpru_if_agent_info_provider~get_agent_info.
    DATA(lo_provider) = NEW lcl_adf_agent_info_provider( ).
    rv_agent_info = lo_provider->get_agent_info( iv_agent_uuid = iv_agent_uuid ).
  ENDMETHOD.

  " ═══════════════════════════════════════════════
  " SYSTEM PROMPT PROVIDER
  " ═══════════════════════════════════════════════
  METHOD zpru_if_prompt_provider~get_system_prompt.
    DATA(lo_provider) = NEW lcl_adf_syst_prompt_provider( ).
    rv_system_prompt = lo_provider->get_system_prompt( iv_agent_uuid = iv_agent_uuid ).
  ENDMETHOD.

  " ═══════════════════════════════════════════════
  " AGENT MAPPER
  " ═══════════════════════════════════════════════
  METHOD zpru_if_agent_mapper~map_tools_parameter.
    DATA(lo_mapper) = NEW lcl_adf_agent_mapper( ).
    lo_mapper->map_tools_parameter(
      EXPORTING io_request                   = io_request
                iv_input_string              = iv_input_string
                is_curr_tool_master_data     = is_curr_tool_master_data
                is_curr_execution_step       = is_curr_execution_step
                io_controller                = io_controller
                io_util                      = io_util
                io_curr_tool_schema_provider = io_curr_tool_schema_provider
                it_key_value_pair            = it_key_value_pair
      IMPORTING ev_error_flag                = ev_error_flag
      CHANGING  cr_input                     = cr_input ).
  ENDMETHOD.

  " ═══════════════════════════════════════════════
  " TOOL PROVIDER
  " ═══════════════════════════════════════════════
  METHOD zpru_if_tool_provider~get_tool.
    DATA(lo_provider) = NEW lcl_adf_tool_provider( ).
    ro_executor = lo_provider->get_tool(
      is_agent            = is_agent
      io_controller       = io_controller
      io_input            = io_input
      is_tool_master_data = is_tool_master_data
      is_execution_step   = is_execution_step ).
  ENDMETHOD.

  " ═══════════════════════════════════════════════
  " TOOL INFO PROVIDER
  " ═══════════════════════════════════════════════
  METHOD zpru_if_tool_info_provider~get_tool_info.
    DATA(lo_provider) = NEW lcl_adf_tool_info_provider( ).
    rv_tool_info = lo_provider->get_tool_info(
      is_tool_master_data = is_tool_master_data
      is_execution_step   = is_execution_step ).
  ENDMETHOD.

  " ═══════════════════════════════════════════════
  " TOOL SCHEMA PROVIDER
  " ═══════════════════════════════════════════════
  METHOD zpru_if_tool_schema_provider~input_json_schema.
    CLEAR: ev_json_schema, es_json_structure.
    DATA(lo_provider) = NEW lcl_adf_schema_provider( ).
    lo_provider->input_json_schema(
      EXPORTING is_tool_master_data = is_tool_master_data
                is_execution_step   = is_execution_step
      IMPORTING ev_json_schema      = ev_json_schema
                es_json_structure   = es_json_structure ).
  ENDMETHOD.

  METHOD zpru_if_tool_schema_provider~input_rtts_schema.
    DATA(lo_provider) = NEW lcl_adf_schema_provider( ).
    ro_structure_schema = lo_provider->input_rtts_schema(
      is_tool_master_data = is_tool_master_data
      is_execution_step   = is_execution_step ).
  ENDMETHOD.

ENDCLASS.
```

> **Note:** All short memory and long memory methods follow the exact same delegation pattern shown above. See the [template reference](#14-appendix--template-reference) for the complete list.

---

## 4. Step 3 — Implement Local Classes (Decision Provider)

### File 2: Local Definitions (`zpru_cl_your_agent.clas.locals_def.abap`)

```abap
" ── Common Utility Algorithms ────────────────────
CLASS lcl_common_algorithms DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    CLASS-METHODS get_last_thinkingstepnumber
      IMPORTING it_thinking_step                  TYPE zpru_tt_thinking_step
      RETURNING VALUE(rv_last_thinkingstepnumber) TYPE i.
    CLASS-METHODS get_timestamp
      RETURNING VALUE(rv_now) TYPE timestampl.
    CLASS-METHODS get_llm_api_factory
      RETURNING VALUE(ro_llm_api_factory) TYPE REF TO if_aic_islm_compl_api_factory.
ENDCLASS.


" ── DECISION PROVIDER ────────────────────────────
" Inherit from ZPRU_CL_DECISION_PROVIDER
" This is the BRAIN of your agent — defines:
"   • process_thinking     — LLM call + execution plan generation
"   • prepare_first_tool_input — transform LLM output → first tool input
"   • set_final_response_content — build final response
"   • recall_memory        — load past conversations
"   • read_data_4_thinking — RAG/context data
CLASS lcl_adf_decision_provider DEFINITION INHERITING FROM zpru_cl_decision_provider CREATE PUBLIC.
  PROTECTED SECTION.
    METHODS check_authorizations        REDEFINITION.
    METHODS recall_memory               REDEFINITION.
    METHODS read_data_4_thinking        REDEFINITION.
    METHODS process_thinking            REDEFINITION.
    METHODS prepare_first_tool_input    REDEFINITION.
    METHODS set_model_id                REDEFINITION.
    METHODS set_result_comment          REDEFINITION.
    METHODS set_final_response_content  REDEFINITION.
    METHODS set_final_response_metadata REDEFINITION.
ENDCLASS.


" ── SHORT MEMORY PROVIDER ────────────────────────
" Inherit from base — no custom methods needed
CLASS lcl_adf_short_memory_provider DEFINITION INHERITING FROM zpru_cl_short_memory_base CREATE PUBLIC.
ENDCLASS.


" ── LONG MEMORY PROVIDER ─────────────────────────
" Inherit from base — no custom methods needed
CLASS lcl_adf_long_memory_provider DEFINITION INHERITING FROM zpru_cl_long_memory_base CREATE PUBLIC.
ENDCLASS.


" ── AGENT INFO PROVIDER ──────────────────────────
CLASS lcl_adf_agent_info_provider DEFINITION INHERITING FROM zpru_cl_agent_info_provider CREATE PUBLIC.
  PROTECTED SECTION.
    METHODS get_agent_main_info    REDEFINITION.
    METHODS set_agent_goals        REDEFINITION.
    METHODS prepare_agent_domains  REDEFINITION.
    METHODS set_agent_restrictions REDEFINITION.
    METHODS set_tool_metadata      REDEFINITION.
    METHODS get_free_text          REDEFINITION.
ENDCLASS.


" ── SYSTEM PROMPT PROVIDER ───────────────────────
CLASS lcl_adf_syst_prompt_provider DEFINITION INHERITING FROM zpru_cl_syst_prmpt_prvdr_base CREATE PUBLIC.
  PROTECTED SECTION.
    METHODS set_primary_session_task REDEFINITION.
    METHODS set_technical_rules      REDEFINITION.
    METHODS set_business_rules       REDEFINITION.
    METHODS set_format_guidelines    REDEFINITION.
    METHODS set_reasoning_step       REDEFINITION.
    METHODS set_prompt_restrictions  REDEFINITION.
    METHODS set_arbitrary_text       REDEFINITION.
ENDCLASS.


" ── AGENT MAPPER ─────────────────────────────────
" Usually empty (no overrides needed)
CLASS lcl_adf_agent_mapper DEFINITION INHERITING FROM zpru_cl_agent_mapper CREATE PUBLIC.
ENDCLASS.


" ── TOOL: MY_TOOL_A ──────────────────────────────
" Each tool is a separate local class inheriting from ZPRU_CL_ABAP_EXECUTOR
CLASS lcl_adf_my_tool_a DEFINITION INHERITING FROM zpru_cl_abap_executor CREATE PUBLIC.
  PROTECTED SECTION.
    METHODS execute_code_int REDEFINITION.

    " Private helper methods for your tool logic
    METHODS my_helper_method.
ENDCLASS.


" ── TOOL: MY_TOOL_B ──────────────────────────────
CLASS lcl_adf_my_tool_b DEFINITION INHERITING FROM zpru_cl_abap_executor CREATE PUBLIC.
  PROTECTED SECTION.
    METHODS execute_code_int REDEFINITION.
ENDCLASS.


" ── TOOL PROVIDER ────────────────────────────────
" Maps tool names → local tool class instances
CLASS lcl_adf_tool_provider DEFINITION INHERITING FROM zpru_cl_tool_provider CREATE PUBLIC.
  PROTECTED SECTION.
    METHODS provide_tool_instance REDEFINITION.
ENDCLASS.


" ── TOOL INFO PROVIDER ──────────────────────────
CLASS lcl_adf_tool_info_provider DEFINITION INHERITING FROM zpru_cl_tool_info_provider CREATE PUBLIC.
  PROTECTED SECTION.
    METHODS get_main_tool_info  REDEFINITION.
    METHODS set_tool_properties REDEFINITION.
    METHODS set_tool_parameters REDEFINITION.
ENDCLASS.


" ── TOOL SCHEMA PROVIDER ─────────────────────────
CLASS lcl_adf_schema_provider DEFINITION INHERITING FROM zpru_cl_tool_schema_provider CREATE PUBLIC.
  PROTECTED SECTION.
    METHODS get_input_abap_type   REDEFINITION.
    METHODS get_input_json_schema REDEFINITION.
ENDCLASS.
```

### File 3: Local Implementations (`zpru_cl_your_agent.clas.locals_imp.abap`)

#### 4.1 Decision Provider — `process_thinking`

This is where you define **how your agent thinks**:

1. Call an LLM (or use mock data)
2. Parse the response
3. Generate an execution plan (ordered list of tools to run)

```abap
CLASS lcl_adf_decision_provider IMPLEMENTATION.

  METHOD check_authorizations.
    ev_allowed = abap_true.
  ENDMETHOD.

  METHOD process_thinking.
    " ── 1. Decide what your agent should do ────────
    " Option A: Call an LLM (AI agent)
    " DATA(lv_prompt) = is_input_prompt-string_content.
    " DATA(lo_factory) = lcl_common_algorithms=>get_llm_api_factory( ).
    " DATA(lo_api) = lo_factory->create_instance( 'ST-GEMINI-3.0' ).
    " DATA(lv_response) = lo_api->execute_for_string( lv_prompt )->get_completion( ).

    " Option B: Deterministic (IF-ELSE agent)
    " IF ls_input-some_field = 'A'.
    "   " ... run tool A
    " ENDIF.

    " ── 2. Generate Execution Plan ─────────────────
    " An execution plan tells the framework WHICH tools to run and in WHAT order.
    " Tools are referenced by their names (must match ZPRU_AGENT_TOOL entries).

    et_execution_plan = VALUE #(
        agentuuid = is_agent-agentuuid
        ( toolname = zpru_if_your_agent=>cs_tools-my_tool_a
          sequence = 1 )
        ( toolname = zpru_if_your_agent=>cs_tools-my_tool_b
          sequence = 2 ) ).

    " ── 3. Set thinking output (if LLM was used) ──
    " ev_thinking_output = lv_response.

    ev_langu = sy-langu.
  ENDMETHOD.

  METHOD prepare_first_tool_input.
    " Transform the thinking output into the input structure
    " expected by the first tool in the execution plan.

    " CASE is_first_tool-toolname.
    "   WHEN zpru_if_your_agent=>cs_tools-my_tool_a.
    "     er_first_tool_input = NEW zpru_if_your_agent=>ts_my_tool_a_input(
    "         inputdata = lv_parsed_data ).
    " ENDCASE.
  ENDMETHOD.

  METHOD read_data_4_thinking.
    " Load RAG (knowledge base) data and user context.
    " et_rag_data  — knowledge chunks
    " ev_user_data — user-specific context as JSON string

    " Log thinking steps for transparency:
    APPEND INITIAL LINE TO cs_decision_log-thinkingsteps ASSIGNING FIELD-SYMBOL(<ls_step>).
    <ls_step>-thinkingstepnumber   = get_last_thinkingstepnumber( cs_decision_log-thinkingsteps ).
    <ls_step>-thinkingstepdatetime = get_timestamp( ).
    <ls_step>-thinkingstepcontent  = `RAG data loaded`.
  ENDMETHOD.

  METHOD recall_memory.
    " Retrieve past conversation history:

    " 1. Short-term (current session) messages
    " et_session_memory = io_short_memory->get_history( ).

    " 2. Long-term (persisted) messages from database
    " et_episodic_message_memory = io_long_memory->retrieve_message( ... ).

    " 3. Long-term summaries
    " et_episodic_summary_memory = io_long_memory->retrieve_summary( ... ).
  ENDMETHOD.

  METHOD set_final_response_content.
    " Build the final response after all tools have executed.

    " 1. Read execution data (runtime metadata)
    " 2. Attach freshest context (key-value pairs from last tool output)
    " 3. Set response content and type

    SORT io_controller->mt_input_output BY number DESCENDING.
    DATA(lt_freshest) = VALUE #( io_controller->mt_input_output[ 1 ]-key_value_pairs OPTIONAL ).

    LOOP AT lt_freshest ASSIGNING FIELD-SYMBOL(<ls_kv>).
      APPEND INITIAL LINE TO cs_final_response_body-structureddata ASSIGNING FIELD-SYMBOL(<ls_sd>).
      <ls_sd>-name  = <ls_kv>-name.
      <ls_sd>-value = <ls_kv>-value.
    ENDLOOP.

    " Use utility to convert structured data to JSON string
    " DATA(lo_util) = CAST zpru_if_agent_util(
    "     zpru_cl_agent_service_mngr=>get_service( ... ) ).
    " lo_util->convert_to_string(
    "     EXPORTING ir_abap = REF #( cs_final_response_body-structureddata )
    "     CHANGING  cr_string = cs_final_response_body-responsecontent ).

    cs_final_response_body-type = cl_abap_datadescr=>describe_by_data(
        p_data = cs_final_response_body-structureddata )->absolute_name.
  ENDMETHOD.

  METHOD set_final_response_metadata.
    cs_reasoning_trace-rationalsummary = 'Agent processing completed.'.
    cs_reasoning_trace-confidencescore = `90.00`.
  ENDMETHOD.

  METHOD set_model_id.
    rv_model_id = `MY_MODEL_ID`.
  ENDMETHOD.

  METHOD set_result_comment.
    rv_result_comment = `Agent processing finished successfully`.
  ENDMETHOD.

ENDCLASS.
```

#### 4.2 Memory Providers (Short + Long)

Usually empty — base classes handle everything:

```abap
CLASS lcl_adf_short_memory_provider IMPLEMENTATION.
ENDCLASS.

CLASS lcl_adf_long_memory_provider IMPLEMENTATION.
ENDCLASS.
```

---

## 5. Step 4 — Implement Agent Info Provider

Defines the **identity and purpose** of your agent. This is what the LLM "knows" about itself.

```abap
CLASS lcl_adf_agent_info_provider IMPLEMENTATION.

  METHOD get_agent_main_info.
    ev_agentname    = `My Custom Agent`.
    ev_agentversion = `Version 1.0.0`.
    ev_agentrole    = `Describe what this agent does in one sentence.`.
  ENDMETHOD.

  METHOD get_free_text.
    ev_freetextcontent = |Extended description of the agent's capabilities.|.
  ENDMETHOD.

  METHOD prepare_agent_domains.
    rs_agent_domains-agentdomainname    = `Main Domain`.
    rs_agent_domains-agentdomaincontent = `Description of the domain.`.

    APPEND INITIAL LINE TO rs_agent_domains-agentsubdomains ASSIGNING FIELD-SYMBOL(<ls_sub>).
    <ls_sub>-agentsubdomainname    = `Sub-domain 1`.
    <ls_sub>-agentsubdomaincontent = `Description of sub-domain 1.`.
  ENDMETHOD.

  METHOD set_agent_goals.
    APPEND INITIAL LINE TO rt_agent_goals ASSIGNING FIELD-SYMBOL(<ls_goal>).
    <ls_goal>-agentgoalid              = 1.
    <ls_goal>-agentgoaldescription     = `Goal 1 Title`.
    <ls_goal>-agentgoalpriority        = 1.
    <ls_goal>-agentgoalcontent         = `What the agent aims to achieve as goal 1.`.
    <ls_goal>-agentgoalsuccesscriteria = `How success is measured for goal 1.`.
  ENDMETHOD.

  METHOD set_agent_restrictions.
    APPEND INITIAL LINE TO rt_agent_restrictions ASSIGNING FIELD-SYMBOL(<ls_res>).
    <ls_res>-agentrestrictionname = `RESTRICTION_NAME`.
    <ls_res>-agentrestriction     = `Description of the restriction (read-only, no financial, etc).`.
  ENDMETHOD.

  METHOD set_tool_metadata.
    " Describe each tool so the LLM knows when to use it
    APPEND INITIAL LINE TO rt_tool_metadata ASSIGNING FIELD-SYMBOL(<ls_tool>).
    <ls_tool>-toolname        = `MY_TOOL_A`.
    <ls_tool>-tooldesciption  = `Short tool description`.
    <ls_tool>-toolexplanation = `Detailed explanation of what the tool does.`.
    <ls_tool>-tooltype        = zpru_if_adf_type_and_constant=>cs_step_type-abap_code.
  ENDMETHOD.

ENDCLASS.
```

---

## 6. Step 5 — Implement System Prompt Provider

Defines the **system instructions** sent to the LLM.

```abap
CLASS lcl_adf_syst_prompt_provider IMPLEMENTATION.

  METHOD set_primary_session_task.
    ev_primary_session_task =
      `Describe the main task the agent must perform.`.
  ENDMETHOD.

  METHOD set_business_rules.
    APPEND INITIAL LINE TO rt_business_rules ASSIGNING FIELD-SYMBOL(<ls_rule>).
    <ls_rule>-businessrulesname = `RULE_1`.
    <ls_rule>-businessrule      = `Always use USD as currency.`.
  ENDMETHOD.

  METHOD set_format_guidelines.
    APPEND INITIAL LINE TO rt_format_guidelines ASSIGNING FIELD-SYMBOL(<ls_guide>).
    <ls_guide>-formatguidelinename = `NO_MARKDOWN`.
    <ls_guide>-formatguideline     = `Return raw JSON only. No markdown wrappers.`.
  ENDMETHOD.

  METHOD set_technical_rules.
    APPEND INITIAL LINE TO rt_tech_rules ASSIGNING FIELD-SYMBOL(<ls_tech>).
    <ls_tech>-technicalrulesname = `TIMEOUT`.
    <ls_tech>-technicalrule      = `API calls should not exceed 30 seconds.`.
  ENDMETHOD.

  METHOD set_reasoning_step.
    APPEND INITIAL LINE TO rt_reasoning_step ASSIGNING FIELD-SYMBOL(<ls_step>).
    <ls_step>-reasoningstepname        = `STEP_1`.
    <ls_step>-reasoningstepquestion    = `Question the LLM should ask itself.`.
    <ls_step>-reasoninginstruction     = `Instruction on how to reason.`.
    <ls_step>-reasoningstepismandatory = abap_true.
  ENDMETHOD.

  METHOD set_prompt_restrictions.
    APPEND INITIAL LINE TO rt_prompt_restrictions ASSIGNING FIELD-SYMBOL(<ls_res>).
    <ls_res>-promptrestrictionname = `NO_HALLUCINATION`.
    <ls_res>-promptrestriction     = `Do not invent data.`.
  ENDMETHOD.

  METHOD set_arbitrary_text.
    ev_arbitrarytexttitle = `ADDITIONAL_INFO`.
    ev_arbitrarytext = `Any additional free-text context.`.
  ENDMETHOD.

ENDCLASS.
```

---

## 7. Step 6 — Implement Tools (ABAP Code Executors)

Each tool is a local class inheriting from `ZPRU_CL_ABAP_EXECUTOR`. You implement the `execute_code_int` method.

### Anatomy of a Tool

```abap
CLASS lcl_adf_my_tool_a IMPLEMENTATION.

  METHOD execute_code_int.
    " ── 1. Read input ─────────────────────────────
    " is_input  → REF TO data (typed as your tool's input structure)
    " es_output → REF TO data (you set this to the output table)
    " et_key_value_pairs → output context fields (passed to next tools)

    FIELD-SYMBOLS <ls_input> TYPE zpru_if_your_agent=>ts_my_tool_a_input.
    ASSIGN is_input->* TO <ls_input>.
    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zpru_cx_agent_core( ).
    ENDIF.

    " ── 2. Execute business logic ─────────────────
    " Call BAPIs, read/write database, invoke RAP, etc.

    " ── 3. Prepare output context fields ──────────
    " These key-value pairs flow to the next tool in the plan
    DATA lt_output TYPE zpru_tt_key_value.

    APPEND INITIAL LINE TO lt_output ASSIGNING FIELD-SYMBOL(<ls_kv>).
    <ls_kv>-name  = zpru_if_your_agent=>cs_context_field-output_data-field_name.
    <ls_kv>-type  = cl_abap_typedescr=>describe_by_data(
        p_data = VALUE string( ) )->absolute_name.
    <ls_kv>-value = `{"result": "success"}`.  " Serialized JSON

    " ── 4. Set output ─────────────────────────────
    es_output = NEW zpru_tt_key_value( lt_output ).
    et_key_value_pairs = lt_output.

    " ── 5. (Optional) Trigger miniloop ────────────
    " You can add extra steps to be executed immediately after this tool:
    " APPEND INITIAL LINE TO et_additional_step ASSIGNING FIELD-SYMBOL(<ls_add>).
    " <ls_add>-toolname = `MY_DYNAMIC_TOOL`.
  ENDMETHOD.

ENDCLASS.
```

---

## 8. Step 7 — Implement Tool Provider, Tool Info & Schema Providers

### 8.1 Tool Provider — Routes tool names to instances

```abap
CLASS lcl_adf_tool_provider IMPLEMENTATION.
  METHOD provide_tool_instance.
    CASE is_tool_master_data-toolname.
      WHEN zpru_if_your_agent=>cs_tools-my_tool_a.
        ro_executor = NEW lcl_adf_my_tool_a( ).
      WHEN zpru_if_your_agent=>cs_tools-my_tool_b.
        ro_executor = NEW lcl_adf_my_tool_b( ).
      WHEN OTHERS.
        RETURN.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
```

### 8.2 Tool Info Provider

```abap
CLASS lcl_adf_tool_info_provider IMPLEMENTATION.
  METHOD get_main_tool_info.
  ENDMETHOD.
  METHOD set_tool_parameters.
  ENDMETHOD.
  METHOD set_tool_properties.
  ENDMETHOD.
ENDCLASS.
```

### 8.3 Tool Schema Provider — Defines input structure for each tool

```abap
CLASS lcl_adf_schema_provider IMPLEMENTATION.

  METHOD get_input_abap_type.
    " Return the RTTS descriptor for each tool's input structure
    CASE is_tool_master_data-toolname.
      WHEN zpru_if_your_agent=>cs_tools-my_tool_a.
        ro_structure_schema ?= cl_abap_structdescr=>describe_by_name(
            p_name = `ZPRU_IF_YOUR_AGENT` ). " <- your type name
      WHEN OTHERS.
        RETURN.
    ENDCASE.
  ENDMETHOD.

  METHOD get_input_json_schema.
    " Return the JSON schema for each tool's input
    CASE is_tool_master_data-toolname.
      WHEN zpru_if_your_agent=>cs_tools-my_tool_a.
        ev_json_schema = `{ "type": "object", "properties": { ... } }`.
      WHEN OTHERS.
        RETURN.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.
```

---

## 9. Step 8 — Create Agent Type

Before you can create an agent, you need an **agent type**. This is a simple entry in table `ZPRU_AGENT_TYPE`.

Use the test data class or a direct `MODIFY`:

```abap
" Run this once in ADT (as a class or classrun)
DATA ls_agent_type TYPE zpru_agent_type.
ls_agent_type-agenttype = 'AGTYP1'.
ls_agent_type-typename  = 'Your Agent Type'.
ls_agent_type-description = 'Description of your agent type'.
MODIFY zpru_agent_type FROM @ls_agent_type.
IF sy-subrc = 0.
  COMMIT WORK.
ENDIF.
```

> **Tip:** If you already have an agent type `AGTYP1` (from the template), you can reuse it.

---

## 10. Step 9 — Create Agent Definition

Insert a row in table `ZPRU_AGENT`. All class names point to your **main facade class**.

```abap
DATA lv_agent_uuid TYPE sysuuid_x16.
TRY.
    lv_agent_uuid = cl_system_uuid=>create_uuid_x16_static( ).
  CATCH cx_uuid_error.
ENDTRY.

DATA ls_agent TYPE zpru_agent.
ls_agent-agentuuid            = lv_agent_uuid.
ls_agent-agentname            = 'MY_FIRST_AGENT'.
ls_agent-agenttype            = 'AGTYP1'.
ls_agent-agentstatus          = 'N'.
ls_agent-decisionprovider     = 'ZPRU_CL_YOUR_AGENT'.    " Your facade class
ls_agent-shortmemoryprovider  = 'ZPRU_CL_YOUR_AGENT'.    " Same facade class
ls_agent-longmemoryprovider   = 'ZPRU_CL_YOUR_AGENT'.    " Same facade class
ls_agent-agentinfoprovider    = 'ZPRU_CL_YOUR_AGENT'.    " Same facade class
ls_agent-systempromptprovider = 'ZPRU_CL_YOUR_AGENT'.    " Same facade class
ls_agent-agentmapper          = 'ZPRU_CL_YOUR_AGENT'.    " Same facade class

MODIFY zpru_agent FROM @ls_agent.
IF sy-subrc = 0.
  COMMIT WORK.
ENDIF.
```

> **Key point:** All five provider columns + the agent mapper column point to the **same** class — your facade. That class implements all the required interfaces and delegates to local classes internally.

---

## 11. Step 10 — Create Agent Tool Entries

For each tool, insert a row in table `ZPRU_AGENT_TOOL` referencing the agent's UUID.

```abap
TRY.
    DATA lt_agent_tool TYPE STANDARD TABLE OF zpru_agent_tool.

    lt_agent_tool = VALUE #(
      ( tooluuid           = cl_system_uuid=>create_uuid_x16_static( )
        agentuuid          = lv_agent_uuid      " Same UUID as the agent
        toolname           = 'MY_TOOL_A'
        toolprovider       = 'ZPRU_CL_YOUR_AGENT'   " Facade class
        steptype           = 'B'                     " B = ABAP Code executor
        toolschemaprovider = 'ZPRU_CL_YOUR_AGENT'   " Facade class
        toolinfoprovider   = 'ZPRU_CL_YOUR_AGENT'   " Facade class  )
      ( tooluuid           = cl_system_uuid=>create_uuid_x16_static( )
        agentuuid          = lv_agent_uuid
        toolname           = 'MY_TOOL_B'
        toolprovider       = 'ZPRU_CL_YOUR_AGENT'
        steptype           = 'B'
        toolschemaprovider = 'ZPRU_CL_YOUR_AGENT'
        toolinfoprovider   = 'ZPRU_CL_YOUR_AGENT'  ) ).

    MODIFY zpru_agent_tool FROM TABLE @lt_agent_tool.
    IF sy-subrc = 0.
      COMMIT WORK.
    ENDIF.
  CATCH cx_uuid_error.
ENDTRY.
```

### Step Type Values

| Steptype | Description |
|----------|-------------|
| `B` | ABAP Code Executor (most common) |
| `K` | Knowledge Source |
| `N` | Nested Agent |
| `H` | HTTP Request |
| `C` | Service Consumption Model |
| `L` | Call LLM |
| `D` | Dynamic ABAP Code |
| `M` | ML Model Inference |
| `U` | User Tool |

---

## 12. Step 11 — Run Your Agent

Once the agent and tools are defined, you can execute it from any ABAP code:

```abap
" ── Run agent from ABAP ──────────────────────────
DATA(lo_agent) = NEW zpru_cl_unit_agent( ).

lo_agent->execute_agent(
    iv_agent_name  = 'MY_FIRST_AGENT'    " Must match ZPRU_AGENT-AGENTNAME
    is_prompt      = VALUE #( string_content = `Your input prompt here` ) ).
```

---

## 13. Complete File Structure

Your agent in the abapGit repo should look like this:

```
zpru_demo_your_agent/                  " Package folder
├── package.devc.xml                   " Package definition
├── zpru_if_your_agent.intf.abap       " Interface (types, constants)
├── zpru_if_your_agent.intf.xml        " Interface metadata
├── zpru_cl_your_agent.clas.abap       " Main facade class
├── zpru_cl_your_agent.clas.locals_def.abap   " Local class definitions
├── zpru_cl_your_agent.clas.locals_imp.abap   " Local class implementations
├── zpru_cl_your_agent.clas.xml        " Class metadata
└── zpru_demo_your_tech/               " Technical package (optional)
    ├── package.devc.xml
    └── zpru_cl_your_agent_test_data.clas.abap  " Test data creation
    └── zpru_cl_your_agent_test_data.clas.xml
```

---

## 14. Appendix — Template Reference

### Complete Facade Method Signatures (Generated Section)

The facade class must implement all these methods for **short memory** and **long memory**. Use the delegation pattern shown in Step 2.

**Short Memory Methods:**
- `clear_history`
- `get_discard_strategy`
- `get_history`
- `get_long_memory`
- `get_mem_volume`
- `save_message`
- `set_discard_strategy`
- `set_long_memory`
- `set_mem_volume`
- `flush_memory`

**Long Memory Methods:**
- `get_msg_persistence`
- `get_summarization`
- `get_sum_persistence`
- `retrieve_message`
- `retrieve_summary`
- `save_messages`
- `save_summary`
- `set_msg_persistence`
- `set_summarization`
- `set_sum_persistence`
- `summarize_conversation`

**Additional Provider Methods:**
- `get_abap_agent_info` (Agent Info)
- `get_abap_system_prompt` (System Prompt)
- `get_abap_tool_info` (Tool Info)

### Quick Start Checklist

- [ ] 1. Create interface with types, constants, tool names, context fields
- [ ] 2. Create facade class implementing all interfaces, delegating to local classes
- [ ] 3. Create `locals_def` — define all local classes (decision, memory, tools, providers)
- [ ] 4. Create `locals_imp` — implement all local classes
- [ ] 5. Create agent type in `ZPRU_AGENT_TYPE`
- [ ] 6. Create agent definition in `ZPRU_AGENT`
- [ ] 7. Create tool entries in `ZPRU_AGENT_TOOL`
- [ ] 8. Run the agent and verify!

---

> **Pro Tip:** Use the Visual Recognition Agent (`ZPRU_CL_COMPUTER_VISION`) as your living reference implementation. It demonstrates every pattern described in this guide with real-world complexity.
