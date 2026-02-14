# AIPF - Agent Integration Processing Framework

The name is inspired by famous **Business Object Processing Framework (BOPF)**

**So far AIPF is under heavy development!!!**

**Disclaimer**: SAP, ABAP, BTP, and BOPF are trademarks or registered trademarks of SAP SE in Germany and other countries. AIPF is an independent open-source project and is not affiliated with, sponsored by, or endorsed by SAP SE.

## Table of Contents
* [Long Story Short](#long-story-short)
* [Installation](#installation)
* [Agent Definition](#agent-definition)
    * [Decision Provider](#decision-provider)
    * [Execution Plan](#execution-plan)
    * [Tool](#tool)
    * [Supported Tools](#supported-tools)
    * [Tool Info Provider](#tool-info-provider)
    * [Tool Schema Provider](#tool-schema-provider)
* [Agent Type](#agent-type)
* [Agent Execution](#agent-execution)
    * [Execution Header](#execution-header)
    * [Execution Query](#execution-query)
    * [Execution Step](#execution-step)
    * [Miniloop](#miniloop)
* [Memory Management](#memory-management)
    * [Short Memory](#short-memory)
    * [Long Memory](#long-memory)
* [Developer Experience](#developer-experience)
    * [Core Interfaces](#core-interfaces)
* [Agent Categories](#agent-categories)
    * [IF-ELSE Agent](#if-else-agent)
    * [LLM Agent](#llm-agent)
    * [Decision Agent](#decision-agent)
* [Agent Composition](#agent-composition)
* [How to implement your first agent](#how-to-implement-your-first-agent)
    * [Implement Decision Provider](#implement-decision-provider)
    * [Implement Agent Info Provider](#implement-agent-info-provider)
    * [Implement System Prompt Provider](#implement-system-prompt-provider)
    * [Short Memory Provider](#short-memory-provider)
    * [Long Memory Provider](#long-memory-provider)
    * [Create entry in database table ZPRU_AGENT](#create-entry-in-database-table-zpru_agent)
    * [Implement your first tool - ABAP code tool](#implement-your-first-tool---abap-code-tool)
    * [Implement Tool Info Provider](#implement-tool-info-provider-1)
    * [Implement Tool Schema Provider](#implement-tool-schema-provider-1)
    * [Create entry in database table ZPRU_AGENT_TOOL](#create-entry-in-database-table-zpru_agent_tool)
    * [How to run Agent from your abap code](#how-to-run-agent-from-your-abap-code)

## Long Story Short
Standard AI calls in ABAP are stateless—they send a prompt and get a response. AIPF adds the Brain and Muscles:
**Decision Platform:** ABAP-based Decision Engine, ABAP-based Short-Term Memory Management, ABAP-based Long-Term Memory Framework, etc.
**Actionables:** Various ABAP-based Tools, etc.
The framework requires writing a significant amount of ABAP code. It is designed for developers and implies that you are proficient in writing ABAP code and have a solid understanding of concepts like APIs, HTTP, and integration patterns. You must be capable of integrating different types of APIs, such as LLMs, Machine Learning models, and various external services. Moreover, you must know how to execute these integrations within the constraints and architecture of the SAP landscape.

Core idea expressed in pseudo ABAP:

```abap
" select tool metadata from agent database table
" feed tools metadata to LLM to get execution plan
LOOP AT lt_execution_plan ASSIGNING TO FIELD-SYMBOL(<ls_execution_plan>).
  ASSIGN lt_agent_tool[ tool_name = <ls_execution_plan>-tool_name ] TO FIELD-SYMBOL(<ls_agent_tool>).
  """"
  """"
  CREATE OBJECT lo_tool TYPE (<ls_agent_tool>-classname).
  """"
  """"
  lo_tool->execute_tool( EXPORTING is_input = ls_input
                         IMPORTING es_output = ls_output ).
  """"
  """"
ENDLOOP.
" feed LLM last output to get final response
" return final response to consumer
```




## Installation
1. Install [abapGit](https://abapgit.org/).
2. Create a new Online Repo with the URL: `https://github.com/IlyaPrusakou/aipf`
3. Pull the objects into the system.

## Agent Definition

Agent Definition is a combination of data base tables, containing names of main ABAP classes; table for Agent and table for Agent Tools.
Agent table contains names for ABAP classes: Decision Provider, Short Memory Provider, Long Memory Provider, Agent Info Provider, System Prompr Provider.
Agent Tool contains names for ABAP classes: Tool Provider, Tools Schema Provider and Tool Info Provider

### Decision Provider

Decision Provider is an ABAP class. The framework does not contain a generic implementation. The developer is responsible for writing ABAP code to provide the decision logic.
Decision logic is a broad concept. It may involve a call to an LLM, hard-coded ABAP logic, or the invocation of a Machine Learning API. Creating a Decision Provider means decomposing domain logic into manageable questions. Some of these questions can be decided by ABAP, while others can be decided by an LLM, etc. The Decision Provider must implement the interface ZPRU_IF_DECISION_PROVIDER. The output of the Decision Provider's work is an Execution Plan.
For each agent, the developer must insert an entry into table ZPRU_AGENT containing the name of the ABAP class for the Decision Provider.

### Execution Plan

The Execution Plan is a sequence of steps to be executed. Each step is a tool and an ABAP class that receives a string as input and provides a string as output. The next step takes the output of the previous step as its new input, and so on.

### Tool

A Tool is an ABAP class providing a piece of executable ABAP code. Additionally, a Tool contains metadata expanding and clarifying the tool's role. The Tool itself must implement the interface ZPRU_IF_TOOL_EXECUTOR and a specific tool interface, e.g., ZPRU_IF_ABAP_EXECUTOR. Alternatively, you can inherit from a specific base class, e.g., ZPRU_CL_ABAP_EXECUTOR. Each specific class contains an abstract method where the developer must provide the ABAP code.

### Supported Tools

| # | Tool Type | Description |
| :--- | :--- | :--- |
| 1 | ABAP Code | You write an ABAP class to be executed by the AIPF framework. |
| 2 | Knowledge Source | You write an ABAP class which returns any piece of data to be processed. |
| 3 | Nested Agent | You write an ABAP class to invoke other agents as a tool for your current agent. |
| 4 | HTTP Request | You write an ABAP class sending an HTTP request to any source and returning the payload to the agent processing loop. |
| 5 | Service Consumption Model | Basically the same as the HTTP tool, but using another request technique. |
| 6 | Call LLM | You write an ABAP class sending a prompt to an LLM and returning its response. |
| 7 | Dynamic ABAP Code | The tool has a default implementation which just dynamically invokes a method which you saved into database tables `ZPRU_DYN_LIST` and `ZPRU_DYN_LIST_PR`. |
| 8 | Inference Machine Learning Model | You write an ABAP class which calls a Machine Learning API. |
| 9 | User Tool | You write an ABAP class where you can invoke screens if you work in on-premise or private cloud systems to provide the Human-In-The-Loop pattern. |

### Tool Info Provider

Tool Info Provider is an ABAP class implementing the interface ZPRU_IF_TOOL_INFO_PROVIDER and returning tool metadata as a plain string.

### Tool Schema Provider

Tool Schema Provider is ABAP class returning input and output schema. It support two formats: JSON and ABAP RTTS types. 

## Agent Type


## Agent Execution

While Agent Definition contains project of the Agent, Agent Execution will use these data to create and execute agent. It reads names of ABAP classes from agent definition from database and create and perform Agent Execution. Technically, Agent Execution will saved in data base tables for Execution Header, Execution Query and Execution Steps.

### Execution Header

Execution Header is root for execution and has unique UUID. It can have multiple execution queries. 

### Execution Query

Execution Query is combination initial input prompt, provided by framework consumer, and final response, provided by agent framework after execution all steps from the execution plan. It recieves unique UUID and attach to certain Execution Header. It can have multiple execution steps.

### Execution Step

Execution Step contains input prompt to step and output of execution of ABAP Tool Provider class. Apart of output itself the Execution Step can return list of additional steps for execution to initiate Miniloop.
It recieves unique UUID and attach to certain Execution Query.

### Miniloop

Miniloop is a dynamic feature that enables the creation and execution of sub-steps within an existing execution plan.
Technically, a developer simply populates an exporting parameter during tool execution with the new steps required. The framework then intercepts these additional steps and injects them into the workflow - executing them immediately after the current step and before the next scheduled step in the initial plan.

## Memory Management

Memory Management comprises ABAP classes, processing Short Memory, Long Memory, Memory Summirization and Memory Discard.

### Short Memory

Short Memory is an ABAP class handling messages during execution particular Execution Query. It has basic implementation and developer must attach certain ABAP class (basic, custom or inherited from basic) to each Agent Definition.  

### Long Memory

Long Memory is an ABAP class handling different strategies for saving messages into database table. It has basic implementation and developer must attach certain ABAP class (basic, custom or inherited from basic) to each Agent Definition. Save strategies are persisting raw messages or persisting summarized messages in database table.  

### Discard Strategy

### Summarize Strategy

### Technical Features

#### Adapter Service Framework

#### ABAP Cloud Language Version

#### Base Implementation

### Developer Experience

AIPF is built specifically **for developers**. It does not provide a "generic" out-of-the-box agent; instead, it serves as a robust platform where you provide the implementation logic via framework-defined interfaces. 

The development experience is intentionally designed to feel similar to writing an **unmanaged RAP implementation**, where you have full control over the business logic while the framework handles the orchestration and persistence.

#### Core Interfaces
To build your agent, you implement the following interfaces:
* **Planning Logic:** Implement `ZPRU_IF_DECISION_PROVIDER` to define how the agent reasons and selects steps.
* **Tool Definition:** Implement `ZPRU_IF_TOOL_PROVIDER` to describe the tool's capabilities.
* **Tool Execution:** Implement `ZPRU_IF_TOOL_EXECUTOR` to write the actual ABAP code the agent will trigger.

### Agent Categories

AIPF is designed to be model-agnostic. You can choose the "Intelligence Level" of your agent based on the complexity of the business task:

| Agent Type | Planning Logic / Engine | Description |
| :--- | :--- | :--- |
| **IF-ELSE Agent** | Plain ABAP, Condition Technique, or **BRF+** | Planning logic written in plain ABAP or using standard frameworks. |
| **ML Agent** | **SAP AI Core** (Machine Learning) | Planning logic made using ML models or multiple models deployed on SAP AI Core. |
| **LLM Agent** | **SAP AI Core** (Generative AI Hub) | Planning logic made using Large Language Models (LLMs) via SAP AI Core. |
| **Decision Agent** | Hybrid: **LLM + ML + ABAP (BRF+, etc.)** | Complex logic combining multiple calls of LLM, ML models, and ABAP frameworks. |

### Implementation Details

Basically, this differiantion are made based on how developer provides implementation for interface zpru_if_decision_provider~call_decision_engine 

#### IF-ELSE Agent

```abap
METHOD zpru_if_decision_provider~call_decision_engine.
  IF ls_input-strategy_name = `CREATE_INBOUND_DELIVERY`.
    APPEND INITIAL LINE TO et_execution_plan ASSIGNING FIELD-SYMBOL(<ls_execution_plan>).
    <ls_execution_plan>-toolname = `GET_PO_DETAILS`.
 
    APPEND INITIAL LINE TO et_execution_plan ASSIGNING FIELD-SYMBOL(<ls_execution_plan>).
    <ls_execution_plan>-toolname = `POST_INBOUND_DELIVERY`
  ELSEIF ls_input-strategy_name = `COMPLETE_WAREHOUSE_ORDER`.
    APPEND INITIAL LINE TO et_execution_plan ASSIGNING FIELD-SYMBOL(<ls_execution_plan>).
    <ls_execution_plan>-toolname = `CONFIRM_PICKING_WTS`.
 
    APPEND INITIAL LINE TO et_execution_plan ASSIGNING FIELD-SYMBOL(<ls_execution_plan>).
    <ls_execution_plan>-toolname = `SET_WO_STATUS_COMPLETE`.
  ELSE.
    APPEND INITIAL LINE TO et_execution_plan ASSIGNING FIELD-SYMBOL(<ls_execution_plan>).
    <ls_execution_plan>-toolname = `SEND_EMAIL_WRONG_STRATEGY`.
  ENDIF.
ENDMETHOD. 
```
#### LLM Agent

```abap
METHOD zpru_if_decision_provider~call_decision_engine.
  FINAL(lo_api) = cl_aic_islm_compl_api_factory=>get( )->create_instance( 'ST-GEMINI-3.0' ).
  FINAL(lv_response) = lo_api->execute_for_string( 'I got CMR N1234562KL. How to procced?' )->get_completion( ).

  /ui2/cl_json=>deserialize( EXPORTING json = lv_response
                             CHANGING  data = ls_parsed_response ).

  LOOP AT ls_parsed_response-content ASSIGNING TO FIELD-SYMBOL(<ls_candidate>).
    APPEND INITIAL LINE TO lt_execution_plan ASSIGNING FIELD-SYMBOL(<ls_execution_plan>).
    <ls_execution_plan>-toolname  = <ls_candidate>-toolname.
  ENDLOOP.
ENDMETHOD. 
```

#### Decision Agent
```abap
METHOD zpru_if_decision_provider~call_decision_engine.
  "Main Process: Decide if a received Handling Unit (HU) needs full inspection or immediate putaway.

  "First Question: Does the vendor's attached delivery note or damage description suggest a recurring quality issue?
  lo_llm_provider->call_llm( EXPORTING iv_input      = ls_delivery_note-raw_text
                             IMPORTING ev_risk_level = lv_risk_level ).
  IF lv_risk_level = `HIGH`.
    APPEND INITIAL LINE TO et_execution_plan ASSIGNING FIELD-SYMBOL(<ls_execution_plan>).
    <ls_execution_plan>-toolname = `PROVIDE_HUMAN_VISUAL_INSPECTION`.
  ENDIF.

  "Second Question: Based on the Product Category and Risk Level, what is the required Sampling Percentage?
  lo_bfr_provider->call_brf( EXPORTING iv_product_category = ls_delivery_note-product_category
                                       iv_risk_level       = lv_risk_level
                             IMPORTING ev_sample_size      = lv_sample_size ).
  IF lv_sample_size > 50.
    APPEND INITIAL LINE TO et_execution_plan ASSIGNING FIELD-SYMBOL(<ls_execution_plan>).
    <ls_execution_plan>-toolname = `CHECK_WORKFORCE_AVAILABILITY`.
  ENDIF.

  lo_ewm_api->read_work_center( EXPORTING iv_wc_name     = 'QLTY'
                                IMPORTING es_work_center = ls_work_center ).

  IF ls_work_center-workcenterisavailable = abap_true.
    APPEND INITIAL LINE TO et_execution_plan ASSIGNING FIELD-SYMBOL(<ls_execution_plan>).
    <ls_execution_plan>-toolname = `SEND_TO_WORK_CENTER`.
  ELSE.
    APPEND INITIAL LINE TO et_execution_plan ASSIGNING FIELD-SYMBOL(<ls_execution_plan>).
    <ls_execution_plan>-toolname = `IMMIDIATE_PUTAWAY`.
  ENDIF. 
ENDMETHOD.
```

* **Decision Agent:** Acts as a meta-orchestrator. It can use an LLM to "think," an ML model to "calculate," and BRF+ to "verify" against corporate policy before executing a BAPI.

### Agent Composition

* **Nested Agents:** An agent can be assigned as a tool to another agent. This allows for specialized "sub-agents" to handle specific domains (e.g., an HR Agent calling a Payroll Agent as a tool).
* **Composed Agents:** Multiple agents can be chained together to handle multi-stage workflows.
* **Reflexive Agents:** The simplest form of composition where the output of an agent is immediately fed back as the input for the next round of processing to refine or validate the result.

## How to implement your first agent

### Implement Decision Provider
Create class and implement interface ZPRU_IF_DECISION_PROVIDER
```abap
CLASS ZCL_DECISION_PROVIDER DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zpru_if_decision_provider.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS ZCL_DECISION_PROVIDER IMPLEMENTATION.  
  METHOD zpru_if_decision_provider~call_decision_engine.

  ENDMETHOD.

  METHOD zpru_if_decision_provider~prepare_final_response.

  ENDMETHOD.
ENDCLASS.
```
### Implement Agent Info Provider
Create class and implement interface ZPRU_IF_AGENT_INFO_PROVIDER
```abap
CLASS ZCL_AGENT_INFO_PROVIDER DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zpru_if_agent_info_provider.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS ZCL_AGENT_INFO_PROVIDER IMPLEMENTATION.
  METHOD zpru_if_agent_info_provider~get_agent_info.
    rv_agent_info = `It is my first agent and it does some stuff`.
  ENDMETHOD.
ENDCLASS.
```

### Implement System Prompt Provider
Create class and implement interface ZPRU_IF_PROMPT_PROVIDER
```abap
CLASS ZCL_SYST_PROMPT_PROVIDER DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zpru_if_prompt_provider.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS ZCL_AGENT_INFO_PROVIDER IMPLEMENTATION.
  METHOD zpru_if_prompt_provider~get_system_prompt.
    rv_system_prompt = `It is my system prompt. I am about to feed to LLM`.
  ENDMETHOD.
ENDCLASS.
```
### Short Memory Provider
Just use framework class ZPRU_CL_SHORT_MEMORY_BASE, but if you want to implement your own short memory management, you may inherit theis class or even create new class and implement interface ZPRU_IF_SHORT_MEMORY_PROVIDER. 

### Long Memory Provider
Just use framework class ZPRU_CL_LONG_MEMORY_BASE, but if you want to implement your own short memory management, you may inherit theis class or even create new class and implement interface ZPRU_IF_LONG_MEMORY_PROVIDER. 

### Create entry in database table ZPRU_AGENT
You can use preview app for RAP business object ZR_PRU_AGENT via service binding ZUI_PRU_AGENT_TYPE_O4
```abap
DATA ls_agent TYPE zpru_agent.
ls_agent-agentname            = `MY_FIRST_AGENT`.
ls_agent-decisionprovider     = `ZCL_DECISION_PROVIDER`.
ls_agent-shortmemoryprovider  = `ZPRU_CL_SHORT_MEMORY_BASE`.
ls_agent-longmemoryprovider   = `ZPRU_CL_LONG_MEMORY_BASE`.
ls_agent-agentinfoprovider    = `ZCL_AGENT_INFO_PROVIDER`.
ls_agent-systempromptprovider = `ZCL_SYST_PROMPT_PROVIDER`.
```

### Implement your first tool - ABAP code tool
Developer has two options. First is to create new class and implement common tool interface ZPRU_IF_TOOL_EXECUTOR and specific tool interface ZPRU_IF_ABAP_EXECUTOR. The second option is just inherit from basic class ZPRU_CL_ABAP_EXECUTOR and implement only abstract method EXECUTE_CODE_INT. In both variants developer must additionally implement interface ZPRU_IF_TOOL_PROVIDER to provide logic to instantiate tool.
For sure you can make separate class for interface ZPRU_IF_TOOL_PROVIDER if you need make some fancy instantiating logic. In our simple example we just return self reference ME.

#### Tool from scratch
```abap
CLASS ZCL_ABAP_CODE_TOOL1 DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zpru_if_tool_executor.
    INTERFACES zpru_if_abap_executor.
    INTERFACES zpru_if_tool_provider
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS ZCL_ABAP_CODE_TOOL1 IMPLEMENTATION.
  METHOD zpru_if_abap_executor~execute_code.
    DATA lv_tool_output TYPE string.
    lv_tool_output = `My tool is executed`.
  ENDMETHOD.

  METHOD zpru_if_tool_provider~get_tool.
    ro_executor = me.
  ENDMETHOD.
ENDCLASS.
```

#### Tool via inheriting
```abap
CLASS ZCL_ABAP_CODE_TOOL2 DEFINITION INHERITING FROM zpru_cl_abap_executor CREATE PUBLIC.
  PUBLIC SECTION.
  INTERFACES zpru_if_tool_provider.
  PROTECTED SECTION.
   METHODS execute_code_int REDEFINITION.
  PRIVATE SECTION.
ENDCLASS.

CLASS ZCL_ABAP_CODE_TOOL2 IMPLEMENTATION.
  METHOD  execute_code_int.
    DATA lv_tool_output TYPE string.
    lv_tool_output = `My tool is executed`.
  ENDMETHOD.

  METHOD zpru_if_tool_provider~get_tool.
    ro_executor = me.
  ENDMETHOD.
ENDCLASS.
```

### Implement Tool Info Provider
Create ABAP class and implement interface ZPRU_IF_TOOL_INFO_PROVIDER.
```abap
CLASS ZCL_TOOL_INFO DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
  INTERFACES zpru_if_tool_info_provider.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS ZCL_TOOL_INFO IMPLEMENTATION.
  METHOD zpru_if_tool_info_provider~get_tool_info.
    rv_tool_info = `some tool metadata`.
  ENDMETHOD.
ENDCLASS.
```

### Implement Tool Schema Provider
Create ABAP class and implement interface ZPRU_IF_TOOL_SCHEMA_PROVIDER
```abap
CLASS ZCL_TOOL_SCHEMA DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
  INTERFACES zpru_if_tool_schema_provider.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS ZCL_TOOL_SCHEMA IMPLEMENTATION.
  METHOD zpru_if_tool_schema_provider~input_rtts_schema.
    DATA ls_input_struct TYPE zs_some_input.
    ro_structure_schema ?= cl_abap_typedescr=>describe_by_data( ls_input_struct ).
  ENDMETHOD.

  METHOD zpru_if_tool_schema_provider~output_rtts_schema.
    DATA ls_output_struct TYPE zs_some_output.
    ro_structure_schema ?= cl_abap_typedescr=>describe_by_data( ls_output_struct ).
  ENDMETHOD.
ENDCLASS.
```

### Create entry in database table ZPRU_AGENT_TOOL
You can use preview app for RAP business object ZR_PRU_AGENT via service binding ZUI_PRU_AGENT_TYPE_O4
```abap
DATA ls_agent_tool TYPE zpru_agent_tool.
ls_agent_tool-toolname           = `My first Tool`.
ls_agent_tool-toolprovider       = `ZCL_ABAP_CODE_TOOL1`. " or ZCL_ABAP_CODE_TOOL2 or separate class implementing interface ZPRU_IF_TOOL_PROVIDER
ls_agent_tool-toolschemaprovider = `ZCL_TOOL_SCHEMA`.
ls_agent_tool-toolinfoprovider   = `ZCL_TOOL_INFO`.
```

### How to run Agent from your abap code
```abap
    DATA lo_cl_unit_agent TYPE REF TO zpru_if_unit_agent.

    lo_cl_unit_agent = NEW zpru_cl_unit_agent( ).
    lo_cl_unit_agent->execute_agent( iv_agent_name  = 'MY_FIRST_AGENT'
                                     is_prompt = VALUE #( string_content = `Process CMR N1231411KL` ) ).
```
