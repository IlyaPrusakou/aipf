CLASS zpru_todo_list DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS todo.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zpru_todo_list IMPLEMENTATION.
  METHOD todo.
*21.12.2025
*1 ADD MESSAGED TO API -DONE
*2 IMPLEMENT METHOD ADD_QUERY_2_RUN IN API CLASS - done
*3 ADD ADDITIONAL FIELDS TO AGENT AND RUN TABLES - done
*4 EXPAND MESSAGES IN API WITH NEW FIELDS - DONE
*5 ADD VALIDATION INTO SERVICE CLASSES FOR AGENT AND RUN
*6 CHECK SAVE LOGIN IN SERVICE CLASSES FOR AGENT AND RUN

*23.12.2025
*1 add fields and logic to generate run_id, query_number, step_number to api - done
*2 save final response as query output - done
*3 adjust messages with new fields - DONE
*4 add field agent_type to agent node and customizing table with values and configs for agent type
*5 add strategy for summarize and long memory process
*6 save data as xstring in db ?????

*27.12.2025
*1 WHAT DO WITH ATTRIBUTES IN API CLASS???
*2 what do with error flags in long memory save methods and in io_long_memory->save_summary - fixed
*3 add possibility to provide configuration outside of agent definition
*4 add paralel save flag and logic in long memory save methods - refused

*1 do we need user defined tool when we have simple abap code
*2 add configuration for strategy( additional table and field in step definition )
*possible values
*RETRY_LIMIT INT1        INT1    3   Max Retries for Handler
*REQ_USER_APP    ABAP_BOOL       CHAR    1   Pause for User Approval
*COMMIT_REQ  ABAP_BOOL       CHAR    1   Trigger COMMIT WORK
*IS_IDEMPOTENT   ABAP_BOOL       CHAR    1   Safe for Auto-Retry
*TERM_ON_FAIL    ABAP_BOOL       CHAR    1   Terminate Process on Error

*3 make framework system prompt
*4 again recheck what formats to use json or string
*5 finish with dynamic invokation

*delete objects:
*ZPRU_CL_ADF_BUFFER
*ZPRU_CL_ADF_FACTORY
*ZPRU_CL_ADF_PRECHECK - WE WILL USE IN RAP BO
*ZPRU_IF_ADF_FACTORY
*ZPRU_IF_ADF_PRECHECK - WE WILL USE IN RAP BO
*ZPRU_CL_AXC_BUFFER
*ZPRU_CL_AXC_FACTORY
*ZPRU_CL_AXC_PRECHECK

*wrap raise exception into method

*IMPLEMENT INPUT SCHEMA PROVIDER AND TOOL INFO PROVIDER IN FRAMEWORK AND IN EXAMPLE

*cleanup dummy agent from technical methods like get_input_prompt and readme.

*rework idea of input and output in a way that we have short_memory + long_memory + context(will gather during run) + small input + small output
* rework dynamic tool execution according to new output and input approach
* the same as above for example

*DURING MINI LOOP WE CAN GET TOOLS IN EXECUTION WHICH HAS NO DEFINITION. TWO CASES( 1 BORROWED FROM ANOTHER DEFINITION 2 WITHOUT DEFINITION)

* rename fields in db and structures without underscore
* create structures for db tables - done
* remove usage of data base table
* create RAP service classes for all tables

* add field to tool def table is_borrowed, is_transient - DONE
* add field to query table decision_log - DONE
* add field to step table step_status - DONE

* use agent type lopp_numb to control number of miniloops

* make conversion of my message class into rap message class

* isolate zpru_cl_test_data as it is used data base tables

* add new parameters into minifyed implmentation for retreive messages

* add service for class ZPRU_DISC_STRAT and for ZPRU_SUMM_STRAT

* add rap bo and service for for ZPRU_DYN_LIST + ZPRU_DYN_LIST_PR

* nested agent will return context and we will attach it to parent agent

* rework input parameter as structure

* make normal numbering for execution business object and delete usage of buffer from generate id method

*add messages into code

*rename field executionseq normally in step table

*rename paramname, paramtype and paramorder in table for dynamic tool normally

* ADD BASIC CLASS FOR DECISION PROVIDER
* ADD STRUCTURE FOR DECISION LOG - LOOK AT EXAMPLE IMPLEMENTATION

* check how you use messages wher json and where is string

* add service for zpru_semant_mem - semantic memory and zpru_semmem_rlnp - semantic relationships
* add service for  - procedural memory

* add basic classes for agent info, agent syst prompt(reuse class), tool info, tool schema and tool provider


  ENDMETHOD.

ENDCLASS.
