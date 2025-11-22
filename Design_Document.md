ABAP Agent Framework

Main definitions:

AGENT DEFINITION - static container for agent and its tools, technical foundation for agent execution. One agent definition can have many executions.

AGENT - root entity defining decision provider, short/long memory providers, ancor for assignment of tools. Represented as row in database table ZPRU_AGENT.

TOOL - child entity containing name of tool class. Represented as a row in database table ZPRU_AGENT_TOOL

TOOL CLASS - ABAP class representing a peace of work to be executed by agent. There are assigned tools and equiped tools.

ASSIGNED TOOL - tool assigned to agent during definition phase and persisted in table ZPRU_AGENT_TOOL.

EQUIPED TOOL - tool added to agent execution ad hoc via API method during building execution from agent definition. [NOT YET IMPLEMENTED]

DECISION PROVIDER - ABAP class contained in agent definition which is using any decision platform(LLM, ML, Condition technique API etc. or combination of them) and produces invocation sequence of tools. 

SHORT MEMORY PROVIDER - ABAP class saving query, input and output messages and final response during one execution run

LONG MEMORY PROVIDER - ABAP class persists in database table all data during one or more execution runs

INFO PROVIDER - ABAP class for getting info about agent, tool etc.

Framework stucture
1 ZPRU_AGENT_FRAMEWORK
1.1 ZPRU_AGENT_API
1.2 ZPRU_AGENT_CORE
1.3 ZPRU_AGENT_DEFIN_RAP_BO
1.4 ZPRU_AGENT_DEFINITION
1.5 ZPRU_AGENT_EXAMPLE
1.6 ZPRU_AGENT_EXEC_RAP_BO
1.7 ZPRU_AGENT_EXECUTION

User story:
User can define agent and tools for them.
User can use high level API to prepare agent instance for execution.
User can save into persistance storage prepared execution to use it later
User can run execution to get result of agent work
User can rerun execution

Main packages
Package ZPRU_AGENT_DEFINITION contains tables and classes relevant to agent definition.


Main database tables:
1 ZPRU_AGENT contains definition of agent, header table for tools table ZPRU_AGENT_TOOL with relation one to many.
2 ZPRU_AGENT_TOOL contains assignments of tools to the agent



Main interfaces:



Main classes:
