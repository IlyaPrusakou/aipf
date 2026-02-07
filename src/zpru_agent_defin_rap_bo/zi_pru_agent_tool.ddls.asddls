@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Agent Tool Basic'
@Metadata.ignorePropagatedAnnotations: true
@VDM.viewType: #BASIC
define view entity ZI_PRU_AGENT_TOOL
  as select from zpru_agent_tool
  association to one ZI_PRU_AGENT as _agent on _agent.AgentUuid = $projection.AgentUuid
{
  key tool_uuid            as ToolUuid,
      agent_uuid           as AgentUuid,
      tool_name            as ToolName,
      tool_provider        as ToolProvider,
      step_type            as StepType,
      tool_schema_provider as ToolSchemaProvider,
      tool_info_provider   as ToolInfoProvider,
      is_borrowed          as IsBorrowed,
      is_transient         as IsTransient,
      _agent
}
