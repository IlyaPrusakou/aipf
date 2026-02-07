@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Agent Tool'
@Metadata.ignorePropagatedAnnotations: true
@VDM.viewType: #BASIC
define view entity ZR_PRU_AGENT_TOOL
  as select from ZI_PRU_AGENT_TOOL
  association to parent ZR_PRU_AGENT as _agent on $projection.AgentUuid = _agent.AgentUUID
{
  key ToolUuid             as ToolUuid,
      AgentUuid            as AgentUuid,
      ToolName             as ToolName,
      ToolProvider         as ToolProvider,
      StepType             as StepType,
      ToolSchemaProvider as ToolSchemaProvider,
      ToolInfoProvider    as ToolInfoProvider,
      IsBorrowed   as IsBorrowed, 
      IsTransient  as IsTransient,
      _agent 
}
