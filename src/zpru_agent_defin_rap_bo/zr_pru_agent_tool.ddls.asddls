@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Agent Tool'
@Metadata.ignorePropagatedAnnotations: true
@VDM.viewType: #BASIC
define view entity ZR_PRU_AGENT_TOOL
  as select from ZI_PRU_AGENT_TOOL
  association to parent ZR_PRU_AGENT as _agent on $projection.AIPF7AgentUuid = _agent.AIPF7AgentUUID
{
  key AIPF7ToolUuid             as AIPF7ToolUuid,
      AIPF7AgentUuid            as AIPF7AgentUuid,
      AIPF7ToolName             as AIPF7ToolName,
      AIPF7ToolProvider         as AIPF7ToolProvider,
      AIPF7StepType             as AIPF7StepType,
      AIPF7ToolSchemaProvider   as AIPF7ToolSchemaProvider,
      AIPF7ToolInfoProvider     as AIPF7ToolInfoProvider,
      AIPF7ToolIsBorrowed       as AIPF7ToolIsBorrowed, 
      AIPF7ToolIsTransient      as AIPF7ToolIsTransient
      ,
      _agent 
}
