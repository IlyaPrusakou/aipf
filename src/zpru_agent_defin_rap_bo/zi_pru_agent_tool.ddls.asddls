@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Agent Tool Basic'
@Metadata.ignorePropagatedAnnotations: true
@VDM.viewType: #BASIC
define view entity ZI_PRU_AGENT_TOOL
  as select from zpru_agent_tool
  association to one ZI_PRU_AGENT as _agent on _agent.AIPF7agentuuid = $projection.AIPF7AgentUuid
{
  key tooluuid            as AIPF7ToolUuid,
      agentuuid           as AIPF7AgentUuid,
      toolname            as AIPF7ToolName,
      toolprovider        as AIPF7ToolProvider,
      steptype            as AIPF7StepType,
      toolschemaprovider  as AIPF7ToolSchemaProvider,
      toolinfoprovider    as AIPF7ToolInfoProvider,
      ToolIsBorrowed          as AIPF7ToolIsBorrowed,
      ToolIsTransient         as AIPF7ToolIsTransient
      ,
      _agent
}
