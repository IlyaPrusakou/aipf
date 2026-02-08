@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Agent Tool'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define view entity ZC_PRU_AGENT_TOOL
  as projection on ZR_PRU_AGENT_TOOL
{
  key AIPF7ToolUuid,
      AIPF7AgentUuid,
      AIPF7ToolName,
      AIPF7ToolProvider,
      AIPF7StepType,
      AIPF7ToolSchemaProvider,
      AIPF7ToolInfoProvider,
      AIPF7ToolIsBorrowed, 
      AIPF7ToolIsTransient
      ,
      /* Associations */
      _agent : redirected to parent ZC_PRU_AGENT
}
