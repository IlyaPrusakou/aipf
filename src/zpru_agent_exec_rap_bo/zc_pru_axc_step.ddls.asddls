@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Agent Execution Step'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define view entity ZC_PRU_AXC_STEP
  as projection on ZR_PRU_AXC_STEP
{
  key AIPF7StepUuid,
      AIPF7StepNumber,
      AIPF7QueryUuid,
      AIPF7RunUuid,
      AIPF7ToolUuid,
      _tool.AIPF7ToolName as AIPF7ToolName,
      AIPF7StepSequence,
      AIPF7StepStatus,
      AIPF7StepStartDateTime,
      AIPF7StepEndDateTime,
      AIPF7StepInputPrompt,
      AIPF7StepOutputResponse,

      /* Associations */
      _executionheader : redirected to ZC_PRU_AXC_HEAD,
      _executionquery  : redirected to parent ZC_PRU_AXC_QUERY,
      _tool            : redirected to ZC_PRU_AGENT_TOOL

}
