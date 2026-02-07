@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Agent Execution Step'
@Metadata.ignorePropagatedAnnotations: true
define view entity ZR_PRU_AXC_STEP
  as select from ZI_PRU_AXC_STEP
  association         to parent ZR_PRU_AXC_QUERY   as _executionquery  on $projection.QueryUuid = _executionquery.QueryUuid
  association of many to exact one ZR_PRU_AXC_HEAD as _executionheader on $projection.RunUuid = _executionheader.RunUUID
{
  key StepUuid       as StepUuid,
      StepNumber     as StepNumber,
      QueryUuid      as QueryUuid,
      RunUuid        as RunUuid,
      ToolUuid       as ToolUuid,
      ExecutionSeq   as ExecutionSeq,
      StepStatus     as StepStatus,
      StartTimestamp as StartTimestamp,
      EndTimestamp   as EndTimestamp,
      InputPrompt    as InputPrompt,
      OutputPrompt   as OutputPrompt,
      _executionquery,
      _executionheader
}
