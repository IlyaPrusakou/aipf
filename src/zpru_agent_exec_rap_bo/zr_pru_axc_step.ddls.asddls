@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Agent Execution Step'
@Metadata.ignorePropagatedAnnotations: true
define view entity ZR_PRU_AXC_STEP
  as select from ZI_PRU_AXC_STEP
    association         to parent ZR_PRU_AXC_QUERY   as _executionquery  on $projection.AIPF7QueryUuid = _executionquery.AIPF7QueryUuid
    association of many to exact one ZR_PRU_AXC_HEAD as _executionheader on $projection.AIPF7RunUuid = _executionheader.AIPF7RunUUID
{
  key  AIPF7StepUuid           as AIPF7StepUuid,
       AIPF7StepNumber         as AIPF7StepNumber,
       AIPF7QueryUuid          as AIPF7QueryUuid,
       AIPF7RunUuid            as AIPF7RunUuid,
       AIPF7ToolUuid           as AIPF7ToolUuid,
       AIPF7StepSequence       as AIPF7StepSequence,
       AIPF7StepStatus         as AIPF7StepStatus,
       AIPF7StepStartDateTime  as AIPF7StepStartDateTime,
       AIPF7StepEndDateTime    as AIPF7StepEndDateTime,
       AIPF7StepInputPrompt    as AIPF7StepInputPrompt,
       AIPF7StepOutputResponse as AIPF7StepOutputResponse,
              _executionquery,
              _executionheader
}
