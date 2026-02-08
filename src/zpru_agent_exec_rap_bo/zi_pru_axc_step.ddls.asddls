@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Execution Step Basic'
@Metadata.ignorePropagatedAnnotations: true
@VDM.viewType: #BASIC
define view entity ZI_PRU_AXC_STEP
  as select from zpru_axc_step
  association to one ZI_PRU_AXC_QUERY as _executionquery  on _executionquery.AIPF7QueryUuid = $projection.AIPF7QueryUuid
  association to one ZI_PRU_AXC_HEAD  as _executionheader on _executionheader.AIPF7RunUuid = $projection.AIPF7RunUuid
{
  key stepuuid       as AIPF7StepUuid,
      stepnumber     as AIPF7StepNumber,
      queryuuid      as AIPF7QueryUuid,
      runuuid        as AIPF7RunUuid,
      tooluuid       as AIPF7ToolUuid,
      executionseq   as AIPF7StepSequence,
      stepstatus     as AIPF7StepStatus,
      starttimestamp as AIPF7StepStartDateTime,
      endtimestamp   as AIPF7StepEndDateTime,
      inputprompt    as AIPF7StepInputPrompt,
      outputprompt   as AIPF7StepOutputResponse,
      _executionquery,
      _executionheader

}
