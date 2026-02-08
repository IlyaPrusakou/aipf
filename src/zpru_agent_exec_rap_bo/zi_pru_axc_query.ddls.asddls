@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Execution Query Basic'
@Metadata.ignorePropagatedAnnotations: true
@VDM.viewType: #BASIC
define view entity ZI_PRU_AXC_QUERY
  as select from zpru_axc_query
  association to many ZI_PRU_AXC_STEP as _executionstep   on _executionstep.AIPF7QueryUuid = $projection.AIPF7QueryUuid
  association to one ZI_PRU_AXC_HEAD  as _executionheader on _executionheader.AIPF7RunUuid = $projection.AIPF7RunUuid
{
  key queryuuid       as AIPF7QueryUuid,
      querynumber     as AIPF7QueryNumber,
      runuuid         as AIPF7RunUuid,
      language        as AIPF7QueryLanguage,
      executionstatus as AIPF7QueryStatus,
      starttimestamp  as AIPF7QueryStartDateTime,
      endtimestamp    as AIPF7QueryEndDateTime,
      inputprompt     as AIPF7QueryInputPrompt,
      decisionlog     as AIPF7QueryDecisionLog,
      outputresponse  as AIPF7QueryOutputResponse,
      _executionstep,
      _executionheader
}
