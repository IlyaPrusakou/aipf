@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Agent Execution Query'
@Metadata.ignorePropagatedAnnotations: true
define view entity ZR_PRU_AXC_QUERY
  as select from ZI_PRU_AXC_QUERY
    association              to parent ZR_PRU_AXC_HEAD as _executionheader on $projection. AIPF7RunUuid = _executionheader. AIPF7RunUUID
    composition of exact one to many ZR_PRU_AXC_STEP   as _executionstep
{
  key AIPF7QueryUuid           as AIPF7QueryUuid,
      AIPF7QueryNumber         as AIPF7QueryNumber,
      AIPF7RunUuid             as AIPF7RunUuid,
      AIPF7QueryLanguage       as AIPF7QueryLanguage,
      AIPF7QueryStatus         as AIPF7QueryStatus,
      AIPF7QueryStartDateTime  as AIPF7QueryStartDateTime,
      AIPF7QueryEndDateTime    as AIPF7QueryEndDateTime,
      AIPF7QueryInputPrompt    as AIPF7QueryInputPrompt,
      AIPF7QueryDecisionLog    as AIPF7QueryDecisionLog,
      AIPF7QueryOutputResponse as AIPF7QueryOutputResponse,
            _executionheader,
            _executionstep
}
