@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Agent Execution Query'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define view entity ZC_PRU_AXC_QUERY
  as projection on ZR_PRU_AXC_QUERY
{
  key AIPF7QueryUuid,
      AIPF7QueryNumber,
      AIPF7RunUuid,
      AIPF7QueryLanguage,
      AIPF7QueryStatus,
      AIPF7QueryStartDateTime,
      AIPF7QueryEndDateTime,
      AIPF7QueryInputPrompt,
      AIPF7QueryDecisionLog,
      AIPF7QueryOutputResponse,
      /* Associations */
      _executionheader : redirected to parent ZC_PRU_AXC_HEAD,
      _executionstep   : redirected to composition child ZC_PRU_AXC_STEP
}
