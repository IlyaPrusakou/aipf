@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Agent Execution Query'
@Metadata.ignorePropagatedAnnotations: true
define view entity ZR_PRU_AXC_QUERY
  as select from ZI_PRU_AXC_QUERY
  association              to parent ZR_PRU_AXC_HEAD as _executionheader on $projection.RunUuid = _executionheader.RunUUID
  composition of exact one to many ZR_PRU_AXC_STEP   as _executionstep
{
  key QueryUuid       as QueryUuid,
      QueryNumber     as QueryNumber,
      RunUuid         as RunUuid,
      Language        as Language,
      ExecutionStatus as ExecutionStatus,
      StartTimestamp  as StartTimestamp,
      EndTimestamp    as EndTimestamp,
      InputPrompt     as InputPrompt,
      DecisionLog     as DecisionLog,
      OutputResponse  as OutputResponse,
      _executionheader,
      _executionstep
}
