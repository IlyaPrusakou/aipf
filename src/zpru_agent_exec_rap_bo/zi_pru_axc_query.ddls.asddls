@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Execution Query Basic'
@Metadata.ignorePropagatedAnnotations: true
@VDM.viewType: #BASIC
define view entity ZI_PRU_AXC_QUERY
  as select from zpru_axc_query
  association to many ZI_PRU_AXC_STEP as _executionstep   on _executionstep.QueryUuid = $projection.QueryUuid
  association to one ZI_PRU_AXC_HEAD  as _executionheader on _executionheader.RunUuid = $projection.RunUuid
{
  key query_uuid       as QueryUuid,
      query_number     as QueryNumber,
      run_uuid         as RunUuid,
      language         as Language,
      execution_status as ExecutionStatus,
      start_timestamp  as StartTimestamp,
      end_timestamp    as EndTimestamp,
      input_prompt     as InputPrompt,
      decision_log     as DecisionLog,
      output_response  as OutputResponse,
      _executionstep,
      _executionheader
}
