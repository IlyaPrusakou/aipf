@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Execution Step Basic'
@Metadata.ignorePropagatedAnnotations: true
@VDM.viewType: #BASIC
define view entity ZI_PRU_AXC_STEP
  as select from zpru_axc_step
  association to one ZI_PRU_AXC_QUERY as _executionquery  on _executionquery.QueryUuid = $projection.QueryUuid
  association to one ZI_PRU_AXC_HEAD  as _executionheader on _executionheader.RunUuid = $projection.RunUuid
{
  key step_uuid       as StepUuid,
      step_number     as StepNumber,
      query_uuid      as QueryUuid,
      run_uuid        as RunUuid,
      tool_uuid       as ToolUuid,
      execution_seq   as ExecutionSeq,
      step_status     as StepStatus,
      start_timestamp as StartTimestamp,
      end_timestamp   as EndTimestamp,
      input_prompt    as InputPrompt,
      output_prompt   as OutputPrompt,
      _executionquery,
      _executionheader

}
