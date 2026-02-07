@AccessControl.authorizationCheck: #MANDATORY
@Metadata.allowExtensions: true
@ObjectModel.sapObjectNodeType.name: 'ZPRU_MEM_SUM'
@EndUserText.label: '###GENERATED Core Data Service Entity'
define root view entity ZR_PRU_MEM_SUM
  as select from ZPRU_MEM_SUM
{
  key summary_uuid as SummaryUUID,
  content as Content,
  summary_cid as SummaryCid,
  stage as Stage,
  sub_stage as SubStage,
  namespace as Namespace,
  user_name as UserName,
  agent_uuid as AgentUUID,
  run_uuid as RunUUID,
  query_uuid as QueryUUID,
  step_uuid as StepUUID,
  message_time as MessageTime,
  created_by as CreatedBy,
  created_at as CreatedAt,
  changed_by as ChangedBy,
  changed_at as ChangedAt
}
