@AccessControl.authorizationCheck: #MANDATORY
@Metadata.allowExtensions: true
@ObjectModel.sapObjectNodeType.name: 'ZPRU_MEM_SUM'
@EndUserText.label: '###GENERATED Core Data Service Entity'
define root view entity ZR_PRU_MEM_SUM
  as select from zpru_mem_sum
{
  key summaryuuid as SummaryUUID,
  content as Content,
  summarycid as SummaryCid,
  stage as Stage,
  substage as SubStage,
  namespace as Namespace,
  username as UserName,
  agentuuid as AgentUUID,
  runuuid as RunUUID,
  queryuuid as QueryUUID,
  stepuuid as StepUUID,
  messagetime as MessageTime,
  createdby as CreatedBy,
  createdat as CreatedAt,
  changedby as ChangedBy,
  changedat as ChangedAt
}
