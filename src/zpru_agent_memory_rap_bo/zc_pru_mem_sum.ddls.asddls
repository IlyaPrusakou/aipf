@Metadata.allowExtensions: true
@Metadata.ignorePropagatedAnnotations: true
@Endusertext: {
  Label: '###GENERATED Core Data Service Entity'
}
@Objectmodel: {
  Sapobjectnodetype.Name: 'ZPRU_MEM_SUM'
}
@AccessControl.authorizationCheck: #MANDATORY
define root view entity ZC_PRU_MEM_SUM
  provider contract TRANSACTIONAL_QUERY
  as projection on ZR_PRU_MEM_SUM
  association [1..1] to ZR_PRU_MEM_SUM as _BaseEntity on $projection.SUMMARYUUID = _BaseEntity.SUMMARYUUID
{
  key SummaryUUID,
  Content,
  SummaryCid,
  Stage,
  SubStage,
  Namespace,
  UserName,
  AgentUUID,
  RunUUID,
  QueryUUID,
  StepUUID,
  MessageTime,
  CreatedBy,
  CreatedAt,
  ChangedBy,
  ChangedAt,
  _BaseEntity
}
