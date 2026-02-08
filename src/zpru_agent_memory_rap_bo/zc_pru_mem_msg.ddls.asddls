@Metadata.allowExtensions: true
@Metadata.ignorePropagatedAnnotations: true
@EndUserText: {
  label: '###GENERATED Core Data Service Entity'
}
@ObjectModel: {
  sapObjectNodeType.name: 'ZPRU_MEM_MSG'
}
@AccessControl.authorizationCheck: #MANDATORY
define root view entity ZC_PRU_MEM_MSG
  provider contract transactional_query
  as projection on ZR_PRU_MEM_MSG
  association [1..1] to ZR_PRU_MEM_MSG as _BaseEntity on $projection.AIPF7MessageUUID = _BaseEntity.AIPF7MessageUUID
{
  key AIPF7MessageUUID,
      AIPF7Content,
      AIPF7MessageType,
      AIPF7MessageContentId,
      AIPF7Stage,
      AIPF7SubStage,
      AIPF7Namespace,
      AIPF7UserName,
      AIPF7AgentUUID,
      AIPF7RunUUID,
      AIPF7QueryUUID,
      AIPF7StepUUID,
      AIPF7MessageDateTime,
      AIPF7CreatedBy,
      AIPF7CreatedAt,
      AIPF7ChangedBy,
      AIPF7ChangedAt,
      _BaseEntity
}
