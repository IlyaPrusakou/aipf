@Metadata.allowExtensions: true
@Metadata.ignorePropagatedAnnotations: true
@Endusertext: {
  Label: '###GENERATED Core Data Service Entity'
}
@Objectmodel: {
  Sapobjectnodetype.Name: 'ZPRU_MEM_MSG'
}
@AccessControl.authorizationCheck: #MANDATORY
define root view entity ZC_PRU_MEM_MSG
  provider contract TRANSACTIONAL_QUERY
  as projection on ZR_PRU_MEM_MSG
  association [1..1] to ZR_PRU_MEM_MSG as _BaseEntity on $projection.MESSAGEUUID = _BaseEntity.MESSAGEUUID
{
  key MessageUUID,
  Content,
  MessageType,
  MessageCid,
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
