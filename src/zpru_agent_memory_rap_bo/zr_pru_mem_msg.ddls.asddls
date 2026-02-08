@AccessControl.authorizationCheck: #MANDATORY
@Metadata.allowExtensions: true
@ObjectModel.sapObjectNodeType.name: 'ZPRU_MEM_MSG'
@EndUserText.label: '###GENERATED Core Data Service Entity'
define root view entity ZR_PRU_MEM_MSG
  as select from zpru_mem_msg
{
  key messageuuid as AIPF7MessageUUID,
      content     as AIPF7Content,
      messagetype as AIPF7MessageType,
      MessageContentId  as AIPF7MessageContentId,
      stage       as AIPF7Stage,
      substage    as AIPF7SubStage,
      namespace   as AIPF7Namespace,
      username    as AIPF7UserName,
      agentuuid   as AIPF7AgentUUID,
      runuuid     as AIPF7RunUUID,
      queryuuid   as AIPF7QueryUUID,
      stepuuid    as AIPF7StepUUID,
      MessageDateTime as AIPF7MessageDateTime,
      createdby   as AIPF7CreatedBy,
      createdat   as AIPF7CreatedAt,
      changedby   as AIPF7ChangedBy,
      changedat   as AIPF7ChangedAt
}
