@AccessControl.authorizationCheck: #MANDATORY
@Metadata.allowExtensions: true
@ObjectModel.sapObjectNodeType.name: 'ZPRU_MEM_MSG'
@EndUserText.label: '###GENERATED Core Data Service Entity'
define root view entity ZR_PRU_MEM_MSG
  as select from ZPRU_MEM_MSG
{
  key message_uuid as MessageUUID,
  content as Content,
  message_type as MessageType,
  message_cid as MessageCid,
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
