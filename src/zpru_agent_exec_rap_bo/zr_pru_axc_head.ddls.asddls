@AccessControl.authorizationCheck: #MANDATORY
@Metadata.allowExtensions: true
@ObjectModel.sapObjectNodeType.name: 'ZPRU_AXC_HEAD'
@EndUserText.label: '###GENERATED Core Data Service Entity'
define root view entity ZR_PRU_AXC_HEAD
  as select from ZPRU_AXC_HEAD
{
  key run_uuid as RunUUID,
  agent_uuid as AgentUUID,
  user_id as UserID,
  start_timestamp as StartTimestamp,
  end_timestamp as EndTimestamp,
  run_status as RunStatus,
  created_by as CreatedBy,
  created_at as CreatedAt,
  changed_by as ChangedBy,
  @Semantics.systemDateTime.lastChangedAt: true
  last_changed as LastChanged,
  @Semantics.systemDateTime.localInstanceLastChangedAt: true
  local_last_changed as LocalLastChanged
}
