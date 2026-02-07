@AccessControl.authorizationCheck: #MANDATORY
@Metadata.allowExtensions: true
@ObjectModel.sapObjectNodeType.name: 'ZPRU_AXC_HEAD'
@EndUserText.label: '###GENERATED Core Data Service Entity'
define root view entity ZR_PRU_AXC_HEAD
  as select from ZI_PRU_AXC_HEAD
  composition of exact one to many ZR_PRU_AXC_QUERY as _executionquery
  association of exact one to many ZR_PRU_AXC_STEP  as _executionstep on $projection.RunUUID = _executionstep.RunUuid
{
  key RunUuid          as RunUUID,
      RunId            as RunID,
      AgentUuid        as AgentUUID,
      UserId           as UserID,
      StartTimestamp   as StartTimestamp,
      EndTimestamp     as EndTimestamp,
      CreatedBy        as CreatedBy,
      CreatedAt        as CreatedAt,
      ChangedBy        as ChangedBy,
      @Semantics.systemDateTime.lastChangedAt: true
      LastChanged      as LastChanged,
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      LocalLastChanged as LocalLastChanged,
      _executionquery,
      _executionstep
}
