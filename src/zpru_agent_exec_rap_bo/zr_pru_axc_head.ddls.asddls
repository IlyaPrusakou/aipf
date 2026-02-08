@AccessControl.authorizationCheck: #MANDATORY
@Metadata.allowExtensions: true
@ObjectModel.sapObjectNodeType.name: 'ZPRU_AXC_HEAD'
@EndUserText.label: '###GENERATED Core Data Service Entity'
define root view entity ZR_PRU_AXC_HEAD
  as select from ZI_PRU_AXC_HEAD
  composition of exact one to many ZR_PRU_AXC_QUERY as _executionquery
  association of exact one to many ZR_PRU_AXC_STEP  as _executionstep on $projection.AIPF7RunUUID = _executionstep.AIPF7RunUuid
{
  key AIPF7RunUuid          as AIPF7RunUUID,
      AIPF7RunId            as AIPF7RunID,
      AIPF7AgentUuid        as AIPF7AgentUUID,
      AIPF7UserId           as AIPF7UserID,
      AIPF7RunStartDateTime    as AIPF7RunStartDateTime,
      AIPF7RunEndDateTime      as AIPF7RunEndDateTime,
      AIPF7CreatedBy        as AIPF7CreatedBy,
      AIPF7CreatedAt        as AIPF7CreatedAt,
      AIPF7ChangedBy        as AIPF7ChangedBy,
      @Semantics.systemDateTime.lastChangedAt: true
      AIPF7LastChanged      as AIPF7LastChanged,
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      AIPF7LocalLastChanged as AIPF7LocalLastChanged,
      _executionquery,
      _executionstep
}
