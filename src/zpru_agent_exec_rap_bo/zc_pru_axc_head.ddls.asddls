@Metadata.allowExtensions: true
@Metadata.ignorePropagatedAnnotations: true
@EndUserText: {
  label: '###GENERATED Core Data Service Entity'
}
@ObjectModel: {
  sapObjectNodeType.name: 'ZPRU_AXC_HEAD'
}
@AccessControl.authorizationCheck: #MANDATORY
define root view entity ZC_PRU_AXC_HEAD
  provider contract transactional_query
  as projection on ZR_PRU_AXC_HEAD
  association [1..1] to ZR_PRU_AXC_HEAD as _BaseEntity on $projection.AIPF7RunUUID = _BaseEntity.AIPF7RunUUID
{
  key AIPF7RunUUID,
      AIPF7RunID,
      AIPF7AgentUUID,
      AIPF7UserID,
      AIPF7RunStartDateTime,
      AIPF7RunEndDateTime,
      AIPF7CreatedBy,
      AIPF7CreatedAt,
      AIPF7ChangedBy,
      @Semantics: {
        systemDateTime.lastChangedAt: true
      }
      AIPF7LastChanged,
      @Semantics: {
        systemDateTime.localInstanceLastChangedAt: true
      }
      AIPF7LocalLastChanged,
      _executionquery : redirected to composition child ZC_PRU_AXC_QUERY,
      _executionstep  : redirected to ZC_PRU_AXC_STEP,
      _BaseEntity
}
