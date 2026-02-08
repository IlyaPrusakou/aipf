@Metadata.allowExtensions: true
@Metadata.ignorePropagatedAnnotations: true
@EndUserText: {
  label: '###GENERATED Core Data Service Entity'
}
@ObjectModel: {
  sapObjectNodeType.name: 'ZPRU_AGENT_TYPE'
}
@AccessControl.authorizationCheck: #MANDATORY
define root view entity ZC_PRU_AGENT_TYPE
  provider contract transactional_query
  as projection on ZR_PRU_AGENT_TYPE
  association [1..1] to ZR_PRU_AGENT_TYPE as _BaseEntity on $projection.AIPF7AgentType = _BaseEntity.AIPF7AgentType
{
  key AIPF7AgentType,
      AIPF7ShortMemoryVolume,
      AIPF7DiscardStrategy,
      AIPF7SummaryStrategy,
      AIPF7MaxNumberOfLoop,
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
      _BaseEntity
}
