@AccessControl.authorizationCheck: #MANDATORY
@Metadata.allowExtensions: true
@ObjectModel.sapObjectNodeType.name: 'ZPRU_AGENT_TYPE'
@EndUserText.label: '###GENERATED Core Data Service Entity'
define root view entity ZR_PRU_AGENT_TYPE
  as select from ZI_PRU_AGENT_TYPE
{
  key AIPF7AgentType         as AIPF7AgentType,
      AIPF7ShortMemoryVolume as AIPF7ShortMemoryVolume,
      AIPF7DiscardStrategy   as AIPF7DiscardStrategy,
      AIPF7SummaryStrategy   as AIPF7SummaryStrategy,
      AIPF7MaxNumberOfLoop   as AIPF7MaxNumberOfLoop,
      AIPF7CreatedBy         as AIPF7CreatedBy,
      AIPF7CreatedAt         as AIPF7CreatedAt,
      AIPF7ChangedBy         as AIPF7ChangedBy,
      @Semantics.systemDateTime.lastChangedAt: true
      AIPF7LastChanged       as AIPF7LastChanged,
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      AIPF7LocalLastChanged  as AIPF7LocalLastChanged
}
