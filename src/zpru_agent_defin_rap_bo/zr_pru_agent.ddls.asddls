@AccessControl.authorizationCheck: #MANDATORY
@Metadata.allowExtensions: true
@ObjectModel.sapObjectNodeType.name: 'ZPRU_AGENT'
@EndUserText.label: '###GENERATED Core Data Service Entity'
define root view entity ZR_PRU_AGENT
  as select from ZI_PRU_AGENT
  composition of exact one to many ZR_PRU_AGENT_TOOL as _tool
{
  key AIPF7AgentUuid            as AIPF7AgentUUID,
      AIPF7AgentName            as AIPF7AgentName,
      AIPF7AgentType            as AIPF7AgentType,
      AIPF7DecisionProvider     as AIPF7DecisionProvider,
      AIPF7ShortMemoryProvider  as AIPF7ShortMemoryProvider,
      AIPF7LongMemoryProvider   as AIPF7LongMemoryProvider,
      AIPF7AgentInfoProvider    as AIPF7AgentInfoProvider,
      AIPF7SystemPromptProvider as AIPF7SystemPromptProvider,
      AIPF7AgentStatus          as AIPF7AgentStatus,
      AIPF7CreatedBy            as AIPF7CreatedBy,
      AIPF7CreatedAt            as AIPF7CreatedAt,
      AIPF7ChangedBy            as AIPF7ChangedBy,
      @Semantics.systemDateTime.lastChangedAt: true
      AIPF7LastChanged          as AIPF7LastChanged,
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      AIPF7LocalLastChanged     as AIPF7LocalLastChanged
      ,
      _tool
}
