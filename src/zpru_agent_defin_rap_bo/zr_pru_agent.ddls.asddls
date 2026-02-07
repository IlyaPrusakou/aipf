@AccessControl.authorizationCheck: #MANDATORY
@Metadata.allowExtensions: true
@ObjectModel.sapObjectNodeType.name: 'ZPRU_AGENT'
@EndUserText.label: '###GENERATED Core Data Service Entity'
define root view entity ZR_PRU_AGENT
  as select from ZI_PRU_AGENT
  composition of exact one to many ZR_PRU_AGENT_TOOL as _tool
{
  key AgentUuid            as AgentUUID,
      AgentName            as AgentName,
      AgentType            as AgentType,
      DecisionProvider     as DecisionProvider,
      ShortMemoryProvider  as ShortMemoryProvider,
      LongMemoryProvider   as LongMemoryProvider,
      AgentInfoProvider    as AgentInfoProvider,
      SystemPromptProvider as SystemPromptProvider,
      Status               as Status,
      CreatedBy            as CreatedBy,
      CreatedAt            as CreatedAt,
      ChangedBy            as ChangedBy,
      @Semantics.systemDateTime.lastChangedAt: true
      LastChanged          as LastChanged,
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      LocalLastChanged     as LocalLastChanged,
      _tool
}
