@AccessControl.authorizationCheck: #MANDATORY
@Metadata.allowExtensions: true
@ObjectModel.sapObjectNodeType.name: 'ZPRU_AGENT_TYPE'
@EndUserText.label: '###GENERATED Core Data Service Entity'
define root view entity ZR_PRU_AGENT_TYPE
  as select from ZI_PRU_AGENT_TYPE
{
  key AgentType        as AgentType,
      ShortMemVolume   as ShortMemVolume,
      DiscardStrategy  as DiscardStrategy,
      SummaryStrategy  as SummaryStrategy,
      MaxNumbLoop      as MaxNumbLoop,
      CreatedBy        as CreatedBy,
      CreatedAt        as CreatedAt,
      ChangedBy        as ChangedBy,
      @Semantics.systemDateTime.lastChangedAt: true
      LastChanged      as LastChanged,
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      LocalLastChanged as LocalLastChanged
}
