@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Agent Type Basic'
@Metadata.ignorePropagatedAnnotations: true
@VDM.viewType: #BASIC
define view entity ZI_PRU_AGENT_TYPE
  as select from zpru_agent_type
{
  key agenttype        as AIPF7AgentType,
      ShortMemoryVolume   as AIPF7ShortMemoryVolume,
      discardstrategy  as AIPF7DiscardStrategy,
      summarystrategy  as AIPF7SummaryStrategy,
      MaximumNumberOfLoop      as AIPF7MaximumNumberOfLoop,
      createdby        as AIPF7CreatedBy,
      createdat        as AIPF7CreatedAt,
      changedby        as AIPF7ChangedBy,
      lastchanged      as AIPF7LastChanged,
      locallastchanged as AIPF7LocalLastChanged
}
