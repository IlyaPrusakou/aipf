@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Agent Type Basic'
@Metadata.ignorePropagatedAnnotations: true
@VDM.viewType: #BASIC
define view entity ZI_PRU_AGENT_TYPE as select from zpru_agent_type
{
    key agent_type as AgentType,
    short_mem_volume as ShortMemVolume,
    discard_strategy as DiscardStrategy,
    summary_strategy as SummaryStrategy,
    max_numb_loop as MaxNumbLoop,
    created_by as CreatedBy,
    created_at as CreatedAt,
    changed_by as ChangedBy,
    last_changed as LastChanged,
    local_last_changed as LocalLastChanged
}
