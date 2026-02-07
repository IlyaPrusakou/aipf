@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Agent Basic'
@Metadata.ignorePropagatedAnnotations: true
@VDM.viewType: #BASIC
define view entity ZI_PRU_AGENT
  as select from zpru_agent
  association to many ZI_PRU_AGENT_TOOL as _tool on _tool.AgentUuid = $projection.AgentUuid
{
  key agent_uuid             as AgentUuid,
      agent_type             as AgentType,
      agent_name             as AgentName,
      decision_provider      as DecisionProvider,
      short_memory_provider  as ShortMemoryProvider,
      long_memory_provider   as LongMemoryProvider,
      agent_info_provider    as AgentInfoProvider,
      system_prompt_provider as SystemPromptProvider,
      status                 as Status,
      created_by             as CreatedBy,
      created_at             as CreatedAt,
      changed_by             as ChangedBy,
      last_changed           as LastChanged,
      local_last_changed     as LocalLastChanged,
      _tool
}
