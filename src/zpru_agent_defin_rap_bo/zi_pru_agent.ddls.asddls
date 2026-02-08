@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Agent Basic'
@Metadata.ignorePropagatedAnnotations: true
@VDM.viewType: #BASIC
define view entity ZI_PRU_AGENT
  as select from zpru_agent
  association to many ZI_PRU_AGENT_TOOL as _tool on _tool.AIPF7AgentUuid = $projection.AIPF7AgentUuid
{
  key agentuuid            as AIPF7AgentUuid,
      agenttype            as AIPF7AgentType,
      agentname            as AIPF7AgentName,
      decisionprovider     as AIPF7DecisionProvider,
      shortmemoryprovider  as AIPF7ShortMemoryProvider,
      longmemoryprovider   as AIPF7LongMemoryProvider,
      agentinfoprovider    as AIPF7AgentInfoProvider,
      systempromptprovider as AIPF7SystemPromptProvider,
      status               as AIPF7AgentStatus,
      createdby            as AIPF7CreatedBy,
      createdat            as AIPF7CreatedAt,
      changedby            as AIPF7ChangedBy,
      lastchanged          as AIPF7LastChanged,
      locallastchanged     as AIPF7LocalLastChanged
      ,
      _tool
}
