@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Execution Header Basic'
@Metadata.ignorePropagatedAnnotations: true
@VDM.viewType: #BASIC
define view entity ZI_PRU_AXC_HEAD
  as select from zpru_axc_head
    association to many ZI_PRU_AXC_QUERY as _executionquery on _executionquery.RunUuid = $projection.RunUuid
    association to many ZI_PRU_AXC_STEP as _executionstep on _executionstep.RunUuid = $projection.RunUuid    
{
  key run_uuid           as RunUuid,
      run_id             as RunId,
      agent_uuid         as AgentUuid,
      user_id            as UserId,
      start_timestamp    as StartTimestamp,
      end_timestamp      as EndTimestamp,
      created_by         as CreatedBy,
      created_at         as CreatedAt,
      changed_by         as ChangedBy,
      last_changed       as LastChanged,
      local_last_changed as LocalLastChanged,
      _executionquery,
      _executionstep
}
