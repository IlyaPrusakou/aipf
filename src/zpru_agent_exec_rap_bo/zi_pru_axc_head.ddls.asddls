@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Execution Header Basic'
@Metadata.ignorePropagatedAnnotations: true
@VDM.viewType: #BASIC
define view entity ZI_PRU_AXC_HEAD
  as select from zpru_axc_head
  association to many ZI_PRU_AXC_QUERY as _executionquery on _executionquery.aipf7runuuid = $projection.AIPF7RunUuid
  association to many ZI_PRU_AXC_STEP  as _executionstep  on _executionstep.aipf7runuuid = $projection.AIPF7RunUuid
{
  key runuuid          as AIPF7RunUuid,
      runid            as AIPF7RunId,
      agentuuid        as AIPF7AgentUuid,
      userid           as AIPF7UserId,
      RunStartDateTime   as AIPF7RunStartDateTime,
      RunEndDateTime     as AIPF7RunEndDateTime,
      createdby        as AIPF7CreatedBy,
      createdat        as AIPF7CreatedAt,
      changedby        as AIPF7ChangedBy,
      lastchanged      as AIPF7LastChanged,
      locallastchanged as AIPF7LocalLastChanged,
      _executionquery,
      _executionstep
}
