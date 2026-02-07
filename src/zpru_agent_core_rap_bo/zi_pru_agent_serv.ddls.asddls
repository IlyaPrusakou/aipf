@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Agent Service Basic'
@Metadata.ignorePropagatedAnnotations: true
@VDM.viewType: #BASIC
define view entity ZI_PRU_AGENT_SERV
  as select from zpru_agent_serv
{
  key service            as Service,
  key context            as Context,
      class              as Class,
      created_by         as CreatedBy,
      created_at         as CreatedAt,
      changed_by         as ChangedBy,
      last_changed       as LastChanged,
      local_last_changed as LocalLastChanged
}
