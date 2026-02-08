@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Agent Service Basic'
@Metadata.ignorePropagatedAnnotations: true
@VDM.viewType: #BASIC
define view entity ZI_PRU_AGENT_SERV
  as select from zpru_agent_serv
{
  key service          as AIPF7Service,
  key context          as AIPF7Context,
      class            as AIPF7Class,
      createdby        as AIPF7CreatedBy,
      createdat        as AIPF7CreatedAt,
      changedby        as AIPF7ChangedBy,
      lastchanged      as AIPF7LastChanged,
      locallastchanged as AIPF7LocalLastChanged
}
