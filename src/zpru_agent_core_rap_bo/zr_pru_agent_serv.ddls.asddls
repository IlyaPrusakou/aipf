@AccessControl.authorizationCheck: #MANDATORY
@Metadata.allowExtensions: true
@ObjectModel.sapObjectNodeType.name: 'ZPRU_AGENT_SERV'
@EndUserText.label: '###GENERATED Core Data Service Entity'
define root view entity ZR_PRU_AGENT_SERV
  as select from ZPRU_AGENT_SERV
{
  key service as Service,
  key context as Context,
  class as Class,
  created_by as CreatedBy,
  created_at as CreatedAt,
  changed_by as ChangedBy,
  @Semantics.systemDateTime.lastChangedAt: true
  last_changed as LastChanged,
  @Semantics.systemDateTime.localInstanceLastChangedAt: true
  local_last_changed as LocalLastChanged
}
