@AccessControl.authorizationCheck: #MANDATORY
@Metadata.allowExtensions: true
@ObjectModel.sapObjectNodeType.name: 'ZPRU_AGENT_SERV'
@EndUserText.label: '###GENERATED Core Data Service Entity'
define root view entity ZR_PRU_AGENT_SERV
  as select from ZI_PRU_AGENT_SERV
{
  key Service          as Service,
  key Context          as Context,
      Class            as Class,
      CreatedBy        as CreatedBy,
      CreatedAt        as CreatedAt,
      ChangedBy        as ChangedBy,
      @Semantics.systemDateTime.lastChangedAt: true
      LastChanged      as LastChanged,
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      LocalLastChanged as LocalLastChanged
}
