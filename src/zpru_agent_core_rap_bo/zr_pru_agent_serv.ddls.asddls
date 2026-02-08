@AccessControl.authorizationCheck: #MANDATORY
@Metadata.allowExtensions: true
@ObjectModel.sapObjectNodeType.name: 'ZPRU_AGENT_SERV'
@EndUserText.label: '###GENERATED Core Data Service Entity'
define root view entity ZR_PRU_AGENT_SERV
  as select from ZI_PRU_AGENT_SERV
{
  key AIPF7Service          as AIPF7Service,
  key AIPF7Context          as AIPF7Context,
      AIPF7Class            as AIPF7Class,
      AIPF7CreatedBy        as AIPF7CreatedBy,
      AIPF7CreatedAt        as AIPF7CreatedAt,
      AIPF7ChangedBy        as AIPF7ChangedBy,
      @Semantics.systemDateTime.lastChangedAt: true
      AIPF7LastChanged      as AIPF7LastChanged,
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      AIPF7LocalLastChanged as AIPF7LocalLastChanged
}
