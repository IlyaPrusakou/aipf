@Metadata.allowExtensions: true
@Metadata.ignorePropagatedAnnotations: true
@EndUserText: {
  label: '###GENERATED Core Data Service Entity'
}
@ObjectModel: {
  sapObjectNodeType.name: 'ZPRU_AGENT_SERV'
}
@AccessControl.authorizationCheck: #MANDATORY
define root view entity ZC_PRU_AGENT_SERV
  provider contract transactional_query
  as projection on ZR_PRU_AGENT_SERV
  association [1..1] to ZR_PRU_AGENT_SERV as _BaseEntity on  $projection.AIPF7Service = _BaseEntity.AIPF7Service
                                                         and $projection.AIPF7Context = _BaseEntity.AIPF7Context
{
  key AIPF7Service,
  key AIPF7Context,
      AIPF7Class,
      AIPF7CreatedBy,
      AIPF7CreatedAt,
      AIPF7ChangedBy,
      @Semantics: {
        systemDateTime.lastChangedAt: true
      }
      AIPF7LastChanged,
      @Semantics: {
        systemDateTime.localInstanceLastChangedAt: true
      }
      AIPF7LocalLastChanged,
      _BaseEntity
}
