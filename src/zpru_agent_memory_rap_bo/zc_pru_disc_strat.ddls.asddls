@Metadata.allowExtensions: true
@Metadata.ignorePropagatedAnnotations: true
@EndUserText: {
  label: '###GENERATED Core Data Service Entity'
}
@ObjectModel: {
  sapObjectNodeType.name: 'ZPRU_DISC_STRAT'
}
@AccessControl.authorizationCheck: #MANDATORY
define root view entity ZC_PRU_DISC_STRAT
  provider contract transactional_query
  as projection on ZR_PRU_DISC_STRAT
  association [1..1] to ZR_PRU_DISC_STRAT as _BaseEntity on $projection.AIPF7DiscardStrategy = _BaseEntity.AIPF7DiscardStrategy
{
  key AIPF7DiscardStrategy,
      AIPF7DiscardProvider,
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
