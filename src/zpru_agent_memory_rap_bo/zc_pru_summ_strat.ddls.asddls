@Metadata.allowExtensions: true
@Metadata.ignorePropagatedAnnotations: true
@EndUserText: {
  label: '###GENERATED Core Data Service Entity'
}
@ObjectModel: {
  sapObjectNodeType.name: 'ZPRU_SUMM_STRAT'
}
@AccessControl.authorizationCheck: #MANDATORY
define root view entity ZC_PRU_SUMM_STRAT
  provider contract transactional_query
  as projection on ZR_PRU_SUMM_STRAT
  association [1..1] to ZR_PRU_SUMM_STRAT as _BaseEntity on $projection.AIPF7SummaryStrategy = _BaseEntity.AIPF7SummaryStrategy
{
  key AIPF7SummaryStrategy,
      AIPF7SummaryProvider,
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
