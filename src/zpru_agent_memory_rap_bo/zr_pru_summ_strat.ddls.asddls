@AccessControl.authorizationCheck: #MANDATORY
@Metadata.allowExtensions: true
@ObjectModel.sapObjectNodeType.name: 'ZPRU_SUMM_STRAT'
@EndUserText.label: '###GENERATED Core Data Service Entity'
define root view entity ZR_PRU_SUMM_STRAT
  as select from zpru_summ_strat
{
  key summarystrategy  as AIPF7SummaryStrategy,
      SummaryProvider as AIPF7SummaryProvider,
      createdby        as AIPF7CreatedBy,
      createdat        as AIPF7CreatedAt,
      changedby        as AIPF7ChangedBy,
      @Semantics.systemDateTime.lastChangedAt: true
      lastchanged      as AIPF7LastChanged,
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      locallastchanged as AIPF7LocalLastChanged
}
