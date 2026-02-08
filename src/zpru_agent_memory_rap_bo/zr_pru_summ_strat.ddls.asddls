@AccessControl.authorizationCheck: #MANDATORY
@Metadata.allowExtensions: true
@ObjectModel.sapObjectNodeType.name: 'ZPRU_SUMM_STRAT'
@EndUserText.label: '###GENERATED Core Data Service Entity'
define root view entity ZR_PRU_SUMM_STRAT
  as select from zpru_summ_strat
{
  key summarystrategy as SummaryStrategy,
  strategyprovider as StrategyProvider,
  createdby as CreatedBy,
  createdat as CreatedAt,
  changedby as ChangedBy,
  @Semantics.systemDateTime.lastChangedAt: true
  lastchanged as LastChanged,
  @Semantics.systemDateTime.localInstanceLastChangedAt: true
  locallastchanged as LocalLastChanged
}
