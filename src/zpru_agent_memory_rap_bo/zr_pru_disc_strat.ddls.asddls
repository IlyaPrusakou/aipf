@AccessControl.authorizationCheck: #MANDATORY
@Metadata.allowExtensions: true
@ObjectModel.sapObjectNodeType.name: 'ZPRU_DISC_STRAT'
@EndUserText.label: '###GENERATED Core Data Service Entity'
define root view entity ZR_PRU_DISC_STRAT 
  as select from zpru_disc_strat
{
  key discardstrategy as DiscardStrategy,
  strategyprovider as StrategyProvider,
  createdby as CreatedBy,
  createdat as CreatedAt,
  changedby as ChangedBy,
  @Semantics.systemDateTime.lastChangedAt: true
  lastchanged as LastChanged,
  @Semantics.systemDateTime.localInstanceLastChangedAt: true
  locallastchanged as LocalLastChanged
}
