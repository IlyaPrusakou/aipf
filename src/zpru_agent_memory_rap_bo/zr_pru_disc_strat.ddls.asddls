@AccessControl.authorizationCheck: #MANDATORY
@Metadata.allowExtensions: true
@ObjectModel.sapObjectNodeType.name: 'ZPRU_DISC_STRAT'
@EndUserText.label: '###GENERATED Core Data Service Entity'
define root view entity ZR_PRU_DISC_STRAT
  as select from zpru_disc_strat
{
  key discardstrategy  as AIPF7DiscardStrategy,
      DiscardProvider as AIPF7DiscardProvider,
      createdby        as AIPF7CreatedBy,
      createdat        as AIPF7CreatedAt,
      changedby        as AIPF7ChangedBy,
      @Semantics.systemDateTime.lastChangedAt: true
      lastchanged      as AIPF7LastChanged,
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      locallastchanged as AIPF7LocalLastChanged
}
