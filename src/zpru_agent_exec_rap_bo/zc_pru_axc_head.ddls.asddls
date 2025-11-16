@Metadata.allowExtensions: true
@Metadata.ignorePropagatedAnnotations: true
@Endusertext: {
  Label: '###GENERATED Core Data Service Entity'
}
@Objectmodel: {
  Sapobjectnodetype.Name: 'ZPRU_AXC_HEAD'
}
@AccessControl.authorizationCheck: #MANDATORY
define root view entity ZC_PRU_AXC_HEAD
  provider contract TRANSACTIONAL_QUERY
  as projection on ZR_PRU_AXC_HEAD
  association [1..1] to ZR_PRU_AXC_HEAD as _BaseEntity on $projection.RUNUUID = _BaseEntity.RUNUUID
{
  key RunUUID,
  AgentUUID,
  UserID,
  StartTimestamp,
  EndTimestamp,
  RunStatus,
  CreatedBy,
  CreatedAt,
  ChangedBy,
  @Semantics: {
    Systemdatetime.Lastchangedat: true
  }
  LastChanged,
  @Semantics: {
    Systemdatetime.Localinstancelastchangedat: true
  }
  LocalLastChanged,
  _BaseEntity
}
