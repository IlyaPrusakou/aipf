@Metadata.allowExtensions: true
@Metadata.ignorePropagatedAnnotations: true
@Endusertext: {
  Label: '###GENERATED Core Data Service Entity'
}
@Objectmodel: {
  Sapobjectnodetype.Name: 'ZPRU_AGENT_SERV'
}
@AccessControl.authorizationCheck: #MANDATORY
define root view entity ZC_PRU_AGENT_SERV
  provider contract TRANSACTIONAL_QUERY
  as projection on ZR_PRU_AGENT_SERV
  association [1..1] to ZR_PRU_AGENT_SERV as _BaseEntity on $projection.SERVICE = _BaseEntity.SERVICE and $projection.CONTEXT = _BaseEntity.CONTEXT
{
  key Service,
  key Context,
  Class,
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
