@Metadata.allowExtensions: true
@Metadata.ignorePropagatedAnnotations: true
@Endusertext: {
  Label: '###GENERATED Core Data Service Entity'
}
@Objectmodel: {
  Sapobjectnodetype.Name: 'ZPRU_AGENT'
}
@AccessControl.authorizationCheck: #MANDATORY
define root view entity ZC_PRU_AGENT
  provider contract TRANSACTIONAL_QUERY
  as projection on ZR_PRU_AGENT
  association [1..1] to ZR_PRU_AGENT as _BaseEntity on $projection.AGENTUUID = _BaseEntity.AGENTUUID
{
  key AgentUUID,
  AgentName,
  DecisionProvider,
  ShortMemoryProvider,
  LongMemoryProvider,
  AgentInfoProvider,
  SystemPromptProvider,
  Status,
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
