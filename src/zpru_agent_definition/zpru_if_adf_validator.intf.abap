INTERFACE zpru_if_adf_validator
  PUBLIC .

  METHODS check_decision_provider
    IMPORTING it_keys     TYPE zpru_if_adf_type_and_constant=>tt_agent_read_k
    CHANGING  cs_reported TYPE zpru_if_agent_frw=>ts_adf_reported OPTIONAL
              cs_failed   TYPE zpru_if_agent_frw=>ts_adf_failed   OPTIONAL.

  METHODS check_short_memory
    IMPORTING it_keys     TYPE zpru_if_adf_type_and_constant=>tt_agent_read_k
    CHANGING  cs_reported TYPE zpru_if_agent_frw=>ts_adf_reported OPTIONAL
              cs_failed   TYPE zpru_if_agent_frw=>ts_adf_failed   OPTIONAL.

  METHODS check_long_memory
    IMPORTING it_keys     TYPE zpru_if_adf_type_and_constant=>tt_agent_read_k
    CHANGING  cs_reported TYPE zpru_if_agent_frw=>ts_adf_reported OPTIONAL
              cs_failed   TYPE zpru_if_agent_frw=>ts_adf_failed   OPTIONAL.

  METHODS check_agent_info
    IMPORTING it_keys     TYPE zpru_if_adf_type_and_constant=>tt_agent_read_k
    CHANGING  cs_reported TYPE zpru_if_agent_frw=>ts_adf_reported OPTIONAL
              cs_failed   TYPE zpru_if_agent_frw=>ts_adf_failed   OPTIONAL.

  METHODS check_tool_provider
    IMPORTING it_keys     TYPE zpru_if_adf_type_and_constant=>tt_agent_read_k
    CHANGING  cs_reported TYPE zpru_if_agent_frw=>ts_adf_reported OPTIONAL
              cs_failed   TYPE zpru_if_agent_frw=>ts_adf_failed   OPTIONAL.

  METHODS check_tool_schema_provider
    IMPORTING it_keys     TYPE zpru_if_adf_type_and_constant=>tt_agent_read_k
    CHANGING  cs_reported TYPE zpru_if_agent_frw=>ts_adf_reported OPTIONAL
              cs_failed   TYPE zpru_if_agent_frw=>ts_adf_failed   OPTIONAL.


  METHODS check_info_provider
    IMPORTING it_keys     TYPE zpru_if_adf_type_and_constant=>tt_agent_read_k
    CHANGING  cs_reported TYPE zpru_if_agent_frw=>ts_adf_reported OPTIONAL
              cs_failed   TYPE zpru_if_agent_frw=>ts_adf_failed   OPTIONAL.


  METHODS validate_provider_cls
    IMPORTING iv_class            TYPE zpru_de_seoclname
              iv_intf_2_be_impl_1 TYPE zpru_de_seoclname opTIONAL
              iv_intf_2_be_impl_2 TYPE zpru_de_seoclname opTIONAL
    EXPORTING ev_type_not_exist    TYPE abap_boolean
              ev_type_not_class    TYPE abap_boolean
              ev_intf_not_impl_1  TYPE abap_boolean
              ev_intf_not_impl_2  TYPE abap_boolean .

ENDINTERFACE.
