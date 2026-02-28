CLASS zpru_cl_agent_info_provider DEFINITION
  PUBLIC ABSTRACT
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zpru_if_agent_frw.
    INTERFACES zpru_if_agent_info_provider.

  PROTECTED SECTION.
    METHODS get_agent_main_info ABSTRACT
      IMPORTING iv_agent_uuid   TYPE sysuuid_x16
      EXPORTING ev_agentname    TYPE char100
                ev_agentversion TYPE char100
                ev_agentrole    TYPE string.

    METHODS set_agent_goals ABSTRACT
      IMPORTING iv_agent_uuid         TYPE sysuuid_x16
      RETURNING VALUE(rt_agent_goals) TYPE zpru_tt_agent_goals.

    METHODS prepare_agent_domains ABSTRACT
      IMPORTING iv_agent_uuid           TYPE sysuuid_x16
      RETURNING VALUE(rs_agent_domains) TYPE zpru_s_agent_domain.

    METHODS set_agent_restrictions ABSTRACT
      IMPORTING iv_agent_uuid                TYPE sysuuid_x16
      RETURNING VALUE(rt_agent_restrictions) TYPE zpru_tt_agent_restrictions.

    METHODS set_tool_metadata ABSTRACT
      IMPORTING iv_agent_uuid           TYPE sysuuid_x16
      RETURNING VALUE(rt_tool_metadata) TYPE zpru_tt_tool_info.

    METHODS get_free_text ABSTRACT
      IMPORTING iv_agent_uuid      TYPE sysuuid_x16
      EXPORTING ev_freetext        TYPE char100
                ev_freetextcontent TYPE string.

  PRIVATE SECTION.
ENDCLASS.


CLASS zpru_cl_agent_info_provider IMPLEMENTATION.
  METHOD zpru_if_agent_info_provider~get_abap_agent_info.
    DATA ls_agent_info TYPE zpru_s_agent_info.

    get_agent_main_info( EXPORTING iv_agent_uuid   = iv_agent_uuid
                         IMPORTING ev_agentname    = ls_agent_info-agentname
                                   ev_agentversion = ls_agent_info-agentversion
                                   ev_agentrole    = ls_agent_info-agentrole ).

    ls_agent_info-agentgoals        = set_agent_goals( iv_agent_uuid = iv_agent_uuid ).

    ls_agent_info-agentdomain       = prepare_agent_domains( iv_agent_uuid = iv_agent_uuid ).

    ls_agent_info-agentrestrictions = set_agent_restrictions( iv_agent_uuid = iv_agent_uuid ).

    ls_agent_info-agenttools        = set_tool_metadata( iv_agent_uuid = iv_agent_uuid ).

    get_free_text( EXPORTING iv_agent_uuid      = iv_agent_uuid
                   IMPORTING ev_freetext        = ls_agent_info-freetextlabel
                             ev_freetextcontent = ls_agent_info-freetextcontent ).

    rs_agent_info = ls_agent_info.
  ENDMETHOD.

  METHOD zpru_if_agent_info_provider~get_agent_info.
    DATA lo_util TYPE REF TO zpru_if_agent_util.

    lo_util ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_AGENT_UTIL`
                                                        iv_context = zpru_if_agent_frw=>cs_context-standard ).

    DATA(ls_abap_agent_info) = me->zpru_if_agent_info_provider~get_abap_agent_info( iv_agent_uuid = iv_agent_uuid ).

    DATA(lv_free_text_label) = ls_abap_agent_info-freetextlabel.

    " we will skip the field during JSON serialization
    CLEAR ls_abap_agent_info-freetextlabel.

    lo_util->convert_to_string( EXPORTING ir_abap          = REF #( ls_abap_agent_info )
                                          iv_compress      = abap_true " skip empty freetextlabel field
                                          it_name_mappings = VALUE #( ( abap = 'FREETEXTCONTENT'
                                                                        json = lv_free_text_label ) )
                                CHANGING  cr_string        = rv_agent_info ).
  ENDMETHOD.
ENDCLASS.
