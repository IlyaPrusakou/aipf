CLASS zpru_cl_syst_prmpt_prvdr_base DEFINITION
  PUBLIC ABSTRACT
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zpru_if_agent_frw.
    INTERFACES zpru_if_prompt_provider.

  PROTECTED SECTION.
    METHODS set_primary_session_task ABSTRACT
      IMPORTING iv_agent_uuid           TYPE sysuuid_x16
      EXPORTING ev_primary_session_task TYPE string.

    METHODS set_technical_rules ABSTRACT
      IMPORTING iv_agent_uuid        TYPE sysuuid_x16
      RETURNING VALUE(rt_tech_rules) TYPE zpru_tt_tech_rules.

    METHODS set_business_rules ABSTRACT
      IMPORTING iv_agent_uuid            TYPE sysuuid_x16
      RETURNING VALUE(rt_business_rules) TYPE zpru_tt_business_rules.

    METHODS set_format_guidelines ABSTRACT
      IMPORTING iv_agent_uuid               TYPE sysuuid_x16
      RETURNING VALUE(rt_format_guidelines) TYPE zpru_tt_format_guidelines.

    METHODS set_reasoning_step ABSTRACT
      IMPORTING iv_agent_uuid            TYPE sysuuid_x16
      RETURNING VALUE(rt_reasoning_step) TYPE zpru_tt_reasoning_step.

    METHODS set_prompt_restrictions ABSTRACT
      IMPORTING iv_agent_uuid                 TYPE sysuuid_x16
      RETURNING VALUE(rt_prompt_restrictions) TYPE zpru_tt_prompt_restrictions.

    METHODS set_arbitrary_text ABSTRACT
      IMPORTING iv_agent_uuid         TYPE sysuuid_x16
      EXPORTING ev_arbitrarytexttitle TYPE string
                ev_arbitrarytext      TYPE string.

  PRIVATE SECTION.
ENDCLASS.


CLASS zpru_cl_syst_prmpt_prvdr_base IMPLEMENTATION.
  METHOD zpru_if_prompt_provider~get_abap_system_prompt.
    DATA ls_system_prompt TYPE zpru_s_system_prompt.

    set_primary_session_task( EXPORTING iv_agent_uuid           = iv_agent_uuid
                              IMPORTING ev_primary_session_task = ls_system_prompt-primarysessiontask ).

    ls_system_prompt-technicalrules     = set_technical_rules( iv_agent_uuid = iv_agent_uuid ).
    ls_system_prompt-businessrules      = set_business_rules( iv_agent_uuid = iv_agent_uuid ).
    ls_system_prompt-formatguidelines   = set_format_guidelines( iv_agent_uuid = iv_agent_uuid ).
    ls_system_prompt-reasoningstep      = set_reasoning_step( iv_agent_uuid = iv_agent_uuid ).
    ls_system_prompt-promptrestrictions = set_prompt_restrictions( iv_agent_uuid = iv_agent_uuid ).

    set_arbitrary_text( EXPORTING iv_agent_uuid         = iv_agent_uuid
                        IMPORTING ev_arbitrarytexttitle = ls_system_prompt-arbitrarytexttitle
                                  ev_arbitrarytext      = ls_system_prompt-arbitrarytext ).

    rs_abap_system_prompt = ls_system_prompt.
  ENDMETHOD.

  METHOD zpru_if_prompt_provider~get_system_prompt.
    DATA lo_util TYPE REF TO zpru_if_agent_util.

    lo_util ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_AGENT_UTIL`
                                                        iv_context = zpru_if_agent_frw=>cs_context-standard ).

    DATA(ls_abap_system_prompt) = zpru_if_prompt_provider~get_abap_system_prompt( iv_agent_uuid = iv_agent_uuid ).

    DATA(lv_arbitrary_text_title) = ls_abap_system_prompt-arbitrarytexttitle.

    " we will skip the field during JSON serialization
    CLEAR ls_abap_system_prompt-arbitrarytexttitle.

    lo_util->convert_to_string( EXPORTING ir_abap          = REF #( ls_abap_system_prompt )
                                          iv_compress      = abap_true " skip empty arbitrarytexttitle field
                                          it_name_mappings = VALUE #( ( abap = 'ARBITRARYTEXT'
                                                                        json = lv_arbitrary_text_title ) )
                                CHANGING  cr_string        = rv_system_prompt ).
  ENDMETHOD.
ENDCLASS.
