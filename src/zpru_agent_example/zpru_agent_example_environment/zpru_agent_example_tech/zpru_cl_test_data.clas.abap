CLASS zpru_cl_test_data DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS refresh_test_data.
ENDCLASS.


CLASS zpru_cl_test_data IMPLEMENTATION.
  METHOD refresh_test_data.
    DATA lt_agent_serv      TYPE STANDARD TABLE OF zpru_agent_serv WITH EMPTY KEY.
    DATA lt_agent           TYPE STANDARD TABLE OF zpru_agent WITH EMPTY KEY.
    DATA lt_agent_tool      TYPE STANDARD TABLE OF zpru_agent_tool WITH EMPTY KEY.
    DATA lt_agent_type      TYPE STANDARD TABLE OF zpru_agent_type WITH EMPTY KEY.
    DATA lt_disc_strat      TYPE STANDARD TABLE OF zpru_disc_strat WITH EMPTY KEY.
    DATA lt_zpru_summ_strat TYPE STANDARD TABLE OF zpru_summ_strat WITH EMPTY KEY.



    lt_agent_serv = VALUE #( created_by = ''
                             created_at = '0.0000000'
                             changed_by = ''
                             ( service            = 'ZPRU_IF_MSUM_SERVICE'
                               context            = 'STANDARD_PERSISTENCE_MESSAGE'
                               class              = 'ZPRU_CL_MSUM_SERVICE'
                               last_changed       = '20260125095408.0057390'
                               local_last_changed = '20260125095408.0057390' )
                             ( service            = 'ZPRU_IF_MSUM_PRECHECK'
                               context            = 'STANDARD_PERSISTENCE_MESSAGE'
                               class              = 'ZPRU_CL_MSUM_PRECHECK'
                               last_changed       = '20260125095408.0057390'
                               local_last_changed = '20260125095408.0057390' )
                             ( service            = 'ZPRU_IF_MMSG_SERVICE'
                               context            = 'STANDARD_PERSISTENCE_MESSAGE'
                               class              = 'ZPRU_CL_MMSG_SERVICE'
                               last_changed       = '20260125095408.0057390'
                               local_last_changed = '20260125095408.0057390' )
                             ( service            = 'ZPRU_IF_MMSG_PRECHECK'
                               context            = 'STANDARD_PERSISTENCE_MESSAGE'
                               class              = 'ZPRU_CL_MMSG_PRECHECK'
                               last_changed       = '20260125095408.0057390'
                               local_last_changed = '20260125095408.0057390' )
                             ( service            = 'ZPRU_IF_AGSRV_SERVICE'
                               context            = 'STANDARD'
                               class              = 'ZPRU_CL_AGSRV_SERVICE'
                               last_changed       = '20260125095408.0057390'
                               local_last_changed = '20260125095408.0057390' )
                             ( service            = 'ZPRU_IF_AGSRV_PRECHECK'
                               context            = 'STANDARD'
                               class              = 'ZPRU_CL_AGSRV_PRECHECK'
                               last_changed       = '20260125095408.0057390'
                               local_last_changed = '20260125095408.0057390' )
                             ( service            = 'ZPRU_IF_ADF_VALIDATOR'
                               context            = 'STANDARD'
                               class              = 'ZPRU_CL_ADF_VALIDATOR'
                               last_changed       = '20260125095408.0057390'
                               local_last_changed = '20260125095408.0057390' )
                             ( service            = 'ZPRU_IF_AGTY_PRECHECK'
                               context            = 'STANDARD_AGENT_DEFINITION'
                               class              = 'ZPRU_CL_AGTY_PRECHECK'
                               last_changed       = '20260125095408.0057390'
                               local_last_changed = '20260125095408.0057390' )
                             ( service            = 'ZPRU_IF_AGTY_SERVICE'
                               context            = 'STANDARD'
                               class              = 'ZPRU_CL_AGTY_SERVICE'
                               last_changed       = '20260125095408.0057390'
                               local_last_changed = '20260125095408.0057390' )
                             ( service            = 'ZPRU_IF_UNIT_AGENT'
                               context            = 'STANDARD'
                               class              = 'ZPRU_CL_UNIT_AGENT'
                               last_changed       = '20260117150518.3887310'
                               local_last_changed = '20260117150518.3887310'  )
                             ( service            = 'ZPRU_IF_API_AGENT'
                               context            = 'STANDARD'
                               class              = 'ZPRU_CL_API_AGENT'
                               last_changed       = '20260117150619.0368210'
                               local_last_changed = '20260117150619.0368210'  )
                             ( service            = 'ZPRU_IF_AGENT_CONTROLLER'
                               context            = 'STANDARD'
                               class              = 'ZPRU_CL_AGENT_CONTROLLER'
                               last_changed       = '20260117150738.0236420'
                               local_last_changed = '20260117150738.0236420' )
                             ( service            = 'ZPRU_IF_AGENT_UTIL'
                               context            = 'STANDARD'
                               class              = 'ZPRU_CL_AGENT_UTIL'
                               last_changed       = '20260117150935.4567820'
                               local_last_changed = '20260117150935.4567820'  )
                             ( service            = 'ZPRU_IF_PAYLOAD'
                               context            = 'STANDARD'
                               class              = 'ZPRU_CL_PAYLOAD'
                               last_changed       = '20260117151018.7600370'
                               local_last_changed = '20260117151018.7600370'  )
                             ( service            = 'ZPRU_IF_ADF_SERVICE'
                               context            = 'STANDARD'
                               class              = 'ZPRU_CL_ADF_SERVICE'
                               last_changed       = '20260117151237.1192170'
                               local_last_changed = '20260117151237.1192170'  )
                             ( service            = 'ZPRU_IF_DUMMY_AGENT_LOGIC'
                               context            = 'STANDARD'
                               class              = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                               last_changed       = '20260117152144.6419870'
                               local_last_changed = '20260117152144.6419870' )
                             ( service            = 'IF_AIC_COMPLETION_API'
                               context            = 'STANDARD'
                               class              = 'ZPRU_CL_AIC_COMPLETION_API'
                               last_changed       = '20260117152251.0551680'
                               local_last_changed = '20260117152251.0551680'  )
                             ( service            = 'IF_AIC_COMPLETION_API_RESULT'
                               context            = 'STANDARD'
                               class              = 'ZPRU_CL_AIC_COMPLETION_API_RES'
                               last_changed       = '20260117152343.0309600'
                               local_last_changed = '20260117152343.0309600'  )
                             ( service            = 'IF_AIC_COMPLETION_PARAMETERS'
                               context            = 'STANDARD'
                               class              = 'ZPRU_CL_AIC_COMPLETION_PARAM'
                               last_changed       = '20260117152423.9709400'
                               local_last_changed = '20260117152423.9709400'  )
                             ( service            = 'IF_AIC_MESSAGE_CONTAINER'
                               context            = 'STANDARD'
                               class              = 'ZPRU_CL_AIC_MESSAGE_CONTAINER'
                               last_changed       = '20260117152547.0539920'
                               local_last_changed = '20260117152547.0539920'  )
                             ( service            = 'IF_AIC_ISLM_COMPL_API_FACTORY'
                               context            = 'STANDARD'
                               class              = 'ZPRU_CL_ISLM_COMPL_API_FACTORY'
                               last_changed       = '20260117152659.4183900'
                               local_last_changed = '20260117152659.4183900'  )
                             ( service            = 'IF_WEB_HTTP_CLIENT'
                               context            = 'STANDARD'
                               class              = 'ZPRU_CL_WEB_HTTP_CLIENT'
                               last_changed       = '20260117152753.5715620'
                               local_last_changed = '20260117152753.5715620'  )
                             ( service            = 'IF_WEB_HTTP_REQUEST'
                               context            = 'STANDARD'
                               class              = 'ZPRU_CL_WEB_HTTP_REQUEST'
                               last_changed       = '20260117152903.9660210'
                               local_last_changed = '20260117152903.9660210'  )
                             ( service            = 'IF_WEB_HTTP_RESPONSE'
                               context            = 'STANDARD'
                               class              = 'ZPRU_CL_WEB_HTTP_RESPONSE'
                               last_changed       = '20260117153008.0378680'
                               local_last_changed = '20260117153008.0378680'  )
                             ( service            = 'ZPRU_IF_AXC_SERVICE'
                               context            = 'STANDARD'
                               class              = 'ZPRU_CL_AXC_SERVICE'
                               last_changed       = '20260117153230.0427800'
                               local_last_changed = '20260117153230.0427800'  )
                             ( service            = 'ZPRU_IF_LONG_MEMORY_PROVIDER'
                               context            = 'STANDARD'
                               class              = 'ZPRU_CL_LONG_MEMORY_BASE'
                               last_changed       = '20260117153709.0139100'
                               local_last_changed = '20260117153709.0139100'  )
                             ( service            = 'ZPRU_IF_LONG_MEM_PERSISTENCE'
                               context            = 'STANDARD_PERSISTENCE_MESSAGE'
                               class              = 'ZPRU_CL_PERSISTENCE_MSG'
                               last_changed       = '20260117154416.9645010'
                               local_last_changed = '20260117154416.9645010'  )
                             ( service            = 'ZPRU_IF_LONG_MEM_PERSISTENCE'
                               context            = 'STANDARD_PERSISTENCE_SUMMARIZE'
                               class              = 'ZPRU_CL_PERSISTENCE_SUM'
                               last_changed       = '20260117154604.0443640'
                               local_last_changed = '20260117154604.0443640'  )
                             ( service            = 'ZPRU_IF_SUMMARIZATION'
                               context            = 'STANDARD_SUMMARIZE'
                               class              = 'ZPRU_CL_PERSISTENCE_SUM'
                               last_changed       = '20260117154729.8684840'
                               local_last_changed = '20260117154729.8684840'  )
                             ( service            = 'ZPRU_IF_SHORT_MEMORY_PROVIDER'
                               context            = 'STANDARD'
                               class              = 'ZPRU_CL_SHORT_MEMORY_BASE'
                               last_changed       = '20260117154917.7603470'
                               local_last_changed = '20260117154917.7603470'  )
                             ( service            = 'ZPRU_IF_DISCARD_STRATEGY'
                               context            = 'STANDARD_DISCARD_STRATEGY_DELETE'
                               class              = 'ZPRU_CL_DISCARD_DELETE'
                               last_changed       = '20260117155051.9175870'
                               local_last_changed = '20260117155051.9175870'  )
                             ( service            = 'ZPRU_IF_DISCARD_STRATEGY'
                               context            = 'STANDARD_DISCARD_STRATEGY_SAVE'
                               class              = 'ZPRU_CL_DISCARD_SAVE'
                               last_changed       = '20260117155223.7925380'
                               local_last_changed = '20260117155223.7925380'  )
                             ( service            = 'ZPRU_IF_DISCARD_STRATEGY'
                               context            = 'STANDARD_DISCARD_STRATEGY_SUMMARIZE'
                               class              = 'ZPRU_CL_DISCARD_SUMMARIZE'
                               last_changed       = '20260117155325.9377550'
                               local_last_changed = '20260117155325.9377550'  )
                             ( service            = 'ZPRU_IF_AXC_PRECHECK'
                               context            = 'STANDARD_AGENT_EXECUTION'
                               class              = 'ZPRU_CL_AXC_PRECHECK'
                               last_changed       = '20260118160211.0649630'
                               local_last_changed = '20260118160211.0649630'  )
                             ( service            = 'ZPRU_IF_ADF_PRECHECK'
                               context            = 'STANDARD_AGENT_DEFINITION'
                               class              = 'ZPRU_CL_ADF_PRECHECK'
                               last_changed       = '20260118160851.3804580'
                               local_last_changed = '20260118160851.3804580'  ) ).

    lt_agent = VALUE #( agent_type = 'AGTYP1'
                        status     = 'N'
                        created_by = ''
                        created_at = '0.0000000'
                        changed_by = ''
                        ( agent_uuid             = 'EE3D176152061FD0BE937056641C11CB'
                          agent_name             = 'NESTED_AGENT'
                          decision_provider      = 'ZPRU_CL_NESTED_DECISION'
                          short_memory_provider  = 'ZPRU_CL_NESTED_SHORT_MEMORY'
                          long_memory_provider   = 'ZPRU_CL_NESTED_LONG_MEMORY'
                          agent_info_provider    = 'ZPRU_CL_NESTED_AGENT_INFO'
                          system_prompt_provider = 'ZPRU_CL_NESTED_SYSTEM_PROMPT'
                          last_changed           = '20260123211537.8743820'
                          local_last_changed     = '20260123204019.7685060'  )
                        ( agent_uuid             = 'CA8C3B6EC0741FE0B8D32070B3735FCE'
                          agent_name             = 'DUMMY_AGENT'
                          decision_provider      = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                          short_memory_provider  = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                          long_memory_provider   = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                          agent_info_provider    = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                          system_prompt_provider = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                          last_changed           = '20260123212025.9435130'
                          local_last_changed     = '20251228170556.0725850'  ) ).

    lt_agent_tool = VALUE #( ( tool_uuid            = 'CA8C3B6EC0741FE0B8D33D039743DFCE'
                               agent_uuid           = 'CA8C3B6EC0741FE0B8D32070B3735FCE'
                               tool_name            = 'NESTED_AGENT'
                               tool_provider        = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                               step_type            = 'A'
                               tool_schema_provider = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                               tool_info_provider   = 'ZPRU_CL_DUMMY_AGENT_LOGIC'  )
                             ( tool_uuid            = '7E5BF75F6D161FD0BE937A03A6668E66'
                               agent_uuid           = 'EE3D176152061FD0BE937056641C11CB'
                               tool_name            = 'NESTED_ABAP'
                               tool_provider        = 'ZPRU_CL_NESTED_CODE'
                               step_type            = 'B'
                               tool_schema_provider = 'ZPRU_CL_NESTED_CODE_SCHM_PRVDR'
                               tool_info_provider   = 'ZPRU_CL_NESTED_CODE_INFO_PRVDR'  )
                             ( tool_uuid            = '7E5BF75F6D161FD0BE941150AE55EE66'
                               agent_uuid           = 'EE3D176152061FD0BE937056641C11CB'
                               tool_name            = 'NESTED_LLM'
                               tool_provider        = 'ZPRU_CL_NESTED_LLM'
                               step_type            = 'L'
                               tool_schema_provider = 'ZPRU_CL_NESTED_LLM_SCHM_PRVDR'
                               tool_info_provider   = 'ZPRU_CL_NESTED_LLM_INFO_PRVDR'  )
                             ( tool_uuid            = '7E5BF75F6D161FD0BE9409DD3B9DAE66'
                               agent_uuid           = 'EE3D176152061FD0BE937056641C11CB'
                               tool_name            = 'NESTED_HTTP'
                               tool_provider        = 'ZPRU_CL_NESTED_HTTP'
                               step_type            = 'H'
                               tool_schema_provider = 'ZPRU_CL_NESTED_HTTP_SCHM_PRVDR'
                               tool_info_provider   = 'ZPRU_CL_NESTED_HTTP_INFO_PRVDR'  )
                             ( tool_uuid            = 'EE3D176152061FD0BE941A5B889C51CB'
                               agent_uuid           = 'CA8C3B6EC0741FE0B8D32070B3735FCE'
                               tool_name            = 'DUMMY_CODE'
                               tool_provider        = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                               step_type            = 'B'
                               tool_schema_provider = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                               tool_info_provider   = 'ZPRU_CL_DUMMY_AGENT_LOGIC'  )
                             ( tool_uuid            = '7E5BF75F6D161FD0BE94291281BEEE66'
                               agent_uuid           = 'CA8C3B6EC0741FE0B8D32070B3735FCE'
                               tool_name            = 'DUMMY_USER_TOOL'
                               tool_provider        = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                               step_type            = 'Z'
                               tool_schema_provider = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                               tool_info_provider   = 'ZPRU_CL_DUMMY_AGENT_LOGIC'  )
                             ( tool_uuid            = '7E5BF75F6D161FD0BE9427D045730E66'
                               agent_uuid           = 'CA8C3B6EC0741FE0B8D32070B3735FCE'
                               tool_name            = 'DUMMY_ML'
                               tool_provider        = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                               step_type            = 'M'
                               tool_schema_provider = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                               tool_info_provider   = 'ZPRU_CL_DUMMY_AGENT_LOGIC'  )
                             ( tool_uuid            = '7E5BF75F6D161FD0BE9425631F734E66'
                               agent_uuid           = 'CA8C3B6EC0741FE0B8D32070B3735FCE'
                               tool_name            = 'DUMMY_DYN_CODE'
                               tool_provider        = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                               step_type            = 'D'
                               tool_schema_provider = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                               tool_info_provider   = 'ZPRU_CL_DUMMY_AGENT_LOGIC'  )
                             ( tool_uuid            = '7E5BF75F6D161FD0BE94223960C56E66'
                               agent_uuid           = 'CA8C3B6EC0741FE0B8D32070B3735FCE'
                               tool_name            = 'DUMMY_SCM'
                               tool_provider        = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                               step_type            = 'S'
                               tool_schema_provider = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                               tool_info_provider   = 'ZPRU_CL_DUMMY_AGENT_LOGIC'  )
                             ( tool_uuid            = 'EE3D176152061FD0BE9420963166D1CB'
                               agent_uuid           = 'CA8C3B6EC0741FE0B8D32070B3735FCE'
                               tool_name            = 'DUMMY_HTTP'
                               tool_provider        = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                               step_type            = 'H'
                               tool_schema_provider = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                               tool_info_provider   = 'ZPRU_CL_DUMMY_AGENT_LOGIC'  )
                             ( tool_uuid            = 'EE3D176152061FD0BE941DB3EE6831CB'
                               agent_uuid           = 'CA8C3B6EC0741FE0B8D32070B3735FCE'
                               tool_name            = 'DUMMY_KNOWLEDGE'
                               tool_provider        = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                               step_type            = 'K'
                               tool_schema_provider = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                               tool_info_provider   = 'ZPRU_CL_DUMMY_AGENT_LOGIC'  ) ).

    lt_agent_type = VALUE #( ( agent_type         = 'AGTYP1'
                               short_mem_volume   = '8'
                               discard_strategy   = 'SUM1'
                               summary_strategy   = 'SUM1'
                               max_numb_loop      = '3'
                               created_by         = ''
                               created_at         = '0.0000000'
                               changed_by         = ''
                               last_changed       = '20251228170454.7391400'
                               local_last_changed = '20251228170454.7391400'  ) ).

    lt_disc_strat = VALUE #( created_by = ''
                             created_at = '0.0000000'
                             changed_by = ''
                             ( discard_strategy   = 'SAV1'
                               strategy_provider  = 'ZPRU_CL_DISCARD_SAVE'
                               last_changed       = '20251228160900.9861430'
                               local_last_changed = '20251228160900.9861430'  )
                             ( discard_strategy   = 'SUM1'
                               strategy_provider  = 'ZPRU_CL_DISCARD_SUMMARIZE'
                               last_changed       = '20251228160923.7645650'
                               local_last_changed = '20251228160923.7645650'  )
                             ( discard_strategy   = 'DEL1'
                               strategy_provider  = 'ZPRU_CL_DISCARD_DELETE'
                               last_changed       = '20251228160933.6209440'
                               local_last_changed = '20251228160933.6209440'  ) ).

    lt_zpru_summ_strat = VALUE #( ( summary_strategy   = 'SUM1'
                                    strategy_provider  = 'ZPRU_CL_SUMMARIZE_SIMPLE'
                                    created_by         = ''
                                    created_at         = '0.0000000'
                                    changed_by         = ''
                                    last_changed       = '20251228162302.4041410'
                                    local_last_changed = '20251228162302.4041410'  ) ).

    DELETE zpru_agent_serv FROM TABLE @lt_agent_serv.
    IF sy-subrc <> 0.
      ROLLBACK WORK.
      RETURN.
    ENDIF.
    DELETE zpru_agent FROM TABLE @lt_agent.
    IF sy-subrc <> 0.
      ROLLBACK WORK.
      RETURN.
    ENDIF.
    DELETE zpru_agent_tool FROM TABLE @lt_agent_tool.
    IF sy-subrc <> 0.
      ROLLBACK WORK.
      RETURN.
    ENDIF.
    DELETE zpru_agent_type FROM TABLE @lt_agent_type.
    IF sy-subrc <> 0.
      ROLLBACK WORK.
      RETURN.
    ENDIF.
    DELETE zpru_disc_strat FROM TABLE @lt_disc_strat.
    IF sy-subrc <> 0.
      ROLLBACK WORK.
      RETURN.
    ENDIF.
    DELETE zpru_summ_strat FROM TABLE @lt_zpru_summ_strat.
    IF sy-subrc <> 0.
      ROLLBACK WORK.
      RETURN.
    ENDIF.

    MODIFY zpru_agent_serv FROM TABLE @lt_agent_serv.
    IF sy-subrc <> 0.
      ROLLBACK WORK.
      RETURN.
    ENDIF.
    MODIFY zpru_agent FROM TABLE @lt_agent.
    IF sy-subrc <> 0.
      ROLLBACK WORK.
      RETURN.
    ENDIF.
    MODIFY zpru_agent_tool FROM TABLE @lt_agent_tool.
    IF sy-subrc <> 0.
      ROLLBACK WORK.
      RETURN.
    ENDIF.
    MODIFY zpru_agent_type FROM TABLE @lt_agent_type.
    IF sy-subrc <> 0.
      ROLLBACK WORK.
      RETURN.
    ENDIF.
    MODIFY zpru_disc_strat FROM TABLE @lt_disc_strat.
    IF sy-subrc <> 0.
      ROLLBACK WORK.
      RETURN.
    ENDIF.
    MODIFY zpru_summ_strat FROM TABLE @lt_zpru_summ_strat.
    IF sy-subrc <> 0.
      ROLLBACK WORK.
      RETURN.
    ENDIF.

    COMMIT WORK.
  ENDMETHOD.
ENDCLASS.
