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



    lt_agent_serv = VALUE #( createdby = ''
                             createdat = '0.0000000'
                             changedby = ''
                             ( service            = 'ZPRU_IF_MSUM_SERVICE'
                               context            = 'STANDARD_PERSISTENCE_MESSAGE'
                               class              = 'ZPRU_CL_MSUM_SERVICE'
                               lastchanged       = '20260125095408.0057390'
                               locallastchanged = '20260125095408.0057390' )
                             ( service            = 'ZPRU_IF_MSUM_PRECHECK'
                               context            = 'STANDARD_PERSISTENCE_MESSAGE'
                               class              = 'ZPRU_CL_MSUM_PRECHECK'
                               lastchanged       = '20260125095408.0057390'
                               locallastchanged = '20260125095408.0057390' )
                             ( service            = 'ZPRU_IF_MMSG_SERVICE'
                               context            = 'STANDARD_PERSISTENCE_MESSAGE'
                               class              = 'ZPRU_CL_MMSG_SERVICE'
                               lastchanged       = '20260125095408.0057390'
                               locallastchanged = '20260125095408.0057390' )
                             ( service            = 'ZPRU_IF_MMSG_PRECHECK'
                               context            = 'STANDARD_PERSISTENCE_MESSAGE'
                               class              = 'ZPRU_CL_MMSG_PRECHECK'
                               lastchanged       = '20260125095408.0057390'
                               locallastchanged = '20260125095408.0057390' )
                             ( service            = 'ZPRU_IF_AGSRV_SERVICE'
                               context            = 'STANDARD'
                               class              = 'ZPRU_CL_AGSRV_SERVICE'
                               lastchanged       = '20260125095408.0057390'
                               locallastchanged = '20260125095408.0057390' )
                             ( service            = 'ZPRU_IF_AGSRV_PRECHECK'
                               context            = 'STANDARD'
                               class              = 'ZPRU_CL_AGSRV_PRECHECK'
                               lastchanged       = '20260125095408.0057390'
                               locallastchanged = '20260125095408.0057390' )
                             ( service            = 'ZPRU_IF_ADF_VALIDATOR'
                               context            = 'STANDARD'
                               class              = 'ZPRU_CL_ADF_VALIDATOR'
                               lastchanged       = '20260125095408.0057390'
                               locallastchanged = '20260125095408.0057390' )
                             ( service            = 'ZPRU_IF_AGTY_PRECHECK'
                               context            = 'STANDARD_AGENT_DEFINITION'
                               class              = 'ZPRU_CL_AGTY_PRECHECK'
                               lastchanged       = '20260125095408.0057390'
                               locallastchanged = '20260125095408.0057390' )
                             ( service            = 'ZPRU_IF_AGTY_SERVICE'
                               context            = 'STANDARD'
                               class              = 'ZPRU_CL_AGTY_SERVICE'
                               lastchanged       = '20260125095408.0057390'
                               locallastchanged = '20260125095408.0057390' )
                             ( service            = 'ZPRU_IF_UNIT_AGENT'
                               context            = 'STANDARD'
                               class              = 'ZPRU_CL_UNIT_AGENT'
                               lastchanged       = '20260117150518.3887310'
                               locallastchanged = '20260117150518.3887310'  )
                             ( service            = 'ZPRU_IF_API_AGENT'
                               context            = 'STANDARD'
                               class              = 'ZPRU_CL_API_AGENT'
                               lastchanged       = '20260117150619.0368210'
                               locallastchanged = '20260117150619.0368210'  )
                             ( service            = 'ZPRU_IF_AGENT_CONTROLLER'
                               context            = 'STANDARD'
                               class              = 'ZPRU_CL_AGENT_CONTROLLER'
                               lastchanged       = '20260117150738.0236420'
                               locallastchanged = '20260117150738.0236420' )
                             ( service            = 'ZPRU_IF_AGENT_UTIL'
                               context            = 'STANDARD'
                               class              = 'ZPRU_CL_AGENT_UTIL'
                               lastchanged       = '20260117150935.4567820'
                               locallastchanged = '20260117150935.4567820'  )
                             ( service            = 'ZPRU_IF_PAYLOAD'
                               context            = 'STANDARD'
                               class              = 'ZPRU_CL_PAYLOAD'
                               lastchanged       = '20260117151018.7600370'
                               locallastchanged = '20260117151018.7600370'  )
                             ( service            = 'ZPRU_IF_ADF_SERVICE'
                               context            = 'STANDARD'
                               class              = 'ZPRU_CL_ADF_SERVICE'
                               lastchanged       = '20260117151237.1192170'
                               locallastchanged = '20260117151237.1192170'  )
                             ( service            = 'ZPRU_IF_DUMMY_AGENT_LOGIC'
                               context            = 'STANDARD'
                               class              = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                               lastchanged       = '20260117152144.6419870'
                               locallastchanged = '20260117152144.6419870' )
                             ( service            = 'IF_AIC_COMPLETION_API'
                               context            = 'STANDARD'
                               class              = 'ZPRU_CL_AIC_COMPLETION_API'
                               lastchanged       = '20260117152251.0551680'
                               locallastchanged = '20260117152251.0551680'  )
                             ( service            = 'IF_AIC_COMPLETION_API_RESULT'
                               context            = 'STANDARD'
                               class              = 'ZPRU_CL_AIC_COMPLETION_API_RES'
                               lastchanged       = '20260117152343.0309600'
                               locallastchanged = '20260117152343.0309600'  )
                             ( service            = 'IF_AIC_COMPLETION_PARAMETERS'
                               context            = 'STANDARD'
                               class              = 'ZPRU_CL_AIC_COMPLETION_PARAM'
                               lastchanged       = '20260117152423.9709400'
                               locallastchanged = '20260117152423.9709400'  )
                             ( service            = 'IF_AIC_MESSAGE_CONTAINER'
                               context            = 'STANDARD'
                               class              = 'ZPRU_CL_AIC_MESSAGE_CONTAINER'
                               lastchanged       = '20260117152547.0539920'
                               locallastchanged = '20260117152547.0539920'  )
                             ( service            = 'IF_AIC_ISLM_COMPL_API_FACTORY'
                               context            = 'STANDARD'
                               class              = 'ZPRU_CL_ISLM_COMPL_API_FACTORY'
                               lastchanged       = '20260117152659.4183900'
                               locallastchanged = '20260117152659.4183900'  )
                             ( service            = 'IF_WEB_HTTP_CLIENT'
                               context            = 'STANDARD'
                               class              = 'ZPRU_CL_WEB_HTTP_CLIENT'
                               lastchanged       = '20260117152753.5715620'
                               locallastchanged = '20260117152753.5715620'  )
                             ( service            = 'IF_WEB_HTTP_REQUEST'
                               context            = 'STANDARD'
                               class              = 'ZPRU_CL_WEB_HTTP_REQUEST'
                               lastchanged       = '20260117152903.9660210'
                               locallastchanged = '20260117152903.9660210'  )
                             ( service            = 'IF_WEB_HTTP_RESPONSE'
                               context            = 'STANDARD'
                               class              = 'ZPRU_CL_WEB_HTTP_RESPONSE'
                               lastchanged       = '20260117153008.0378680'
                               locallastchanged = '20260117153008.0378680'  )
                             ( service            = 'ZPRU_IF_AXC_SERVICE'
                               context            = 'STANDARD'
                               class              = 'ZPRU_CL_AXC_SERVICE'
                               lastchanged       = '20260117153230.0427800'
                               locallastchanged = '20260117153230.0427800'  )
                             ( service            = 'ZPRU_IF_LONG_MEMORY_PROVIDER'
                               context            = 'STANDARD'
                               class              = 'ZPRU_CL_LONG_MEMORY_BASE'
                               lastchanged       = '20260117153709.0139100'
                               locallastchanged = '20260117153709.0139100'  )
                             ( service            = 'ZPRU_IF_LONG_MEM_PERSISTENCE'
                               context            = 'STANDARD_PERSISTENCE_MESSAGE'
                               class              = 'ZPRU_CL_PERSISTENCE_MSG'
                               lastchanged       = '20260117154416.9645010'
                               locallastchanged = '20260117154416.9645010'  )
                             ( service            = 'ZPRU_IF_LONG_MEM_PERSISTENCE'
                               context            = 'STANDARD_PERSISTENCE_SUMMARIZE'
                               class              = 'ZPRU_CL_PERSISTENCE_SUM'
                               lastchanged       = '20260117154604.0443640'
                               locallastchanged = '20260117154604.0443640'  )
                             ( service            = 'ZPRU_IF_SUMMARIZATION'
                               context            = 'STANDARD_SUMMARIZE'
                               class              = 'ZPRU_CL_PERSISTENCE_SUM'
                               lastchanged       = '20260117154729.8684840'
                               locallastchanged = '20260117154729.8684840'  )
                             ( service            = 'ZPRU_IF_SHORT_MEMORY_PROVIDER'
                               context            = 'STANDARD'
                               class              = 'ZPRU_CL_SHORT_MEMORY_BASE'
                               lastchanged       = '20260117154917.7603470'
                               locallastchanged = '20260117154917.7603470'  )
                             ( service            = 'ZPRU_IF_DISCARD_STRATEGY'
                               context            = 'STANDARD_DISCARD_STRATEGY_DELETE'
                               class              = 'ZPRU_CL_DISCARD_DELETE'
                               lastchanged       = '20260117155051.9175870'
                               locallastchanged = '20260117155051.9175870'  )
                             ( service            = 'ZPRU_IF_DISCARD_STRATEGY'
                               context            = 'STANDARD_DISCARD_STRATEGY_SAVE'
                               class              = 'ZPRU_CL_DISCARD_SAVE'
                               lastchanged       = '20260117155223.7925380'
                               locallastchanged = '20260117155223.7925380'  )
                             ( service            = 'ZPRU_IF_DISCARD_STRATEGY'
                               context            = 'STANDARD_DISCARD_STRATEGY_SUMMARIZE'
                               class              = 'ZPRU_CL_DISCARD_SUMMARIZE'
                               lastchanged       = '20260117155325.9377550'
                               locallastchanged = '20260117155325.9377550'  )
                             ( service            = 'ZPRU_IF_AXC_PRECHECK'
                               context            = 'STANDARD_AGENT_EXECUTION'
                               class              = 'ZPRU_CL_AXC_PRECHECK'
                               lastchanged       = '20260118160211.0649630'
                               locallastchanged = '20260118160211.0649630'  )
                             ( service            = 'ZPRU_IF_ADF_PRECHECK'
                               context            = 'STANDARD_AGENT_DEFINITION'
                               class              = 'ZPRU_CL_ADF_PRECHECK'
                               lastchanged       = '20260118160851.3804580'
                               locallastchanged = '20260118160851.3804580'  ) ).

    TRY.
        DATA(lv_agent_uuid1) = cl_system_uuid=>create_uuid_x16_static( ).
        DATA(lv_agent_uuid2) = cl_system_uuid=>create_uuid_x16_static( ).
      CATCH cx_uuid_error.
    ENDTRY.

    lt_agent = VALUE #( agenttype = 'AGTYP1'
                        status     = 'N'
                        createdby = ''
                        createdat = '0.0000000'
                        changedby = ''
                        ( agentuuid             = lv_agent_uuid1
                          agentname             = 'NESTED_AGENT'
                          decisionprovider      = 'ZPRU_CL_NESTED_DECISION'
                          shortmemoryprovider  = 'ZPRU_CL_NESTED_SHORT_MEMORY'
                          longmemoryprovider   = 'ZPRU_CL_NESTED_LONG_MEMORY'
                          agentinfoprovider    = 'ZPRU_CL_NESTED_AGENT_INFO'
                          systempromptprovider = 'ZPRU_CL_NESTED_SYSTEM_PROMPT'
                          lastchanged           = '20260123211537.8743820'
                          locallastchanged     = '20260123204019.7685060'  )
                        ( agentuuid             = lv_agent_uuid2
                          agentname             = 'DUMMY_AGENT'
                          decisionprovider      = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                          shortmemoryprovider  = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                          longmemoryprovider   = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                          agentinfoprovider    = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                          systempromptprovider = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                          lastchanged           = '20260123212025.9435130'
                          locallastchanged     = '20251228170556.0725850'  ) ).
    TRY.
        lt_agent_tool = VALUE #( ( tooluuid            = cl_system_uuid=>create_uuid_x16_static( )
                                   agentuuid           = lv_agent_uuid2 "2
                                   toolname            = 'NESTED_AGENT'
                                   toolprovider        = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                                   steptype            = 'A'
                                   toolschemaprovider = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                                   toolinfoprovider   = 'ZPRU_CL_DUMMY_AGENT_LOGIC'  )
                                 ( tooluuid            = cl_system_uuid=>create_uuid_x16_static( )
                                   agentuuid           = lv_agent_uuid1 "1
                                   toolname            = 'NESTED_ABAP'
                                   toolprovider        = 'ZPRU_CL_NESTED_CODE'
                                   steptype            = 'B'
                                   toolschemaprovider = 'ZPRU_CL_NESTED_CODE_SCHM_PRVDR'
                                   toolinfoprovider   = 'ZPRU_CL_NESTED_CODE_INFO_PRVDR'  )
                                 ( tooluuid            = cl_system_uuid=>create_uuid_x16_static( )
                                   agentuuid           = lv_agent_uuid1 "1
                                   toolname            = 'NESTED_LLM'
                                   toolprovider        = 'ZPRU_CL_NESTED_LLM'
                                   steptype            = 'L'
                                   toolschemaprovider = 'ZPRU_CL_NESTED_LLM_SCHM_PRVDR'
                                   toolinfoprovider   = 'ZPRU_CL_NESTED_LLM_INFO_PRVDR'  )
                                 ( tooluuid            = cl_system_uuid=>create_uuid_x16_static( )
                                   agentuuid           = lv_agent_uuid1 "1
                                   toolname            = 'NESTED_HTTP'
                                   toolprovider        = 'ZPRU_CL_NESTED_HTTP'
                                   steptype            = 'H'
                                   toolschemaprovider = 'ZPRU_CL_NESTED_HTTP_SCHM_PRVDR'
                                   toolinfoprovider   = 'ZPRU_CL_NESTED_HTTP_INFO_PRVDR'  )
                                 ( tooluuid            = cl_system_uuid=>create_uuid_x16_static( )
                                   agentuuid           = lv_agent_uuid2 "2
                                   toolname            = 'DUMMY_CODE'
                                   toolprovider        = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                                   steptype            = 'B'
                                   toolschemaprovider = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                                   toolinfoprovider   = 'ZPRU_CL_DUMMY_AGENT_LOGIC'  )
                                 ( tooluuid            = cl_system_uuid=>create_uuid_x16_static( )
                                   agentuuid           = lv_agent_uuid2 "2
                                   toolname            = 'DUMMY_USER_TOOL'
                                   toolprovider        = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                                   steptype            = 'Z'
                                   toolschemaprovider = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                                   toolinfoprovider   = 'ZPRU_CL_DUMMY_AGENT_LOGIC'  )
                                 ( tooluuid            = cl_system_uuid=>create_uuid_x16_static( )
                                   agentuuid           = lv_agent_uuid2 "2
                                   toolname            = 'DUMMY_ML'
                                   toolprovider        = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                                   steptype            = 'M'
                                   toolschemaprovider = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                                   toolinfoprovider   = 'ZPRU_CL_DUMMY_AGENT_LOGIC'  )
                                 ( tooluuid            = cl_system_uuid=>create_uuid_x16_static( )
                                   agentuuid           = lv_agent_uuid2 "2
                                   toolname            = 'DUMMY_DYN_CODE'
                                   toolprovider        = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                                   steptype            = 'D'
                                   toolschemaprovider = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                                   toolinfoprovider   = 'ZPRU_CL_DUMMY_AGENT_LOGIC'  )
                                 ( tooluuid            = cl_system_uuid=>create_uuid_x16_static( )
                                   agentuuid           = lv_agent_uuid2 "2
                                   toolname            = 'DUMMY_SCM'
                                   toolprovider        = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                                   steptype            = 'S'
                                   toolschemaprovider = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                                   toolinfoprovider   = 'ZPRU_CL_DUMMY_AGENT_LOGIC'  )
                                 ( tooluuid            = cl_system_uuid=>create_uuid_x16_static( )
                                   agentuuid           = lv_agent_uuid2 "2
                                   toolname            = 'DUMMY_HTTP'
                                   toolprovider        = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                                   steptype            = 'H'
                                   toolschemaprovider = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                                   toolinfoprovider   = 'ZPRU_CL_DUMMY_AGENT_LOGIC'  )
                                 ( tooluuid            = cl_system_uuid=>create_uuid_x16_static( )
                                   agentuuid           = lv_agent_uuid2 "2
                                   toolname            = 'DUMMY_KNOWLEDGE'
                                   toolprovider        = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                                   steptype            = 'K'
                                   toolschemaprovider = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                                   toolinfoprovider   = 'ZPRU_CL_DUMMY_AGENT_LOGIC'  ) ).
      CATCH cx_uuid_error.
    ENDTRY.


    lt_agent_type = VALUE #( ( agenttype         = 'AGTYP1'
                               shortmemvolume   = '8'
                               discardstrategy   = 'SUM1'
                               summarystrategy   = 'SUM1'
                               maxnumbloop      = '3'
                               createdby         = ''
                               createdat         = '0.0000000'
                               changedby         = ''
                               lastchanged       = '20251228170454.7391400'
                               locallastchanged = '20251228170454.7391400'  ) ).

    lt_disc_strat = VALUE #( createdby = ''
                             createdat = '0.0000000'
                             changedby = ''
                             ( discardstrategy   = 'SAV1'
                               strategyprovider  = 'ZPRU_CL_DISCARD_SAVE'
                               lastchanged       = '20251228160900.9861430'
                               locallastchanged = '20251228160900.9861430'  )
                             ( discardstrategy   = 'SUM1'
                               strategyprovider  = 'ZPRU_CL_DISCARD_SUMMARIZE'
                               lastchanged       = '20251228160923.7645650'
                               locallastchanged = '20251228160923.7645650'  )
                             ( discardstrategy   = 'DEL1'
                               strategyprovider  = 'ZPRU_CL_DISCARD_DELETE'
                               lastchanged       = '20251228160933.6209440'
                               locallastchanged = '20251228160933.6209440'  ) ).

    lt_zpru_summ_strat = VALUE #( ( summarystrategy   = 'SUM1'
                                    strategyprovider  = 'ZPRU_CL_SUMMARIZE_SIMPLE'
                                    createdby         = ''
                                    createdat         = '0.0000000'
                                    changedby         = ''
                                    lastchanged       = '20251228162302.4041410'
                                    locallastchanged = '20251228162302.4041410'  ) ).

    DELETE zpru_agent_serv FROM TABLE @lt_agent_serv.


    SELECT * FROM zpru_agent
    WHERE agentname = 'NESTED_AGENT' OR agentname = 'DUMMY_AGENT'
    INTO TABLE @DATA(lt_agent_to_be_del).
    IF sy-subrc = 0.
      DELETE zpru_agent FROM TABLE @lt_agent_to_be_del.
    ENDIF.

    IF lt_agent_to_be_del IS NOT INITIAL.
      SELECT * FROM zpru_agent_tool
      FOR ALL ENTRIES IN @lt_agent_to_be_del
      WHERE agentuuid = @lt_agent_to_be_del-agentuuid
      INTO TABLE @DATA(lt_tool_to_be_del).
      IF sy-subrc = 0.
        DELETE zpru_agent_tool FROM TABLE @lt_tool_to_be_del.
      ENDIF.
    ENDIF.

    DELETE zpru_agent_type FROM TABLE @lt_agent_type.
    DELETE zpru_disc_strat FROM TABLE @lt_disc_strat.
    DELETE zpru_summ_strat FROM TABLE @lt_zpru_summ_strat.


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
