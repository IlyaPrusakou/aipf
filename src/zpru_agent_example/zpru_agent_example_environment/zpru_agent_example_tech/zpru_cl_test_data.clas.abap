CLASS zpru_cl_test_data DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.

    CLASS-METHODS refresh_test_data.
    CLASS-METHODS greate_snro_intervals.
ENDCLASS.


CLASS zpru_cl_test_data IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
    refresh_test_data( ).
*    greate_snro_intervals( ).
  ENDMETHOD.

  METHOD refresh_test_data.
    DATA lt_agent_serv      TYPE STANDARD TABLE OF zpru_agent_serv WITH EMPTY KEY.
    DATA lt_agent           TYPE STANDARD TABLE OF zpru_agent WITH EMPTY KEY.
    DATA lt_agent_tool      TYPE STANDARD TABLE OF zpru_agent_tool WITH EMPTY KEY.
    DATA lt_agent_type      TYPE STANDARD TABLE OF zpru_agent_type WITH EMPTY KEY.
    DATA lt_disc_strat      TYPE STANDARD TABLE OF zpru_disc_strat WITH EMPTY KEY.
    DATA lt_zpru_summ_strat TYPE STANDARD TABLE OF zpru_summ_strat WITH EMPTY KEY.

    lt_agent_serv = VALUE #( CreatedBy = ''
                             CreatedAt = '0.0000000'
                             ChangedBy = ''
                             ( Service          = 'ZPRU_IF_DECISION_REQUEST'
                               Context          = 'STANDARD'
                               Class            = 'ZPRU_CL_DECISION_REQUEST'
                               LastChanged      = '20260125095408.0057390'
                               LocalLastChanged = '20260125095408.0057390' )
                             ( Service          = 'ZPRU_IF_MSUM_SERVICE'
                               Context          = 'STANDARD_PERSISTENCE_SUMMARIZE'
                               Class            = 'ZPRU_CL_MSUM_SERVICE'
                               LastChanged      = '20260125095408.0057390'
                               LocalLastChanged = '20260125095408.0057390' )
                             ( Service          = 'ZPRU_IF_MSUM_PRECHECK'
                               Context          = 'STANDARD_PERSISTENCE_SUMMARIZE'
                               Class            = 'ZPRU_CL_MSUM_PRECHECK'
                               LastChanged      = '20260125095408.0057390'
                               LocalLastChanged = '20260125095408.0057390' )
                             ( Service          = 'ZPRU_IF_MMSG_SERVICE'
                               Context          = 'STANDARD_PERSISTENCE_MESSAGE'
                               Class            = 'ZPRU_CL_MMSG_SERVICE'
                               LastChanged      = '20260125095408.0057390'
                               LocalLastChanged = '20260125095408.0057390' )
                             ( Service          = 'ZPRU_IF_MMSG_PRECHECK'
                               Context          = 'STANDARD_PERSISTENCE_MESSAGE'
                               Class            = 'ZPRU_CL_MMSG_PRECHECK'
                               LastChanged      = '20260125095408.0057390'
                               LocalLastChanged = '20260125095408.0057390' )
                             ( Service          = 'ZPRU_IF_AGSRV_SERVICE'
                               Context          = 'STANDARD'
                               Class            = 'ZPRU_CL_AGSRV_SERVICE'
                               LastChanged      = '20260125095408.0057390'
                               LocalLastChanged = '20260125095408.0057390' )
                             ( Service          = 'ZPRU_IF_AGSRV_PRECHECK'
                               Context          = 'STANDARD'
                               Class            = 'ZPRU_CL_AGSRV_PRECHECK'
                               LastChanged      = '20260125095408.0057390'
                               LocalLastChanged = '20260125095408.0057390' )
                             ( Service          = 'ZPRU_IF_ADF_VALIDATOR'
                               Context          = 'STANDARD'
                               Class            = 'ZPRU_CL_ADF_VALIDATOR'
                               LastChanged      = '20260125095408.0057390'
                               LocalLastChanged = '20260125095408.0057390' )
                             ( Service          = 'ZPRU_IF_AGTY_PRECHECK'
                               Context          = 'STANDARD_AGENT_DEFINITION'
                               Class            = 'ZPRU_CL_AGTY_PRECHECK'
                               LastChanged      = '20260125095408.0057390'
                               LocalLastChanged = '20260125095408.0057390' )
                             ( Service          = 'ZPRU_IF_AGTY_SERVICE'
                               Context          = 'STANDARD'
                               Class            = 'ZPRU_CL_AGTY_SERVICE'
                               LastChanged      = '20260125095408.0057390'
                               LocalLastChanged = '20260125095408.0057390' )
                             ( Service          = 'ZPRU_IF_UNIT_AGENT'
                               Context          = 'STANDARD'
                               Class            = 'ZPRU_CL_UNIT_AGENT'
                               LastChanged      = '20260117150518.3887310'
                               LocalLastChanged = '20260117150518.3887310'  )
                             ( Service          = 'ZPRU_IF_API_AGENT'
                               Context          = 'STANDARD'
                               Class            = 'ZPRU_CL_API_AGENT'
                               LastChanged      = '20260117150619.0368210'
                               LocalLastChanged = '20260117150619.0368210'  )
                             ( Service          = 'ZPRU_IF_AGENT_CONTROLLER'
                               Context          = 'STANDARD'
                               Class            = 'ZPRU_CL_AGENT_CONTROLLER'
                               LastChanged      = '20260117150738.0236420'
                               LocalLastChanged = '20260117150738.0236420' )
                             ( Service          = 'ZPRU_IF_AGENT_UTIL'
                               Context          = 'STANDARD'
                               Class            = 'ZPRU_CL_AGENT_UTIL'
                               LastChanged      = '20260117150935.4567820'
                               LocalLastChanged = '20260117150935.4567820'  )
                             ( Service          = 'ZPRU_IF_PAYLOAD'
                               Context          = 'STANDARD'
                               Class            = 'ZPRU_CL_PAYLOAD'
                               LastChanged      = '20260117151018.7600370'
                               LocalLastChanged = '20260117151018.7600370'  )
                             ( Service          = 'ZPRU_IF_ADF_SERVICE'
                               Context          = 'STANDARD'
                               Class            = 'ZPRU_CL_ADF_SERVICE'
                               LastChanged      = '20260117151237.1192170'
                               LocalLastChanged = '20260117151237.1192170'  )
                             ( Service          = 'ZPRU_IF_DUMMY_AGENT_LOGIC'
                               Context          = 'STANDARD'
                               Class            = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                               LastChanged      = '20260117152144.6419870'
                               LocalLastChanged = '20260117152144.6419870' )
                             ( Service          = 'IF_AIC_COMPLETION_API'
                               Context          = 'STANDARD'
                               Class            = 'ZPRU_CL_AIC_COMPLETION_API'
                               LastChanged      = '20260117152251.0551680'
                               LocalLastChanged = '20260117152251.0551680'  )
                             ( Service          = 'IF_AIC_COMPLETION_API_RESULT'
                               Context          = 'STANDARD'
                               Class            = 'ZPRU_CL_AIC_COMPLETION_API_RES'
                               LastChanged      = '20260117152343.0309600'
                               LocalLastChanged = '20260117152343.0309600'  )
                             ( Service          = 'IF_AIC_COMPLETION_PARAMETERS'
                               Context          = 'STANDARD'
                               Class            = 'ZPRU_CL_AIC_COMPLETION_PARAM'
                               LastChanged      = '20260117152423.9709400'
                               LocalLastChanged = '20260117152423.9709400'  )
                             ( Service          = 'IF_AIC_MESSAGE_CONTAINER'
                               Context          = 'STANDARD'
                               Class            = 'ZPRU_CL_AIC_MESSAGE_CONTAINER'
                               LastChanged      = '20260117152547.0539920'
                               LocalLastChanged = '20260117152547.0539920'  )
                             ( Service          = 'IF_AIC_ISLM_COMPL_API_FACTORY'
                               Context          = 'STANDARD'
                               Class            = 'ZPRU_CL_ISLM_COMPL_API_FACTORY'
                               LastChanged      = '20260117152659.4183900'
                               LocalLastChanged = '20260117152659.4183900'  )
                             ( Service          = 'IF_WEB_HTTP_CLIENT'
                               Context          = 'STANDARD'
                               Class            = 'ZPRU_CL_WEB_HTTP_CLIENT'
                               LastChanged      = '20260117152753.5715620'
                               LocalLastChanged = '20260117152753.5715620'  )
                             ( Service          = 'IF_WEB_HTTP_REQUEST'
                               Context          = 'STANDARD'
                               Class            = 'ZPRU_CL_WEB_HTTP_REQUEST'
                               LastChanged      = '20260117152903.9660210'
                               LocalLastChanged = '20260117152903.9660210'  )
                             ( Service          = 'IF_WEB_HTTP_RESPONSE'
                               Context          = 'STANDARD'
                               Class            = 'ZPRU_CL_WEB_HTTP_RESPONSE'
                               LastChanged      = '20260117153008.0378680'
                               LocalLastChanged = '20260117153008.0378680'  )
                             ( Service          = 'ZPRU_IF_AXC_SERVICE'
                               Context          = 'STANDARD'
                               Class            = 'ZPRU_CL_AXC_SERVICE'
                               LastChanged      = '20260117153230.0427800'
                               LocalLastChanged = '20260117153230.0427800'  )
                             ( Service          = 'ZPRU_IF_LONG_MEMORY_PROVIDER'
                               Context          = 'STANDARD'
                               Class            = 'ZPRU_CL_LONG_MEMORY_BASE'
                               LastChanged      = '20260117153709.0139100'
                               LocalLastChanged = '20260117153709.0139100'  )
                             ( Service          = 'ZPRU_IF_LONG_MEM_PERSISTENCE'
                               Context          = 'STANDARD_PERSISTENCE_MESSAGE'
                               Class            = 'ZPRU_CL_PERSISTENCE_MSG'
                               LastChanged      = '20260117154416.9645010'
                               LocalLastChanged = '20260117154416.9645010'  )
                             ( Service          = 'ZPRU_IF_LONG_MEM_PERSISTENCE'
                               Context          = 'STANDARD_PERSISTENCE_SUMMARIZE'
                               Class            = 'ZPRU_CL_PERSISTENCE_SUM'
                               LastChanged      = '20260117154604.0443640'
                               LocalLastChanged = '20260117154604.0443640'  )
                             ( Service          = 'ZPRU_IF_SUMMARIZATION'
                               Context          = 'STANDARD_SUMMARIZE'
                               Class            = 'ZPRU_CL_PERSISTENCE_SUM'
                               LastChanged      = '20260117154729.8684840'
                               LocalLastChanged = '20260117154729.8684840'  )
                             ( Service          = 'ZPRU_IF_SHORT_MEMORY_PROVIDER'
                               Context          = 'STANDARD'
                               Class            = 'ZPRU_CL_SHORT_MEMORY_BASE'
                               LastChanged      = '20260117154917.7603470'
                               LocalLastChanged = '20260117154917.7603470'  )
                             ( Service          = 'ZPRU_IF_DISCARD_STRATEGY'
                               Context          = 'STANDARD_DISCARD_STRATEGY_DELETE'
                               Class            = 'ZPRU_CL_DISCARD_DELETE'
                               LastChanged      = '20260117155051.9175870'
                               LocalLastChanged = '20260117155051.9175870'  )
                             ( Service          = 'ZPRU_IF_DISCARD_STRATEGY'
                               Context          = 'STANDARD_DISCARD_STRATEGY_SAVE'
                               Class            = 'ZPRU_CL_DISCARD_SAVE'
                               LastChanged      = '20260117155223.7925380'
                               LocalLastChanged = '20260117155223.7925380'  )
                             ( Service          = 'ZPRU_IF_DISCARD_STRATEGY'
                               Context          = 'STANDARD_DISCARD_STRATEGY_SUMMARIZE'
                               Class            = 'ZPRU_CL_DISCARD_SUMMARIZE'
                               LastChanged      = '20260117155325.9377550'
                               LocalLastChanged = '20260117155325.9377550'  )
                             ( Service          = 'ZPRU_IF_AXC_PRECHECK'
                               Context          = 'STANDARD_AGENT_EXECUTION'
                               Class            = 'ZPRU_CL_AXC_PRECHECK'
                               LastChanged      = '20260118160211.0649630'
                               LocalLastChanged = '20260118160211.0649630'  )
                             ( Service          = 'ZPRU_IF_ADF_PRECHECK'
                               Context          = 'STANDARD_AGENT_DEFINITION'
                               Class            = 'ZPRU_CL_ADF_PRECHECK'
                               LastChanged      = '20260118160851.3804580'
                               LocalLastChanged = '20260118160851.3804580'  ) ).

    TRY.
        DATA(lv_agent_uuid1) = cl_system_uuid=>create_uuid_x16_static( ).
        DATA(lv_agent_uuid2) = cl_system_uuid=>create_uuid_x16_static( ).
      CATCH cx_uuid_error.
    ENDTRY.

    lt_agent = VALUE #( agenttype   = 'AGTYP1'
                        agentstatus = 'N'
                        createdby   = ''
                        createdat   = '0.0000000'
                        changedby   = ''
                        ( agentuuid            = lv_agent_uuid1
                          agentname            = 'NESTED_AGENT'
                          decisionprovider     = 'ZPRU_CL_NESTED_DECISION'
                          shortmemoryprovider  = 'ZPRU_CL_NESTED_SHORT_MEMORY'
                          longmemoryprovider   = 'ZPRU_CL_NESTED_LONG_MEMORY'
                          agentinfoprovider    = 'ZPRU_CL_NESTED_AGENT_INFO'
                          systempromptprovider = 'ZPRU_CL_NESTED_SYSTEM_PROMPT'
                          lastchanged          = '20260123211537.8743820'
                          locallastchanged     = '20260123204019.7685060'  )
                        ( agentuuid            = lv_agent_uuid2
                          agentname            = 'DUMMY_AGENT'
                          decisionprovider     = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                          shortmemoryprovider  = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                          longmemoryprovider   = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                          agentinfoprovider    = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                          systempromptprovider = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                          lastchanged          = '20260123212025.9435130'
                          locallastchanged     = '20251228170556.0725850'  ) ).
    TRY.
        lt_agent_tool = VALUE #( ( tooluuid           = cl_system_uuid=>create_uuid_x16_static( )
                                   agentuuid          = lv_agent_uuid2 " 2
                                   toolname           = 'NESTED_AGENT'
                                   toolprovider       = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                                   steptype           = 'A'
                                   toolschemaprovider = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                                   toolinfoprovider   = 'ZPRU_CL_DUMMY_AGENT_LOGIC'  )
                                 ( tooluuid           = cl_system_uuid=>create_uuid_x16_static( )
                                   agentuuid          = lv_agent_uuid1 " 1
                                   toolname           = 'NESTED_ABAP'
                                   toolprovider       = 'ZPRU_CL_NESTED_CODE'
                                   steptype           = 'B'
                                   toolschemaprovider = 'ZPRU_CL_NESTED_CODE_SCHM_PRVDR'
                                   toolinfoprovider   = 'ZPRU_CL_NESTED_CODE_INFO_PRVDR'  )
                                 ( tooluuid           = cl_system_uuid=>create_uuid_x16_static( )
                                   agentuuid          = lv_agent_uuid1 " 1
                                   toolname           = 'NESTED_LLM'
                                   toolprovider       = 'ZPRU_CL_NESTED_LLM'
                                   steptype           = 'L'
                                   toolschemaprovider = 'ZPRU_CL_NESTED_LLM_SCHM_PRVDR'
                                   toolinfoprovider   = 'ZPRU_CL_NESTED_LLM_INFO_PRVDR'  )
                                 ( tooluuid           = cl_system_uuid=>create_uuid_x16_static( )
                                   agentuuid          = lv_agent_uuid1 " 1
                                   toolname           = 'NESTED_HTTP'
                                   toolprovider       = 'ZPRU_CL_NESTED_HTTP'
                                   steptype           = 'H'
                                   toolschemaprovider = 'ZPRU_CL_NESTED_HTTP_SCHM_PRVDR'
                                   toolinfoprovider   = 'ZPRU_CL_NESTED_HTTP_INFO_PRVDR'  )
                                 ( tooluuid           = cl_system_uuid=>create_uuid_x16_static( )
                                   agentuuid          = lv_agent_uuid2 " 2
                                   toolname           = 'DUMMY_CODE'
                                   toolprovider       = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                                   steptype           = 'B'
                                   toolschemaprovider = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                                   toolinfoprovider   = 'ZPRU_CL_DUMMY_AGENT_LOGIC'  )
                                 ( tooluuid           = cl_system_uuid=>create_uuid_x16_static( )
                                   agentuuid          = lv_agent_uuid2 " 2
                                   toolname           = 'DUMMY_USER_TOOL'
                                   toolprovider       = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                                   steptype           = 'Z'
                                   toolschemaprovider = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                                   toolinfoprovider   = 'ZPRU_CL_DUMMY_AGENT_LOGIC'  )
                                 ( tooluuid           = cl_system_uuid=>create_uuid_x16_static( )
                                   agentuuid          = lv_agent_uuid2 " 2
                                   toolname           = 'DUMMY_ML'
                                   toolprovider       = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                                   steptype           = 'M'
                                   toolschemaprovider = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                                   toolinfoprovider   = 'ZPRU_CL_DUMMY_AGENT_LOGIC'  )
                                 ( tooluuid           = cl_system_uuid=>create_uuid_x16_static( )
                                   agentuuid          = lv_agent_uuid2 " 2
                                   toolname           = 'DUMMY_DYN_CODE'
                                   toolprovider       = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                                   steptype           = 'D'
                                   toolschemaprovider = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                                   toolinfoprovider   = 'ZPRU_CL_DUMMY_AGENT_LOGIC'  )
                                 ( tooluuid           = cl_system_uuid=>create_uuid_x16_static( )
                                   agentuuid          = lv_agent_uuid2 " 2
                                   toolname           = 'DUMMY_SCM'
                                   toolprovider       = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                                   steptype           = 'S'
                                   toolschemaprovider = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                                   toolinfoprovider   = 'ZPRU_CL_DUMMY_AGENT_LOGIC'  )
                                 ( tooluuid           = cl_system_uuid=>create_uuid_x16_static( )
                                   agentuuid          = lv_agent_uuid2 " 2
                                   toolname           = 'DUMMY_HTTP'
                                   toolprovider       = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                                   steptype           = 'H'
                                   toolschemaprovider = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                                   toolinfoprovider   = 'ZPRU_CL_DUMMY_AGENT_LOGIC'  )
                                 ( tooluuid           = cl_system_uuid=>create_uuid_x16_static( )
                                   agentuuid          = lv_agent_uuid2 " 2
                                   toolname           = 'DUMMY_LLM'
                                   toolprovider       = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                                   steptype           = 'L'
                                   toolschemaprovider = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                                   toolinfoprovider   = 'ZPRU_CL_DUMMY_AGENT_LOGIC'  )
                                 ( tooluuid           = cl_system_uuid=>create_uuid_x16_static( )
                                   agentuuid          = lv_agent_uuid2 " 2
                                   toolname           = 'DUMMY_KNOWLEDGE'
                                   toolprovider       = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                                   steptype           = 'K'
                                   toolschemaprovider = 'ZPRU_CL_DUMMY_AGENT_LOGIC'
                                   toolinfoprovider   = 'ZPRU_CL_DUMMY_AGENT_LOGIC'  ) ).
      CATCH cx_uuid_error.
    ENDTRY.

    lt_agent_type = VALUE #( ( agenttype           = 'AGTYP1'
                               shortmemoryvolume   = '8'
                               discardstrategy     = 'SUM1'
                               summarystrategy     = 'SUM1'
                               maximumnumberofloop = '3'
                               createdby           = ''
                               createdat           = '0.0000000'
                               changedby           = ''
                               lastchanged         = '20251228170454.7391400'
                               locallastchanged    = '20251228170454.7391400'  ) ).

    lt_disc_strat = VALUE #( createdby = ''
                             createdat = '0.0000000'
                             changedby = ''
                             ( discardstrategy  = 'SAV1'
                               discardprovider  = 'ZPRU_CL_DISCARD_SAVE'
                               lastchanged      = '20251228160900.9861430'
                               locallastchanged = '20251228160900.9861430'  )
                             ( discardstrategy  = 'SUM1'
                               discardprovider  = 'ZPRU_CL_DISCARD_SUMMARIZE'
                               lastchanged      = '20251228160923.7645650'
                               locallastchanged = '20251228160923.7645650'  )
                             ( discardstrategy  = 'DEL1'
                               discardprovider  = 'ZPRU_CL_DISCARD_DELETE'
                               lastchanged      = '20251228160933.6209440'
                               locallastchanged = '20251228160933.6209440'  ) ).

    lt_zpru_summ_strat = VALUE #( ( summarystrategy  = 'SUM1'
                                    summaryprovider  = 'ZPRU_CL_SUMMARIZE_SIMPLE'
                                    createdby        = ''
                                    createdat        = '0.0000000'
                                    changedby        = ''
                                    lastchanged      = '20251228162302.4041410'
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

  METHOD greate_snro_intervals.
    DATA lt_intervals      TYPE cl_numberrange_intervals=>nr_interval.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA lt_saved_interval TYPE cl_numberrange_intervals=>nr_interval.

    " Define your '01' interval
    lt_intervals = VALUE #( ( nrrangenr  = '01'
                              fromnumber = '00000000000000000001'
                              tonumber   = '99999999999999999999' ) ).

    TRY.
        cl_numberrange_intervals=>create( EXPORTING interval  = lt_intervals
                                                    object    = 'ZPRU_AXCHD'
                                          IMPORTING
                                          " TODO: variable is assigned but never used (ABAP cleaner)
                                                    error     = DATA(lv_error)
                                          " TODO: variable is assigned but never used (ABAP cleaner)
                                                    error_inf = DATA(ls_error_inf)
                                          " TODO: variable is assigned but never used (ABAP cleaner)
                                                    error_iv  = DATA(lt_error_iv)
                                          " TODO: variable is assigned but never used (ABAP cleaner)
                                                    warning   = DATA(lv_warning) ).

        cl_numberrange_intervals=>read( EXPORTING object   = 'ZPRU_AXCHD'
                                        IMPORTING interval = lt_saved_interval ).

      CATCH cx_number_ranges INTO DATA(lx_error). " TODO: variable is assigned but never used (ABAP cleaner)
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
