CLASS zpru_cl_decision_request DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zpru_if_payload.
    INTERFACES zpru_if_agent_frw.
    INTERFACES zpru_if_decision_request.

  PROTECTED SECTION.
    DATA ms_decision_request TYPE REF TO zpru_s_decision_request.
ENDCLASS.


CLASS zpru_cl_decision_request IMPLEMENTATION.
  METHOD zpru_if_decision_request~get_decision_request_string.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA lv_string TYPE string.

    DATA(ls_request) = ms_decision_request->*.

    " agent metadata
    IF ls_request-agentmetadata IS NOT INITIAL.
      lv_string = |{ lv_string } Agent Metadata:{ cl_abap_char_utilities=>newline }|.

      IF ls_request-agentmetadata-agentname IS NOT INITIAL.
        lv_string = |{ lv_string } Agent Name: { ls_request-agentmetadata-agentname } { cl_abap_char_utilities=>newline }|.
      ENDIF.

      IF ls_request-agentmetadata-agentversion IS NOT INITIAL.
        lv_string = |{ lv_string } Agent Version: { ls_request-agentmetadata-agentversion } { cl_abap_char_utilities=>newline }|.
      ENDIF.

      IF ls_request-agentmetadata-agentrole IS NOT INITIAL.
        lv_string = |{ lv_string } Agent Role: { ls_request-agentmetadata-agentrole } { cl_abap_char_utilities=>newline }|.
      ENDIF.

      DATA(lv_goal_count) = 0.
      LOOP AT ls_request-agentmetadata-agentgoals ASSIGNING FIELD-SYMBOL(<ls_agent_goal>).

        IF    <ls_agent_goal>-agentgoalid          IS INITIAL
           OR <ls_agent_goal>-agentgoaldescription IS INITIAL
           OR <ls_agent_goal>-agentgoalcontent     IS INITIAL.
          CONTINUE.
        ENDIF.

        IF lv_goal_count = 0.
          lv_string = |{ lv_string } Agent Goals: { cl_abap_char_utilities=>newline }|.
        ENDIF.

        lv_string = |{ lv_string } Agent Goal ID: { <ls_agent_goal>-agentgoalid } { cl_abap_char_utilities=>newline }|.
        lv_string = |{ lv_string } Agent Goal Description: { <ls_agent_goal>-agentgoaldescription } { cl_abap_char_utilities=>newline }|.
        lv_string = |{ lv_string } Agent Goal: { <ls_agent_goal>-agentgoalcontent } { cl_abap_char_utilities=>newline }|.

        IF <ls_agent_goal>-agentgoalpriority IS NOT INITIAL.
          lv_string = |{ lv_string } Agent Goal Priority: { <ls_agent_goal>-agentgoalpriority } { cl_abap_char_utilities=>newline }|.
        ENDIF.
        IF <ls_agent_goal>-agentgoalsuccesscriteria IS NOT INITIAL.
          lv_string = |{ lv_string } Agent Success Criteria: { <ls_agent_goal>-agentgoalsuccesscriteria } { cl_abap_char_utilities=>newline }|.
        ENDIF.

        lv_goal_count += 1.
      ENDLOOP.

      IF ls_request-agentmetadata-agentdomain IS NOT INITIAL.
        lv_string = |{ lv_string } Agent Domain: { cl_abap_char_utilities=>newline }|.

        IF ls_request-agentmetadata-agentdomain-agentdomainname IS NOT INITIAL.
          lv_string = |{ lv_string } Agent Main Domain: { ls_request-agentmetadata-agentdomain-agentdomainname } { cl_abap_char_utilities=>newline }|.
        ENDIF.

        IF ls_request-agentmetadata-agentdomain-agentdomaincontent IS NOT INITIAL.
          lv_string = |{ lv_string } Agent Main Domain Description: { ls_request-agentmetadata-agentdomain-agentdomaincontent } { cl_abap_char_utilities=>newline }|.
        ENDIF.

        DATA(lv_subdomain_count) = 0.
        LOOP AT ls_request-agentmetadata-agentdomain-agentsubdomains ASSIGNING FIELD-SYMBOL(<ls_agent_subdomain>).

          IF    <ls_agent_subdomain>-agentsubdomainname    IS INITIAL
             OR <ls_agent_subdomain>-agentsubdomaincontent IS INITIAL.
            CONTINUE.
          ENDIF.

          IF lv_subdomain_count = 0.
            lv_string = |{ lv_string } Agent Subdomains: { cl_abap_char_utilities=>newline }|.
          ENDIF.

          lv_string = |{ lv_string } Agent Subdomain Name: { <ls_agent_subdomain>-agentsubdomainname } { cl_abap_char_utilities=>newline }|.
          lv_string = |{ lv_string } Agent Subdomain Description: { <ls_agent_subdomain>-agentsubdomaincontent } { cl_abap_char_utilities=>newline }|.

          lv_subdomain_count += 1.
        ENDLOOP.
      ENDIF.

      DATA(lv_agent_restrict_count) = 0.
      LOOP AT ls_request-agentmetadata-agentrestrictions ASSIGNING FIELD-SYMBOL(<ls_agentrestriction>).

        IF    <ls_agentrestriction>-agentrestrictionname IS INITIAL
           OR <ls_agentrestriction>-agentrestriction     IS INITIAL.
          CONTINUE.
        ENDIF.

        IF lv_agent_restrict_count = 0.
          lv_string = |{ lv_string } Agent Restrictions: { cl_abap_char_utilities=>newline }|.
        ENDIF.

        lv_string = |{ lv_string } Agent Restriction Name: { <ls_agentrestriction>-agentrestrictionname } { cl_abap_char_utilities=>newline }|.
        lv_string = |{ lv_string } Agent Restriction Rule: { <ls_agentrestriction>-agentrestriction } { cl_abap_char_utilities=>newline }|.

        lv_subdomain_count += 1.
      ENDLOOP.

      " tool overview
      DATA(lv_agent_tool_count) = 0.
      LOOP AT ls_request-agentmetadata-agenttools ASSIGNING FIELD-SYMBOL(<ls_agent_tool>).

        IF    <ls_agent_tool>-toolname IS INITIAL
           OR <ls_agent_tool>-toolexplanation     IS INITIAL.
          CONTINUE.
        ENDIF.


        IF lv_agent_tool_count = 0.
          lv_string = |{ lv_string } Agent Tools: { cl_abap_char_utilities=>newline }|.
        ENDIF.

        lv_string = |{ lv_string } Agent Tool Name: { <ls_agent_tool>-toolname } { cl_abap_char_utilities=>newline }|.

        IF <ls_agent_tool>-tooldesciption IS NOT INITIAL.
          lv_string = |{ lv_string } Agent Description: { <ls_agent_tool>-tooldesciption } { cl_abap_char_utilities=>newline }|.
        ENDIF.

        lv_string = |{ lv_string } Agent Explanation: { <ls_agent_tool>-toolexplanation } { cl_abap_char_utilities=>newline }|.


        CASE <ls_agent_tool>-tooltype.
          WHEN zpru_if_adf_type_and_constant=>cs_step_type-nested_agent.
            lv_string = |{ lv_string } Tool Type is Nested Agent. { cl_abap_char_utilities=>newline }|.
            lv_string = |{ lv_string } Invokes another autonomous agent as a tool, allowing for complex, multi-agent task delegation. { cl_abap_char_utilities=>newline }|.
          WHEN zpru_if_adf_type_and_constant=>cs_step_type-knowledge_source.
            lv_string = |{ lv_string } Tool Type is Knowledge Source. { cl_abap_char_utilities=>newline }|.
            lv_string = |{ lv_string } IA specialized ABAP class designed specifically to retrieve and return data packets for the agent to process. { cl_abap_char_utilities=>newline }|.
          WHEN zpru_if_adf_type_and_constant=>cs_step_type-abap_code.
            lv_string = |{ lv_string } Tool Type is ABAP Code. { cl_abap_char_utilities=>newline }|.
            lv_string = |{ lv_string } Executes a standard ABAP code within the SAP backend to perform specific logic or data processing. { cl_abap_char_utilities=>newline }|.
          WHEN zpru_if_adf_type_and_constant=>cs_step_type-http_request.
            lv_string = |{ lv_string } Tool Type is HTTP Request. { cl_abap_char_utilities=>newline }|.
            lv_string = |{ lv_string } Sends standard HTTP requests to external sources and feeds the resulting payload back into the agent loop. { cl_abap_char_utilities=>newline }|.
          WHEN zpru_if_adf_type_and_constant=>cs_step_type-service_consumption_model.
            lv_string = |{ lv_string } Tool Type is Service Consumption. { cl_abap_char_utilities=>newline }|.
            lv_string = |{ lv_string } Similar to HTTP request but utilizes SAP's structured service consumption artifacts for more robust API integration. { cl_abap_char_utilities=>newline }|.
          WHEN zpru_if_adf_type_and_constant=>cs_step_type-call_llm.
            lv_string = |{ lv_string } Tool Type is Call LLM. { cl_abap_char_utilities=>newline }|.
            lv_string = |{ lv_string } Sends a secondary prompt to an Large Language Model and captures its response as part of a larger chain of thought. { cl_abap_char_utilities=>newline }|.
          WHEN zpru_if_adf_type_and_constant=>cs_step_type-dynamic_abap_code.
            lv_string = |{ lv_string } Tool Type is Dynamic ABAP Code. { cl_abap_char_utilities=>newline }|.
            lv_string = |{ lv_string } Runs ABAP code logic stored directly in configuration database tables, allowing for updates without without writting specific class. { cl_abap_char_utilities=>newline }|.
          WHEN zpru_if_adf_type_and_constant=>cs_step_type-infer_ml_model.
            lv_string = |{ lv_string } Tool Type is Call Machine Learning Model. { cl_abap_char_utilities=>newline }|.
            lv_string = |{ lv_string } Calls external Machine Learning APIs to perform predictive analysis or data classification. { cl_abap_char_utilities=>newline }|.
          WHEN zpru_if_adf_type_and_constant=>cs_step_type-user_tool.
            lv_string = |{ lv_string } Tool Type is User Tool. { cl_abap_char_utilities=>newline }|.
            lv_string = |{ lv_string } Provides a "Human-In-The-Loop" pattern by invoking UI screens in on-premise or private cloud systems for manual intervention. { cl_abap_char_utilities=>newline }|.
          WHEN OTHERS.
        ENDCASE.

        DATA(lv_agent_tool_property_count) = 0.
        LOOP AT <ls_agent_tool>-toolproperty ASSIGNING FIELD-SYMBOL(<ls_toolproperty>).

          IF    <ls_toolproperty>-toolpropertyname IS INITIAL
             OR <ls_toolproperty>-toolproperty     IS INITIAL.
            CONTINUE.
          ENDIF.

          IF lv_agent_tool_property_count = 0.
            lv_string = |{ lv_string } Agent Tool Properties for Tool { <ls_agent_tool>-toolname } { cl_abap_char_utilities=>newline }|.
          ENDIF.

          lv_string = |{ lv_string } Tool Property Name: { <ls_toolproperty>-toolpropertyname } { cl_abap_char_utilities=>newline }|.
          lv_string = |{ lv_string } Tool Property: { <ls_toolproperty>-toolproperty } { cl_abap_char_utilities=>newline }|.

          lv_agent_tool_property_count = lv_agent_tool_property_count + 1.
        ENDLOOP.


        DATA(lv_parameter_count) = 0.
        LOOP AT <ls_agent_tool>-parameterinfo ASSIGNING FIELD-SYMBOL(<ls_parameter>).

          IF    <ls_parameter>-parametername IS INITIAL
             OR <ls_parameter>-parameterexplanation      IS INITIAL OR
             <ls_parameter>-parametertype      IS INITIAL.
            CONTINUE.
          ENDIF.

          IF lv_parameter_count = 0.
            lv_string = |{ lv_string } Agent Tool Parameters for Tool { <ls_agent_tool>-toolname } { cl_abap_char_utilities=>newline }|.
          ENDIF.

          lv_string = |{ lv_string } Tool Parameter Name: { <ls_parameter>-parametername } { cl_abap_char_utilities=>newline }|.
          lv_string = |{ lv_string } Tool Parameter Explanation: { <ls_parameter>-parameterexplanation } { cl_abap_char_utilities=>newline }|.

          IF <ls_parameter>-parameterdesciption  IS NOT INITIAL.
            lv_string = |{ lv_string } Tool Parameter Description: { <ls_parameter>-parameterdesciption } { cl_abap_char_utilities=>newline }|.
          ENDIF.

          CASE <ls_parameter>-parametertype.
            WHEN zpru_if_tool_info_provider=>cs_param_kind-importing.
              lv_string = |{ lv_string } Parameter is INPUT. { cl_abap_char_utilities=>newline }|.
            WHEN zpru_if_tool_info_provider=>cs_param_kind-exporting.
              lv_string = |{ lv_string } Parameter is OUTPUT. { cl_abap_char_utilities=>newline }|.
            WHEN zpru_if_tool_info_provider=>cs_param_kind-changing.
              lv_string = |{ lv_string } Parameter is INPUT and OUTPUT. { cl_abap_char_utilities=>newline }|.
            WHEN zpru_if_tool_info_provider=>cs_param_kind-receiving.
              lv_string = |{ lv_string } Parameter is SINGLE OUTPUT. { cl_abap_char_utilities=>newline }|.
          ENDCASE.

          DATA(lv_param_prop_count) = 0.
          LOOP AT <ls_parameter>-parameterproperty ASSIGNING FIELD-SYMBOL(<ls_parameter_property>).

            IF    <ls_parameter_property>-parameterpropertyname IS INITIAL
               OR <ls_parameter_property>-parameterproperty     IS INITIAL.
              CONTINUE.
            ENDIF.

            IF lv_param_prop_count = 0.
              lv_string = |{ lv_string } Properties for Parameter { <ls_parameter>-parametername } { cl_abap_char_utilities=>newline }|.
            ENDIF.

            lv_string = |{ lv_string } Parameter Property Name: { <ls_parameter_property>-parameterpropertyname } { cl_abap_char_utilities=>newline }|.
            lv_string = |{ lv_string } Parameter Property: { <ls_parameter_property>-parameterproperty  } { cl_abap_char_utilities=>newline }|.

            lv_param_prop_count = lv_param_prop_count + 1.
          ENDLOOP.

          lv_string = |{ lv_string } Tool Parameter JSON Schema: { <ls_parameter>-parameterjsonschema } { cl_abap_char_utilities=>newline }|.

          lv_parameter_count = lv_parameter_count + 1.
        ENDLOOP.

        lv_agent_tool_count = lv_agent_tool_count + 1.

      ENDLOOP.

      IF ls_request-agentmetadata-freetextlabel IS NOT INITIAL AND
         ls_request-agentmetadata-freetextcontent IS NOT INITIAL.
        lv_string = |{ lv_string } { ls_request-agentmetadata-freetextlabel }: { cl_abap_char_utilities=>newline }|.
        lv_string = |{ lv_string } { ls_request-agentmetadata-freetextcontent }. { cl_abap_char_utilities=>newline }|.
      ENDIF.
    ENDIF.

    " agent system prompt
    IF ls_request-systemprompt IS NOT INITIAL.
      lv_string = |{ lv_string } System Prompt:{ cl_abap_char_utilities=>newline }|.

      IF ls_request-systemprompt-primarysessiontask IS NOT INITIAL.
        lv_string = |{ lv_string } Primary Session Task: { ls_request-systemprompt-primarysessiontask }{ cl_abap_char_utilities=>newline }|.
      ENDIF.

      DATA(lv_tech_rule_count) = 0.
      LOOP AT ls_request-systemprompt-technicalrules ASSIGNING FIELD-SYMBOL(<ls_technicalrules>).

        IF    <ls_technicalrules>-technicalrulesname IS INITIAL
           OR <ls_technicalrules>-technicalrule      IS INITIAL.
          CONTINUE.
        ENDIF.

        IF lv_tech_rule_count = 0.
          lv_string = |{ lv_string } Technical Rules: { cl_abap_char_utilities=>newline }|.
        ENDIF.

        lv_string = |{ lv_string } Technical Rule Name: { <ls_technicalrules>-technicalrulesname } { cl_abap_char_utilities=>newline }|.
        lv_string = |{ lv_string } Technical Rule: { <ls_technicalrules>-technicalrule } { cl_abap_char_utilities=>newline }|.

        lv_tech_rule_count += 1.
      ENDLOOP.

      DATA(lv_bus_rule_count) = 0.
      LOOP AT ls_request-systemprompt-businessrules ASSIGNING FIELD-SYMBOL(<ls_businessrules>).

        IF    <ls_businessrules>-businessrulesname IS INITIAL
           OR <ls_businessrules>-businessrule      IS INITIAL.
          CONTINUE.
        ENDIF.

        IF lv_bus_rule_count = 0.
          lv_string = |{ lv_string } Business Rules: { cl_abap_char_utilities=>newline }|.
        ENDIF.

        lv_string = |{ lv_string } Business Rule Name: { <ls_businessrules>-businessrulesname } { cl_abap_char_utilities=>newline }|.
        lv_string = |{ lv_string } Business Rule: { <ls_businessrules>-businessrule } { cl_abap_char_utilities=>newline }|.

        lv_bus_rule_count += 1.
      ENDLOOP.

      DATA(lv_format_count) = 0.
      LOOP AT ls_request-systemprompt-formatguidelines ASSIGNING FIELD-SYMBOL(<ls_formatguidelines>).

        IF    <ls_formatguidelines>-formatguidelinename IS INITIAL
           OR <ls_formatguidelines>-formatguideline     IS INITIAL.
          CONTINUE.
        ENDIF.

        IF lv_format_count = 0.
          lv_string = |{ lv_string } Format Guidlines: { cl_abap_char_utilities=>newline }|.
        ENDIF.

        lv_string = |{ lv_string } Format Guidline Name: { <ls_formatguidelines>-formatguidelinename } { cl_abap_char_utilities=>newline }|.
        lv_string = |{ lv_string } Format Guidline Instruction: { <ls_formatguidelines>-formatguideline } { cl_abap_char_utilities=>newline }|.

        lv_format_count += 1.
      ENDLOOP.

      DATA(lv_reason_step_count) = 0.
      LOOP AT ls_request-systemprompt-reasoningstep ASSIGNING FIELD-SYMBOL(<ls_reasoningstep>).

        IF    <ls_reasoningstep>-reasoningstepname     IS INITIAL
           OR <ls_reasoningstep>-reasoningstepquestion IS INITIAL
           OR <ls_reasoningstep>-reasoninginstruction  IS INITIAL.
          CONTINUE.
        ENDIF.

        IF lv_reason_step_count = 0.
          lv_string = |{ lv_string } Reasoning Steps: { cl_abap_char_utilities=>newline }|.
          lv_string = |{ lv_string } Use them as hint what questions and how you must resolve before you send output. { cl_abap_char_utilities=>newline }|.
        ENDIF.

        lv_string = |{ lv_string } Reasoning Step Name: { <ls_reasoningstep>-reasoningstepname } { cl_abap_char_utilities=>newline }|.
        lv_string = |{ lv_string } Reasoning Step Question: { <ls_reasoningstep>-reasoningstepquestion }? { cl_abap_char_utilities=>newline }|.
        lv_string = |{ lv_string } Reasoning Step Instruction: { <ls_reasoningstep>-reasoninginstruction } { cl_abap_char_utilities=>newline }|.

        IF <ls_reasoningstep>-reasoningstepismandatory = abap_true.
          lv_string = |{ lv_string } The Reasoning Step { <ls_reasoningstep>-reasoningstepname } is mandatory. { cl_abap_char_utilities=>newline }|.
        ENDIF.

        lv_reason_step_count += 1.
      ENDLOOP.

      DATA(lv_promptrestrictions_count) = 0.
      LOOP AT ls_request-systemprompt-promptrestrictions ASSIGNING FIELD-SYMBOL(<ls_promptrestriction>).

        IF    <ls_promptrestriction>-promptrestrictionname IS INITIAL
           OR <ls_promptrestriction>-promptrestriction     IS INITIAL.
          CONTINUE.
        ENDIF.

        IF lv_promptrestrictions_count = 0.
          lv_string = |{ lv_string } Format Guidlines: { cl_abap_char_utilities=>newline }|.
        ENDIF.

        lv_string = |{ lv_string } System Prompt Restriction Name: { <ls_promptrestriction>-promptrestrictionname } { cl_abap_char_utilities=>newline }|.
        lv_string = |{ lv_string } System Prompt Restriction Rule: { <ls_promptrestriction>-promptrestriction } { cl_abap_char_utilities=>newline }|.

        lv_promptrestrictions_count += 1.
      ENDLOOP.

      IF ls_request-systemprompt-arbitrarytexttitle IS NOT INITIAL.
        lv_string = |{ lv_string }{ ls_request-systemprompt-arbitrarytexttitle } { cl_abap_char_utilities=>newline }|.
      ENDIF.

      IF ls_request-systemprompt-arbitrarytext IS NOT INITIAL.
        lv_string = |{ lv_string }{ ls_request-systemprompt-arbitrarytext } { cl_abap_char_utilities=>newline }|.
      ENDIF.

    ENDIF.

    " session memory
    IF ls_request-sessionmemory IS NOT INITIAL.
      SORT ls_request-sessionmemory BY messagedatetime ASCENDING.

      DATA(lv_session_msg_count) = 1.
      LOOP AT ls_request-sessionmemory ASSIGNING FIELD-SYMBOL(<ls_sessionmemory>).

        IF <ls_sessionmemory>-content IS INITIAL.
          CONTINUE.
        ENDIF.

        IF lv_session_msg_count = 1.
          lv_string = |{ lv_string } Recent Session Messages:{ cl_abap_char_utilities=>newline }|.
        ENDIF.

        lv_string = |{ lv_string } Message Number: { lv_session_msg_count } { cl_abap_char_utilities=>newline }|.
        lv_string = |{ lv_string } Message Content: { <ls_sessionmemory>-content } { cl_abap_char_utilities=>newline }|.

        CASE <ls_sessionmemory>-messagetype.
          WHEN zpru_if_short_memory_provider=>cs_msg_type-query.
            lv_string = |{ lv_string } Message Type: Input Query { cl_abap_char_utilities=>newline }|.
          WHEN zpru_if_short_memory_provider=>cs_msg_type-step_input.
            lv_string = |{ lv_string } Message Type: Step Input { cl_abap_char_utilities=>newline }|.
          WHEN zpru_if_short_memory_provider=>cs_msg_type-step_output.
            lv_string = |{ lv_string } Message Type: Step Output { cl_abap_char_utilities=>newline }|.
          WHEN zpru_if_short_memory_provider=>cs_msg_type-response.
            lv_string = |{ lv_string } Message Type: Final Output Response { cl_abap_char_utilities=>newline }|.
          WHEN zpru_if_short_memory_provider=>cs_msg_type-info.
            lv_string = |{ lv_string } Message Type: Information Message { cl_abap_char_utilities=>newline }|.
          WHEN OTHERS.
            lv_string = |{ lv_string } Message Type: Information Message { cl_abap_char_utilities=>newline }|.
        ENDCASE.

        lv_session_msg_count += 1.
      ENDLOOP.
    ENDIF.

    " episodic message memory
    IF ls_request-episodicmessagememory IS NOT INITIAL.
      SORT ls_request-episodicmessagememory BY messagedatetime ASCENDING.

      DATA(lv_episodicmessage_count) = 1.
      LOOP AT ls_request-episodicmessagememory ASSIGNING FIELD-SYMBOL(<ls_episodicmessage>).

        IF <ls_episodicmessage>-content IS INITIAL.
          CONTINUE.
        ENDIF.

        IF lv_episodicmessage_count = 1.
          lv_string = |{ lv_string } Conversiation History:{ cl_abap_char_utilities=>newline }|.
        ENDIF.

        lv_string = |{ lv_string } Message Number: { lv_episodicmessage_count } { cl_abap_char_utilities=>newline }|.
        lv_string = |{ lv_string } Message Content: { <ls_episodicmessage>-content } { cl_abap_char_utilities=>newline }|.

        CASE <ls_episodicmessage>-messagetype.
          WHEN zpru_if_short_memory_provider=>cs_msg_type-query.
            lv_string = |{ lv_string } Message Type: Input Query { cl_abap_char_utilities=>newline }|.
          WHEN zpru_if_short_memory_provider=>cs_msg_type-step_input.
            lv_string = |{ lv_string } Message Type: Step Input { cl_abap_char_utilities=>newline }|.
          WHEN zpru_if_short_memory_provider=>cs_msg_type-step_output.
            lv_string = |{ lv_string } Message Type: Step Output { cl_abap_char_utilities=>newline }|.
          WHEN zpru_if_short_memory_provider=>cs_msg_type-response.
            lv_string = |{ lv_string } Message Type: Final Output Response { cl_abap_char_utilities=>newline }|.
          WHEN zpru_if_short_memory_provider=>cs_msg_type-info.
            lv_string = |{ lv_string } Message Type: Information Message { cl_abap_char_utilities=>newline }|.
          WHEN OTHERS.
            lv_string = |{ lv_string } Message Type: Information Message { cl_abap_char_utilities=>newline }|.
        ENDCASE.

        lv_episodicmessage_count += 1.
      ENDLOOP.
    ENDIF.

    " episodic summarized memory
    IF ls_request-episodicsummarymemory IS NOT INITIAL.
      SORT ls_request-episodicsummarymemory BY messagedatetime ASCENDING.

      DATA(lv_episodicsummary_count) = 1.
      LOOP AT ls_request-episodicsummarymemory ASSIGNING FIELD-SYMBOL(<ls_episodicsummary>).

        IF <ls_episodicsummary>-content IS INITIAL.
          CONTINUE.
        ENDIF.

        IF lv_episodicsummary_count = 1.
          lv_string = |{ lv_string } Summary History:{ cl_abap_char_utilities=>newline }|.
        ENDIF.

        lv_string = |{ lv_string } Message Number: { lv_episodicsummary_count } { cl_abap_char_utilities=>newline }|.
        lv_string = |{ lv_string } Message Content: { <ls_episodicsummary>-content } { cl_abap_char_utilities=>newline }|.

        lv_episodicsummary_count += 1.
      ENDLOOP.
    ENDIF.

    " semantic memory
    IF ls_request-semanticmemory IS NOT INITIAL.
      DATA(lv_semantic_head_count) = 0.
      LOOP AT ls_request-semanticmemory ASSIGNING FIELD-SYMBOL(<ls_semanticmemory>).

        IF    <ls_semanticmemory>-semanticterm    IS INITIAL
           OR <ls_semanticmemory>-semanticcontent IS INITIAL.
          CONTINUE.
        ENDIF.

        IF lv_semantic_head_count = 0.
          lv_string = |{ lv_string } Semantic Memory: { cl_abap_char_utilities=>newline }|.
        ENDIF.

        lv_string = |{ lv_string } Semantic Term Name: { <ls_semanticmemory>-semanticterm } { cl_abap_char_utilities=>newline }|.
        lv_string = |{ lv_string } Semantic Term Definition: { <ls_semanticmemory>-semanticcontent } { cl_abap_char_utilities=>newline }|.

        CASE <ls_semanticmemory>-semanticcategory.
          WHEN zpru_if_long_memory_provider=>cs_semantic_cat-entity.
            lv_string = |{ lv_string } Semantic Term Category: Entity { cl_abap_char_utilities=>newline }|.
          WHEN zpru_if_long_memory_provider=>cs_semantic_cat-concept.
            lv_string = |{ lv_string } Semantic Term Category: Concept { cl_abap_char_utilities=>newline }|.
          WHEN zpru_if_long_memory_provider=>cs_semantic_cat-rule.
            lv_string = |{ lv_string } Semantic Term Category: Rule { cl_abap_char_utilities=>newline }|.
          WHEN zpru_if_long_memory_provider=>cs_semantic_cat-domain.
            lv_string = |{ lv_string } Semantic Term Category: Domain { cl_abap_char_utilities=>newline }|.
          WHEN OTHERS.
            lv_string = |{ lv_string } Semantic Term Category: Entity { cl_abap_char_utilities=>newline }|.
        ENDCASE.

        DATA(lv_semantic_rel_count) = 1.
        LOOP AT <ls_semanticmemory>-semanticrelations ASSIGNING FIELD-SYMBOL(<ls_semantic_rel>).

          IF    <ls_semantic_rel>-firstsemanticterm   IS INITIAL
             OR <ls_semantic_rel>-secondsemanticterm  IS INITIAL
             OR <ls_semantic_rel>-conceptrelationship IS INITIAL.
            CONTINUE.
          ENDIF.

          IF <ls_semantic_rel>-firstsemanticterm <> <ls_semanticmemory>-semanticterm.
            CONTINUE.
          ENDIF.

          IF lv_semantic_rel_count = 1.
            lv_string = |{ lv_string } Semantic Relation for Term { <ls_semanticmemory>-semanticterm }: { cl_abap_char_utilities=>newline }|.
          ENDIF.

          lv_string = |{ lv_string } Relation Number: { lv_semantic_rel_count } { cl_abap_char_utilities=>newline }|.
          lv_string = |{ lv_string } Relation Between: { <ls_semantic_rel>-firstsemanticterm } and { <ls_semantic_rel>-secondsemanticterm } { cl_abap_char_utilities=>newline }|.

          CASE <ls_semantic_rel>-conceptrelationship.
            WHEN zpru_if_long_memory_provider=>gc_semantic_rel-parent_of.
              lv_string = lv_string &&
                          |Term { <ls_semantic_rel>-firstsemanticterm } acts as a high-level container or owner for Term { <ls_semantic_rel>-secondsemanticterm }. | &&
                          |Term { <ls_semantic_rel>-secondsemanticterm } exists within the context of { <ls_semantic_rel>-firstsemanticterm }. | &&
                          cl_abap_char_utilities=>newline.
            WHEN zpru_if_long_memory_provider=>gc_semantic_rel-child_of.
              lv_string = lv_string &&
                          |Term { <ls_semantic_rel>-firstsemanticterm } is a sub-component or member of Term { <ls_semantic_rel>-secondsemanticterm }. | &&
                          cl_abap_char_utilities=>newline.
            WHEN zpru_if_long_memory_provider=>gc_semantic_rel-comprises.
              lv_string = lv_string &&
                          |Term { <ls_semantic_rel>-firstsemanticterm } is composed of several instances of Term { <ls_semantic_rel>-secondsemanticterm }. | &&
                          |This implies a structural assembly (e.g., a Header comprises Items). | &&
                          cl_abap_char_utilities=>newline.
            WHEN zpru_if_long_memory_provider=>gc_semantic_rel-part_of.
              lv_string = lv_string &&
                          |Term { <ls_semantic_rel>-firstsemanticterm } is a constituent element that helps make up the larger Term { <ls_semantic_rel>-secondsemanticterm }. | &&
                          cl_abap_char_utilities=>newline.
            WHEN zpru_if_long_memory_provider=>gc_semantic_rel-generalizes.
              lv_string = lv_string &&
                          |Term { <ls_semantic_rel>-firstsemanticterm } is a broad, generic template or category that covers the specific nature of Term { <ls_semantic_rel>-secondsemanticterm }.| &&
                          cl_abap_char_utilities=>newline.
            WHEN zpru_if_long_memory_provider=>gc_semantic_rel-specializes.
              lv_string = lv_string &&
                          |Term { <ls_semantic_rel>-firstsemanticterm } is a specific variation or subtype of the more general Term { <ls_semantic_rel>-secondsemanticterm }. | &&
                          cl_abap_char_utilities=>newline.
            WHEN zpru_if_long_memory_provider=>gc_semantic_rel-depends_on.
              lv_string = lv_string &&
                          |Term { <ls_semantic_rel>-firstsemanticterm } cannot function, be validated, or exist correctly without the prior existence or success of Term { <ls_semantic_rel>-secondsemanticterm }. | &&
                          cl_abap_char_utilities=>newline.
            WHEN zpru_if_long_memory_provider=>gc_semantic_rel-required_by.
              lv_string = lv_string &&
                          |Term { <ls_semantic_rel>-firstsemanticterm } is a mandatory prerequisite that must be satisfied for Term { <ls_semantic_rel>-secondsemanticterm } to proceed. | &&
                          cl_abap_char_utilities=>newline.
            WHEN zpru_if_long_memory_provider=>gc_semantic_rel-precedes.
              lv_string = lv_string &&
                          |Term { <ls_semantic_rel>-firstsemanticterm } occurs earlier in the business process timeline than Term { <ls_semantic_rel>-secondsemanticterm }. | &&
                          cl_abap_char_utilities=>newline.
            WHEN zpru_if_long_memory_provider=>gc_semantic_rel-follows.
              lv_string = lv_string &&
                          |Term { <ls_semantic_rel>-firstsemanticterm } is the subsequent step that takes place after Term { <ls_semantic_rel>-secondsemanticterm } is completed. | &&
                          cl_abap_char_utilities=>newline.
            WHEN zpru_if_long_memory_provider=>gc_semantic_rel-relates_to.
              lv_string = lv_string &&
                          |Term { <ls_semantic_rel>-firstsemanticterm } has a non-specific but relevant connection to Term { <ls_semantic_rel>-secondsemanticterm } that the agent should be aware of for context. | &&
                          cl_abap_char_utilities=>newline.
            WHEN zpru_if_long_memory_provider=>gc_semantic_rel-conflicts.
              lv_string = lv_string &&
                          |Term { <ls_semantic_rel>-firstsemanticterm } contains logic, data, or rules that are mutually exclusive with Term { <ls_semantic_rel>-secondsemanticterm }. | &&
                          |Both cannot be true or active at once.| &&
                          cl_abap_char_utilities=>newline.
            WHEN zpru_if_long_memory_provider=>gc_semantic_rel-duplicates.
              lv_string = lv_string &&
                          |Term { <ls_semantic_rel>-firstsemanticterm } contains essentially the same semantic information as Term { <ls_semantic_rel>-secondsemanticterm } and should be treated as a redundant entry. | &&
                          cl_abap_char_utilities=>newline.
            WHEN OTHERS.
              lv_string = lv_string &&
                          |Term { <ls_semantic_rel>-firstsemanticterm } has a non-specific but relevant connection to Term { <ls_semantic_rel>-secondsemanticterm } that the agent should be aware of for context. | &&
                          cl_abap_char_utilities=>newline.
          ENDCASE.
          lv_semantic_rel_count += 1.
        ENDLOOP.

        lv_semantic_head_count += 1.
      ENDLOOP.
    ENDIF.

    " rag data
    IF ls_request-ragdata IS NOT INITIAL.
      DATA(lv_rag_head_count) = 1.
      LOOP AT ls_request-ragdata ASSIGNING FIELD-SYMBOL(<ls_ragdata>).

        IF    <ls_ragdata>-ragsourcename IS INITIAL
           OR <ls_ragdata>-ragchunks     IS INITIAL.
          CONTINUE.
        ENDIF.

        IF lv_rag_head_count = 1.
          lv_string = |{ lv_string } RAG Data: { cl_abap_char_utilities=>newline }|.
        ENDIF.

        lv_string = |{ lv_string } Resource Name: { <ls_ragdata>-ragsourcename } { cl_abap_char_utilities=>newline }|.

        DATA(lv_rag_item_count) = 1.
        LOOP AT <ls_ragdata>-ragchunks ASSIGNING FIELD-SYMBOL(<ls_rag_chunks>).

          IF    <ls_rag_chunks>-ragchunkid   IS INITIAL
             OR <ls_rag_chunks>-chunkcontent IS INITIAL.
            CONTINUE.
          ENDIF.

          IF lv_rag_item_count = 1.
            lv_string = |{ lv_string } RAG Chunks for { <ls_ragdata>-ragsourcename }: { cl_abap_char_utilities=>newline }|.
          ENDIF.

          lv_string = |{ lv_string } Chunk Number { <ls_rag_chunks>-ragchunkid }: { cl_abap_char_utilities=>newline }|.
          lv_string = |{ lv_string } Chunk Content { <ls_rag_chunks>-chunkcontent }: { cl_abap_char_utilities=>newline }|.

          lv_rag_item_count += 1.
        ENDLOOP.

        lv_rag_head_count += 1.
      ENDLOOP.
    ENDIF.

    lv_string = |{ lv_string } User Data in JSON: { ls_request-userdata }: { cl_abap_char_utilities=>newline }|.

    lv_string = |{ lv_string } User Prompt: { ls_request-userprompt }: { cl_abap_char_utilities=>newline }|.

    rv_decision_request = lv_string.

  ENDMETHOD.

  METHOD zpru_if_payload~clear_data.
    CLEAR ms_decision_request.
  ENDMETHOD.

  METHOD zpru_if_payload~get_data.
    rr_data = ms_decision_request.
  ENDMETHOD.

  METHOD zpru_if_payload~set_data.
    CREATE DATA ms_decision_request.
    ASSIGN ms_decision_request->* TO FIELD-SYMBOL(<ls_data>).
    <ls_data> = ir_data->*.
  ENDMETHOD.
ENDCLASS.
