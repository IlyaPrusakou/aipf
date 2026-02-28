CLASS zpru_cl_syst_prmpt_prvdr_base DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zpru_if_agent_frw.
    INTERFACES zpru_if_prompt_provider.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.


CLASS zpru_cl_syst_prmpt_prvdr_base IMPLEMENTATION.
  METHOD zpru_if_prompt_provider~get_system_prompt.
    DATA lv_dynamyc_tool_example TYPE string.

    rv_system_prompt =
    |### TOOL EXECUTION GUIDELINES| &&
     cl_abap_char_utilities=>newline &&
     |You have access to different step types to process tasks. | &&
     cl_abap_char_utilities=>newline &&
     cl_abap_char_utilities=>newline &&
     |Type '{ zpru_if_adf_type_and_constant=>cs_step_type-nested_agent }' (Nested Agent): Use when the task requires specialized domain expertise | &&
     |outside your primary scope (e.g., specific country customs regulations).| &&
     cl_abap_char_utilities=>newline &&
     |Type '{ zpru_if_adf_type_and_constant=>cs_step_type-knowledge_source }' (Knowledge Source): Use to retrieve additional information, | &&
     |SOPs, or regulatory documentation.| &&
     cl_abap_char_utilities=>newline &&
     |Type '{ zpru_if_adf_type_and_constant=>cs_step_type-abap_code }' (ABAP Code): Use for standard, pre-defined SAP business logic | &&
     |(e.g., standard Delivery creation).| &&
     cl_abap_char_utilities=>newline &&
     |Type '{ zpru_if_adf_type_and_constant=>cs_step_type-http_request }' (HTTP Request): Use for external API integrations or | &&
     |fetching real-time data from non-SAP services.| &&
     cl_abap_char_utilities=>newline &&
     |Type '{ zpru_if_adf_type_and_constant=>cs_step_type-service_consumption_model }' (Service Consumption): Use for OData or | &&
     |Enterprise Services requiring structured proxy consumption.| &&
     cl_abap_char_utilities=>newline &&
     |Type '{ zpru_if_adf_type_and_constant=>cs_step_type-call_llm }' (Call LLM): Use for unstructured text analysis, sentiment | &&
     |detection, or complex reasoning tasks within a sub-process.| &&
     cl_abap_char_utilities=>newline &&
     |Type '{ zpru_if_adf_type_and_constant=>cs_step_type-dynamic_abap_code }' (Dynamic ABAP): Use for highly flexible logic that | &&
     |must be generated or adjusted at runtime based on specific requirements.| &&
     cl_abap_char_utilities=>newline &&
     |Type '{ zpru_if_adf_type_and_constant=>cs_step_type-infer_ml_model }' (Infer ML): Use for predictive analytics, such as | &&
     |estimating arrival times or identifying high-risk fraud patterns.| &&
     cl_abap_char_utilities=>newline &&
     |Type '{ zpru_if_adf_type_and_constant=>cs_step_type-user_tool }' (User Tool): Use when a manual human intervention or | &&
     |UI-based decision is mandatory before proceeding.|.

    lv_dynamyc_tool_example =
  |\{{ cl_abap_char_utilities=>newline }| &&
    |  "dynamic_tool": [{ cl_abap_char_utilities=>newline }| &&
    |    \{{ cl_abap_char_utilities=>newline }| &&
    |      "log_area": "CMR_PROCESSING",{ cl_abap_char_utilities=>newline }| &&
    |      "class_name": "ZCL_CMR_HANDLER",{ cl_abap_char_utilities=>newline }| &&
    |      "method_name": "CREATE_DOC",{ cl_abap_char_utilities=>newline }| &&
    |      "parameters": [{ cl_abap_char_utilities=>newline }| &&
    |        \{ "name": "IV_SENDER", "value": "SteelCorp" \},{ cl_abap_char_utilities=>newline }| &&
    |        \{ "name": "IV_WEIGHT", "value": "10500" \}{ cl_abap_char_utilities=>newline }| &&
    |      ]{ cl_abap_char_utilities=>newline }| &&
    |    \},{ cl_abap_char_utilities=>newline }| &&
    |    \{{ cl_abap_char_utilities=>newline }| &&
    |      "log_area": "SECURITY",{ cl_abap_char_utilities=>newline }| &&
    |      "class_name": "ZCL_GATE_HANDLER",{ cl_abap_char_utilities=>newline }| &&
    |      "method_name": "VERIFY_PASS",{ cl_abap_char_utilities=>newline }| &&
    |      "parameters": [{ cl_abap_char_utilities=>newline }| &&
    |        \{ "name": "IV_PLATE", "value": "XYZ-99-AB" \}{ cl_abap_char_utilities=>newline }| &&
    |      ]{ cl_abap_char_utilities=>newline }| &&
    |    \}{ cl_abap_char_utilities=>newline }| &&
    |  ]{ cl_abap_char_utilities=>newline }| &&
    |\}|.

    rv_system_prompt = |{ rv_system_prompt }| &&
     cl_abap_char_utilities=>newline &&
     |### EXAMPLE OF DYNAMIC TOOL OUTPUT| &&
     |When generating the "DYNAMIC_TOOLS" always use JSON array, provided below, to make context for dynamic tool invocation:| &&
     |Always set array under object field name 'dynamic_tool'| &&
     cl_abap_char_utilities=>newline &&
     |{ lv_dynamyc_tool_example } | &&
     cl_abap_char_utilities=>newline.
  ENDMETHOD.
  METHOD zpru_if_prompt_provider~get_abap_system_prompt.

  ENDMETHOD.

ENDCLASS.
