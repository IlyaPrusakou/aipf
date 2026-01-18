CLASS lcl_decision_provider DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zpru_if_decision_provider.
ENDCLASS.


CLASS lcl_decision_provider IMPLEMENTATION.
  METHOD zpru_if_decision_provider~call_decision_engine.
    DATA lt_execution_plan TYPE zpru_if_decision_provider=>tt_execution_plan.

    zpru_cl_dummy_agent_logic=>ms_method_registr-call_decision_engine = abap_true.

    TRY.
        FINAL(lo_api) = cl_aic_islm_compl_api_factory=>get( )->create_instance( 'ST-GEMINI-3.0' ).
        FINAL(lo_params) = lo_api->get_parameter_setter( ).
        lo_params->set_temperature( '0.5' ).

        " TODO: variable is assigned but never used (ABAP cleaner)
        FINAL(lv_response) = lo_api->execute_for_string( 'How are you?' )->get_completion( ).
      CATCH cx_aic_api_factory
            cx_aic_completion_api.

    ENDTRY.

    GET TIME STAMP FIELD DATA(lv_now).

    APPEND INITIAL LINE TO lt_execution_plan ASSIGNING FIELD-SYMBOL(<ls_execution_plan>).
    <ls_execution_plan>-agent_uuid = is_agent-agent_uuid.
    <ls_execution_plan>-tool_name  = 'SIMPLE_TOOL'.
    <ls_execution_plan>-sequence   = 1.

    APPEND INITIAL LINE TO lt_execution_plan ASSIGNING <ls_execution_plan>.
    <ls_execution_plan>-agent_uuid = is_agent-agent_uuid.
    <ls_execution_plan>-tool_name  = 'KNOWLEDGE'.
    <ls_execution_plan>-sequence   = 2.

    APPEND INITIAL LINE TO lt_execution_plan ASSIGNING <ls_execution_plan>.
    <ls_execution_plan>-agent_uuid = is_agent-agent_uuid.
    <ls_execution_plan>-tool_name  = 'NESTED_AGENT'.
    <ls_execution_plan>-sequence   = 3.

    eo_execution_plan->set_data( ir_data = NEW zpru_if_decision_provider=>tt_execution_plan( lt_execution_plan ) ).
    eo_first_tool_input->set_data( ir_data = NEW string( |FIRST TOOL INPUT - { lv_now }| ) ).
    eo_langu->set_data( ir_data = NEW spras( sy-langu ) ).
    eo_decision_log->set_data( ir_data = NEW string( |DECISION LOG - { lv_now }| ) ).
  ENDMETHOD.

  METHOD zpru_if_decision_provider~prepare_final_response.
    DATA lv_final_response TYPE string.

    zpru_cl_dummy_agent_logic=>ms_method_registr-prepare_final_response = abap_true.

    DATA(lv_last_output) = io_last_output->get_data( ).
    GET TIME STAMP FIELD DATA(lv_now).
    lv_final_response = |{ lv_last_output->* } - FINAL_RESPONSE - { lv_now } |.
    eo_final_response->set_data( ir_data = NEW string( lv_final_response ) ).
  ENDMETHOD.
ENDCLASS.


CLASS lcl_short_memory_provider DEFINITION
  INHERITING FROM zpru_cl_short_memory_base
  CREATE PUBLIC.

  PUBLIC SECTION.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.


CLASS lcl_short_memory_provider IMPLEMENTATION.
ENDCLASS.


CLASS lcl_long_memory_provider DEFINITION
  INHERITING FROM zpru_cl_long_memory_base
  CREATE PUBLIC.

  PUBLIC SECTION.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.


CLASS lcl_long_memory_provider IMPLEMENTATION.

ENDCLASS.


CLASS lcl_agent_info_provider DEFINITION
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zpru_if_agent_info_provider.
ENDCLASS.


CLASS lcl_agent_info_provider IMPLEMENTATION.
  METHOD zpru_if_agent_info_provider~get_agent_info.
    zpru_cl_dummy_agent_logic=>ms_method_registr-get_agent_info = abap_true.

    GET TIME STAMP FIELD DATA(lv_now).
    rv_agent_info = |JUST DUMMY AGENT - { lv_now }|.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_prompt_provider DEFINITION
  INHERITING FROM zpru_cl_syst_prmpt_prvdr_base CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS zpru_if_prompt_provider~get_system_prompt REDEFINITION.
ENDCLASS.


CLASS lcl_prompt_provider IMPLEMENTATION.
  METHOD zpru_if_prompt_provider~get_system_prompt.
    rv_system_prompt = super->zpru_if_prompt_provider~get_system_prompt( ).

    zpru_cl_dummy_agent_logic=>ms_method_registr-get_system_prompt = abap_true.

    rv_system_prompt = |{ rv_system_prompt } CONVENTION ON THE CONTRACT FOR THE INTERNATIONAL { cl_abap_char_utilities=>newline } | &&
                                           | CARRIAGE OF GOODS BY ROAD { cl_abap_char_utilities=>newline } | &&
                                           | (CMR) { cl_abap_char_utilities=>newline } |.

    rv_system_prompt = |{ rv_system_prompt } Consignment note is the same term as CMR { cl_abap_char_utilities=>newline }|.

    rv_system_prompt = |{ rv_system_prompt } Article 4 { cl_abap_char_utilities=>newline } | &&
                                           | The contract of carriage shall be confirmed { cl_abap_char_utilities=>newline } | &&
                                           | by the making out of a consignment note. The { cl_abap_char_utilities=>newline } | &&
                                           | absence, irregularity or loss of the consignment { cl_abap_char_utilities=>newline } | &&
                                           | note shall not affect the existence or the validity { cl_abap_char_utilities=>newline } | &&
                                           | of the contract of carriage which shall remain { cl_abap_char_utilities=>newline } | &&
                                           | subject to the provisions of this Convention. { cl_abap_char_utilities=>newline } |.

    rv_system_prompt = |{ rv_system_prompt } Article 5 { cl_abap_char_utilities=>newline } | &&
                                           | { cl_abap_char_utilities=>newline } | &&
                                           | 1. The consignment note shall be made out { cl_abap_char_utilities=>newline } | &&
                                           | in three original copies signed by the sender { cl_abap_char_utilities=>newline } | &&
                                           | and by the carrier. These signatures may be { cl_abap_char_utilities=>newline } | &&
                                           | printed or replaced by the stamps of the sender { cl_abap_char_utilities=>newline } | &&
                                           | and the carrier if the law of the country in { cl_abap_char_utilities=>newline } | &&
                                           | which the consignment note has been made out { cl_abap_char_utilities=>newline } | &&
                                           | so permits. The first copy shall be handed to { cl_abap_char_utilities=>newline } | &&
                                           | the sender, the second shall accompany the { cl_abap_char_utilities=>newline } | &&
                                           | goods and the third shall be retained by the { cl_abap_char_utilities=>newline } | &&
                                           | carrier. { cl_abap_char_utilities=>newline } | &&
                                           | 2. When the goods which are to be carried { cl_abap_char_utilities=>newline } | &&
                                           | have to be loaded in different vehicles, or are { cl_abap_char_utilities=>newline } | &&
                                           | of different kinds or are divided into different { cl_abap_char_utilities=>newline } | &&
                                           | lots, the sender or the carrier shall have the { cl_abap_char_utilities=>newline } | &&
                                           | right to require a separate consignment note { cl_abap_char_utilities=>newline } | &&
                                           | to be made out for each vehicle used, or for { cl_abap_char_utilities=>newline } | &&
                                           | each kind or lot of goods. { cl_abap_char_utilities=>newline } |.

    rv_system_prompt = |{ rv_system_prompt } Article 6 { cl_abap_char_utilities=>newline } | &&
                                           | 1. The consignment note shall contain the following particulars : { cl_abap_char_utilities=>newline } | &&
                                           | (a) The date of the consignment note and the place at which it is made out; { cl_abap_char_utilities=>newline } | &&
                                           | (b) The name and address of the sender; { cl_abap_char_utilities=>newline } | &&
                                           | (c) The name and address of the carrier; { cl_abap_char_utilities=>newline } | &&
                                           | (d) The place and the date of taking over of the goods and the place designated for delivery; { cl_abap_char_utilities=>newline } | &&
                                           | (e) The name and address of the consignee ; { cl_abap_char_utilities=>newline } | &&
                                           | (f) The description in common use of the nature of the goods and the method of packing, { cl_abap_char_utilities=>newline } | &&
                                           | and, in the case of dangerous goods, their generally recognized description; { cl_abap_char_utilities=>newline } | &&
                                           | (g) The number of packages and their special marks and numbers ; { cl_abap_char_utilities=>newline } | &&
                                           | (h) The gross weight of the goods or their quantity otherwise expressed; { cl_abap_char_utilities=>newline } | &&
                                           | (i) Charges relating to the carriage (carriage charges, supplementary charges, customs { cl_abap_char_utilities=>newline } | &&
                                           | duties and other charges incurred from the making of the contract to the time of delivery) ; { cl_abap_char_utilities=>newline } | &&
                                           | (j) The requisite instructions for Customs and other formalities; { cl_abap_char_utilities=>newline } | &&
                                           | (k) A statement that the carriage is subject, notwithstanding any clause to the contrary, to { cl_abap_char_utilities=>newline } | &&
                                           | the provisions of this Convention. { cl_abap_char_utilities=>newline } | &&
                                           | { cl_abap_char_utilities=>newline } | &&
                                           | 2. Where applicable, the consignment note shall also contain the following particulars : { cl_abap_char_utilities=>newline } | &&
                                           | (a) A statement that trans-shipment is not allowed; { cl_abap_char_utilities=>newline } | &&
                                           | (b) The charges which the sender undertakes to pay; { cl_abap_char_utilities=>newline } | &&
                                           | (c) The amount of "cash on delivery" charges; { cl_abap_char_utilities=>newline } | &&
                                           | (d) A declaration of the value of the goods and the amount representing special interest { cl_abap_char_utilities=>newline } | &&
                                           | in delivery; { cl_abap_char_utilities=>newline } | &&
                                           | (e) The sender's instructions to the carrier regarding insurance of the goods ; { cl_abap_char_utilities=>newline } | &&
                                           | (f) The agreed time-limit within which the carriage is to be carried out; { cl_abap_char_utilities=>newline } | &&
                                           | (g) A list of the documents handed to the carrier. { cl_abap_char_utilities=>newline } | &&
                                           | { cl_abap_char_utilities=>newline } | &&
                                           | 3. The parties may enter in the consignment note any other particulars which they may { cl_abap_char_utilities=>newline } | &&
                                           | deem useful. { cl_abap_char_utilities=>newline } |.

    rv_system_prompt = |{ rv_system_prompt } Article 7 { cl_abap_char_utilities=>newline } | &&
                                           | 1. The sender shall be responsible for all expenses, loss and damage { cl_abap_char_utilities=>newline } | &&
                                           | sustained by the carrier by reason of the inaccuracy or inadequacy of: { cl_abap_char_utilities=>newline } | &&
                                           | (a) The particulars specified in article 6, paragraph 1 (b), (d), (e), (f), (g), (h) and (j); { cl_abap_char_utilities=>newline } | &&
                                           | (b) The particulars specified in article 6, paragraph 2 ; { cl_abap_char_utilities=>newline } | &&
                                           | (c) Any other particulars or instructions given by him to enable the { cl_abap_char_utilities=>newline } | &&
                                           | consignment note to be made out or for the purpose of their being entered therein. { cl_abap_char_utilities=>newline } | &&
                                           | { cl_abap_char_utilities=>newline } | &&
                                           | 2. If, at the request of the sender, the carrier enters in the { cl_abap_char_utilities=>newline } | &&
                                           | consignment note the particulars referred to in paragraph 1 of this { cl_abap_char_utilities=>newline } | &&
                                           | article, he shall be deemed, unless the contrary is proved, to have { cl_abap_char_utilities=>newline } | &&
                                           | done so on behalf of the sender. { cl_abap_char_utilities=>newline } | &&
                                           | { cl_abap_char_utilities=>newline } | &&
                                           | 3. If the consignment note does not contain the statement specified { cl_abap_char_utilities=>newline } | &&
                                           | in article 6, paragraph 1 (k), the carrier shall be liable for all { cl_abap_char_utilities=>newline } | &&
                                           | expenses, loss and damage sustained through such omission by the { cl_abap_char_utilities=>newline } | &&
                                           | person entitled to dispose of the goods. { cl_abap_char_utilities=>newline } |.

    rv_system_prompt = |{ rv_system_prompt } Article 8 { cl_abap_char_utilities=>newline } | &&
                                           | 1. On taking over the goods, the carrier shall check : { cl_abap_char_utilities=>newline } | &&
                                           | (a) The accuracy of the statements in the consignment note as to the { cl_abap_char_utilities=>newline } | &&
                                           | number of packages and their marks and numbers, and { cl_abap_char_utilities=>newline } | &&
                                           | (b) The apparent condition of the goods and their packaging. { cl_abap_char_utilities=>newline } | &&
                                           | { cl_abap_char_utilities=>newline } | &&
                                           | 2. Where the carrier has no reasonable means of checking the accuracy { cl_abap_char_utilities=>newline } | &&
                                           | of the statements referred to in paragraph 1 (a) of this article, he { cl_abap_char_utilities=>newline } | &&
                                           | shall enter his reservations in the consignment note together with { cl_abap_char_utilities=>newline } | &&
                                           | the grounds on which they are based. He shall likewise specify the { cl_abap_char_utilities=>newline } | &&
                                           | grounds for any reservations which he makes with regard to the { cl_abap_char_utilities=>newline } | &&
                                           | apparent condition of the goods and their packaging. Such reservations { cl_abap_char_utilities=>newline } | &&
                                           | shall not bind the sender unless he has expressly agreed to be bound { cl_abap_char_utilities=>newline } | &&
                                           | by them in the consignment note. { cl_abap_char_utilities=>newline } | &&
                                           | { cl_abap_char_utilities=>newline } | &&
                                           | 3. The sender shall be entitled to require the carrier to check the { cl_abap_char_utilities=>newline } | &&
                                           | gross weight of the goods or their quantity otherwise expressed. { cl_abap_char_utilities=>newline } | &&
                                           | He may also require the contents of the packages to be checked. { cl_abap_char_utilities=>newline } | &&
                                           | The carrier shall be entitled to claim the cost of such checking. { cl_abap_char_utilities=>newline } | &&
                                           | The result of the checks shall be entered in the consignment note. { cl_abap_char_utilities=>newline } |.

    rv_system_prompt = |{ rv_system_prompt } Article 9 { cl_abap_char_utilities=>newline } | &&
                                           | 1. The consignment note shall be prima facie evidence of the { cl_abap_char_utilities=>newline } | &&
                                           | making of the contract of carriage, the conditions of the { cl_abap_char_utilities=>newline } | &&
                                           | contract and the receipt of the goods by the carrier. { cl_abap_char_utilities=>newline } | &&
                                           | { cl_abap_char_utilities=>newline } | &&
                                           | 2. If the consignment note contains no specific reservations by { cl_abap_char_utilities=>newline } | &&
                                           | the carrier, it shall be presumed, unless the contrary is proved, { cl_abap_char_utilities=>newline } | &&
                                           | that the goods and their packaging appeared to be in good { cl_abap_char_utilities=>newline } | &&
                                           | condition when the carrier took them over and that the number { cl_abap_char_utilities=>newline } | &&
                                           | of packages, their marks and numbers corresponded with the { cl_abap_char_utilities=>newline } | &&
                                           | statements in the consignment note. |.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_abap_code_tool DEFINITION
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zpru_if_tool_executor.
    INTERFACES zpru_if_abap_executor.
ENDCLASS.


CLASS lcl_abap_code_tool IMPLEMENTATION.
  METHOD zpru_if_abap_executor~execute_code.
    DATA lv_payload    TYPE string.
    DATA lv_risk_score TYPE i.
    DATA lo_agent_util TYPE REF TO zpru_if_agent_util.

    FIELD-SYMBOLS <ls_context> TYPE zpru_cl_dummy_agent_logic=>ts_context.

    zpru_cl_dummy_agent_logic=>ms_method_registr-simple_tool = abap_true.

    IF io_controller->mo_context IS NOT BOUND.
      RETURN.
    ENDIF.

    DATA(lr_context_data) = io_controller->mo_context->get_data( ).
    ASSIGN lr_context_data->* TO <ls_context>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF zpru_cl_stochastic_producer=>get_decision( ) = 1.
      <ls_context>-gate_pass_assessment-is_expected = abap_true.
    ELSE.
      <ls_context>-gate_pass_assessment-is_expected = abap_false.
      lv_risk_score += 1.
    ENDIF.

    IF zpru_cl_stochastic_producer=>get_decision( ) = 1.
      <ls_context>-gate_pass_assessment-is_on_time = abap_true.
    ELSE.
      <ls_context>-gate_pass_assessment-is_on_time = abap_false.
      lv_risk_score += 1.
    ENDIF.

    IF zpru_cl_stochastic_producer=>get_decision( ) = 1.
      <ls_context>-gate_pass_assessment-is_carrier_allowed = abap_true.
    ELSE.
      <ls_context>-gate_pass_assessment-is_carrier_allowed = abap_false.
      lv_risk_score += 1.
    ENDIF.

    IF zpru_cl_stochastic_producer=>get_decision( ) = 1.
      <ls_context>-gate_pass_assessment-is_driver_verified = abap_true.
    ELSE.
      <ls_context>-gate_pass_assessment-is_driver_verified = abap_false.
      lv_risk_score += 1.
    ENDIF.

    CASE zpru_cl_stochastic_producer=>get_stochastic_value( iv_max = 4 ).
      WHEN 1.
        <ls_context>-gate_pass_assessment-assigned_gate = 'GATE01'.
      WHEN 2.
        <ls_context>-gate_pass_assessment-assigned_gate = 'GATE02'.
      WHEN 3.
        <ls_context>-gate_pass_assessment-assigned_gate = 'GATE03'.
      WHEN 4.
        <ls_context>-gate_pass_assessment-assigned_gate = 'GATE04'.
      WHEN OTHERS.
        <ls_context>-gate_pass_assessment-assigned_gate = 'GATE05'.
    ENDCASE.

    IF lv_risk_score <= 1.
      <ls_context>-gate_pass_assessment-risk_score = 'GREEN'.
    ELSEIF lv_risk_score > 1 AND lv_risk_score <= 3.
      <ls_context>-gate_pass_assessment-risk_score = 'YELLOW'.
    ELSE.
      <ls_context>-gate_pass_assessment-risk_score = 'RED'.
    ENDIF.

    TRY.
        lo_agent_util ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_AGENT_UTIL`
                                                                  iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RETURN.
    ENDTRY.

    lo_agent_util->convert_to_string( EXPORTING ir_abap   = REF #( <ls_context>-gate_pass_assessment )
                                      CHANGING  cr_string = <ls_context>-gate_pass_assessment_json ).

    lv_payload = io_request->get_data( )->*.
    lv_payload = |{ lv_payload } - SIMPLE TOOL VISITED - { <ls_context>-gate_pass_assessment_json }|.
    eo_response->set_data( ir_data = NEW string( lv_payload ) ).
  ENDMETHOD.
ENDCLASS.


CLASS lcl_knowledge DEFINITION
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zpru_if_tool_executor.
    INTERFACES zpru_if_knowledge_provider.
ENDCLASS.


CLASS lcl_knowledge IMPLEMENTATION.
  METHOD zpru_if_knowledge_provider~lookup_knowledge.
    DATA lv_payload    TYPE string.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA lt_safety_rag TYPE zpru_cl_dummy_agent_logic=>tt_safety_knowledge.

    lt_safety_rag = VALUE #(
        " -----------------------------------------------------------------------
        " PROTOCOL 1: FLAMMABLE LIQUIDS
        " -----------------------------------------------------------------------
        ( name               = 'PROT-FLAM-01'
          description        = 'Handling and discharge of Class 3 Flammable Liquids'
          date_updated       = '20250115'
          responsible_person = 'Karl Schmidt'
          rules              = VALUE #(
              ( rule_number      = 1
                rule_name        = 'Static Grounding'
                rule_explanation = 'Chassis must be grounded to a certified earth point before opening valves.' )
              ( rule_number      = 2
                rule_name        = 'Vapor Recovery'
                rule_explanation = 'Connect vapor return lines to prevent atmospheric release during discharge.' )
              ( rule_number      = 3
                rule_name        = 'Ignition Control'
                rule_explanation = 'Ensure all mobile devices and non-EX rated equipment are removed from the zone.' )
              ( rule_number      = 4
                rule_name        = 'Spill Containment'
                rule_explanation = 'Deploy secondary containment bunds beneath all flexible hose connections.' )
              ( rule_number      = 5
                rule_name        = 'Fire Suppression'
                rule_explanation = 'Position two 9kg foam extinguishers within 5 meters of the discharge point.' )
              ( rule_number      = 6
                rule_name        = 'Emergency Cut-off'
                rule_explanation = 'Test the remote emergency shutdown valve prior to beginning the transfer.' )
              ( rule_number      = 7
                rule_name        = 'Personal PPE'
                rule_explanation = 'Flame-retardant (EN ISO 11612) coveralls and anti-static boots mandatory.' )
              ( rule_number      = 8
                rule_name        = 'Wind Monitoring'
                rule_explanation = 'Cease operations if cross-wind speeds exceed 40 km/h to avoid vapor drift.' )
              ( rule_number      = 9
                rule_name        = 'Traffic Management'
                rule_explanation = 'Erect No Entry barriers 20 meters around the vehicle perimeter.' )
              ( rule_number      = 10
                rule_name        = 'Flash Point Review'
                rule_explanation = 'Verify flash point on CMR matches Material Safety Data Sheet (MSDS) limits.' ) ) )

        " -----------------------------------------------------------------------
        " PROTOCOL 2: TOXIC SUBSTANCES
        " -----------------------------------------------------------------------
        ( name               = 'PROT-TOX-02'
          description        = 'Safe management of Category 2 Toxic Substances'
          date_updated       = '20241110'
          responsible_person = 'Dr. Elena Rossi'
          rules              = VALUE #(
              ( rule_number      = 1
                rule_name        = 'Respiratory PPE'
                rule_explanation = 'Level B hazmat suit with self-contained breathing apparatus (SCBA) mandatory.' )
              ( rule_number      = 2
                rule_name        = 'Decontamination'
                rule_explanation = 'Setup chemical wash station at Exit Point Alpha for all personnel.' )
              ( rule_number      = 3
                rule_name        = 'Air Quality Sensor'
                rule_explanation = 'Deploy portable VOC sensors to monitor for invisible gas leaks.' )
              ( rule_number      = 4
                rule_name        = 'Seal Inspection'
                rule_explanation = 'Check all drum gaskets for degradation or sweating before movement.' )
              ( rule_number      = 5
                rule_name        = 'Antidote Kit'
                rule_explanation = 'Ensure the specific medical antidote kit is available in the first aid station.' )
              ( rule_number      = 6
                rule_name        = 'Dual-Man Rule'
                rule_explanation = 'No personnel may enter the storage zone alone; must use buddy system.' )
              ( rule_number      = 7
                rule_name        = 'Ventilation'
                rule_explanation = 'Activate high-volume forced air extraction 15 minutes prior to entry.' )
              ( rule_number      = 8
                rule_name        = 'Contact Log'
                rule_explanation = 'Maintain a timed log of every person entering the hazardous perimeter.' )
              ( rule_number      = 9
                rule_name        = 'Ingestion Hazard'
                rule_explanation = 'Strict prohibition of food, drink, or cosmetics in the handling area.' )
              ( rule_number = 10 rule_name = 'Bio-Hazard Waste'    rule_explanation = 'Dispose of all used PPE in dedicated sealed yellow containers only.' ) ) )

        " -----------------------------------------------------------------------
        " PROTOCOL 3: CORROSIVE MATERIALS
        " -----------------------------------------------------------------------
        ( name               = 'PROT-CORR-03'
          description        = 'Handling of Acids and Caustic Alkaline Bases'
          date_updated       = '20250301'
          responsible_person = 'Marcus Weber'
          rules              = VALUE #(
              ( rule_number      = 1
                rule_name        = 'Lifting Gear'
                rule_explanation = 'Use only acid-resistant synthetic slings; do not use metal chains.' )
              ( rule_number      = 2
                rule_name        = 'Spill Neutralizer'
                rule_explanation = 'Ensure 50kg of soda ash or neutralizing agent is staged at the bay.' )
              ( rule_number      = 3
                rule_name        = 'Eye Wash Access'
                rule_explanation = 'Verify clear path to emergency eye-wash station (must be reached in 10s).' )
              ( rule_number      = 4
                rule_name        = 'Separation Rule'
                rule_explanation = 'Maintain minimum 5-meter distance from any Class 3 flammable goods.' )
              ( rule_number      = 5
                rule_name        = 'Corrosion Shield'
                rule_explanation = 'Apply protective coverings to exposed warehouse racking pillars.' )
              ( rule_number      = 6
                rule_name        = 'Hand Protection'
                rule_explanation = 'Use elbow-length chemical resistant gloves (Butyl or Viton).' )
              ( rule_number      = 7
                rule_name        = 'Leak Detection'
                rule_explanation = 'Use pH indicator strips to test any unidentified moisture on pallets.' )
              ( rule_number      = 8
                rule_name        = 'Drum Orientation'
                rule_explanation = 'Ensure bung holes are upright and tightened to specific torque limits.' )
              ( rule_number      = 9
                rule_name        = 'Splashing Prevention'
                rule_explanation = 'Lower palletized goods at a speed of less than 0.5 meters per second.' )
              ( rule_number = 10 rule_name = 'Surface Wash'        rule_explanation = 'Rinse the trailer floor with water after unloading is complete.' ) ) )

        " -----------------------------------------------------------------------
        " PROTOCOL 4: GENERAL CARGO
        " -----------------------------------------------------------------------
        ( name               = 'PROT-GEN-04'
          description        = 'Standard Procedures for Non-Hazardous Industrial Goods'
          date_updated       = '20250520'
          responsible_person = 'Sarah Jenkins'
          rules              = VALUE #(
              ( rule_number      = 1
                rule_name        = 'Visual Inspection'
                rule_explanation = 'Check for external signs of water damage, crushed corners, or holes.' )
              ( rule_number      = 2
                rule_name        = 'Basic PPE'
                rule_explanation = 'High-visibility vest, steel-toe boots, and hard hat are mandatory.' )
              ( rule_number      = 3
                rule_name        = 'Pallet Stability'
                rule_explanation = 'Verify that shrink-wrap is intact and cargo has not shifted in transit.' )
              ( rule_number      = 4
                rule_name        = 'Weight Verification'
                rule_explanation = 'Compare CMR gross weight against vehicle scale ticket (tolerance 2%).' )
              ( rule_number      = 5
                rule_name        = 'Forklift Safety'
                rule_explanation = 'Only certified operators may enter the trailer with a motorized lift.' )
              ( rule_number      = 6
                rule_name        = 'Documentation'
                rule_explanation = 'Confirm CMR Box 24 is signed and dated by the receiving clerk.' )
              ( rule_number      = 7
                rule_name        = 'Debris Clearing'
                rule_explanation = 'Remove all loose dunnage, straps, and nails from the unloading area.' )
              ( rule_number      = 8
                rule_name        = 'Barcode Scanning'
                rule_explanation = 'Scan all 50 units into the system to confirm 100% receipt.' )
              ( rule_number      = 9
                rule_name        = 'Temperature Check'
                rule_explanation = 'Note any unusual heat radiating from electronics or battery cargo.' )
              ( rule_number      = 10
                rule_name        = 'Stowage Sequence'
                rule_explanation = 'Follow First-In-First-Out (FIFO) logic for shelf placement.' ) ) ) ).

    zpru_cl_dummy_agent_logic=>ms_method_registr-knowledge = abap_true.
    GET TIME STAMP FIELD DATA(lv_now).
    lv_payload = io_request->get_data( )->*.
    lv_payload = |{ lv_payload } - KNOWLEDGE TOOL VISITED - { lv_now }|.
    eo_response->set_data( ir_data = NEW string( lv_payload ) ).
  ENDMETHOD.
ENDCLASS.


CLASS lcl_nested_agent DEFINITION
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zpru_if_tool_executor.
    INTERFACES zpru_if_nested_agent_runner.
ENDCLASS.


CLASS lcl_nested_agent IMPLEMENTATION.
  METHOD zpru_if_nested_agent_runner~run_nested_agent.
    DATA lv_payload TYPE string.

    zpru_cl_dummy_agent_logic=>ms_method_registr-nested_agent = abap_true.
    GET TIME STAMP FIELD DATA(lv_now).
    lv_payload = io_request->get_data( )->*.
    lv_payload = |{ lv_payload } - NESTED AGENT TOOL VISITED - { lv_now }|.
    eo_response->set_data( ir_data = NEW string( lv_payload ) ).
  ENDMETHOD.
ENDCLASS.


CLASS lcl_input_schema_provider DEFINITION
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zpru_if_input_schema_provider.
ENDCLASS.


CLASS lcl_input_schema_provider IMPLEMENTATION.
  METHOD zpru_if_input_schema_provider~get_input_schema.
    DATA lv_input_schema TYPE string.

    zpru_cl_dummy_agent_logic=>ms_method_registr-get_input_schema = abap_true.
    CASE is_tool_master_data-tool_name.
      WHEN 'SIMPLE_TOOL'.
        lv_input_schema = |SIMPLE_TOOL_SCHEMA|.
      WHEN 'KNOWLEDGE'.
        lv_input_schema = |KNOWLEDGE_SCHEMA"|.
      WHEN 'NESTED_AGENT'.
        lv_input_schema = |NESTED_AGENT_SCHEMA|.
      WHEN OTHERS.
    ENDCASE.
    ro_input_schema->set_data( ir_data = NEW string( lv_input_schema ) ).
  ENDMETHOD.
ENDCLASS.


" Local class for HTTP Request tool
CLASS lcl_http_request_tool DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zpru_if_tool_executor.
    INTERFACES zpru_if_http_request_sender.

  PROTECTED SECTION.
    METHODS get_http_client
      IMPORTING iv_url                TYPE string
      RETURNING VALUE(ro_http_client) TYPE REF TO if_web_http_client.

    METHODS send_via_url
      IMPORTING io_controller TYPE REF TO zpru_if_agent_controller
                io_request    TYPE REF TO zpru_if_payload
      EXPORTING eo_response   TYPE REF TO zpru_if_payload
                ev_error_flag TYPE abap_boolean.
ENDCLASS.


CLASS lcl_http_request_tool IMPLEMENTATION.
  METHOD zpru_if_http_request_sender~send_http.
    send_via_url( EXPORTING io_controller = io_controller
                            io_request    = io_request
                  IMPORTING eo_response   = eo_response
                            ev_error_flag = ev_error_flag ).
  ENDMETHOD.

  METHOD send_via_url.
    " TODO: parameter IO_CONTROLLER is never used (ABAP cleaner)
    " TODO: parameter EO_RESPONSE is never cleared or assigned (ABAP cleaner)
    " TODO: parameter EV_ERROR_FLAG is never cleared or assigned (ABAP cleaner)

    DATA lv_url         TYPE string.
    DATA lo_http_client TYPE REF TO if_web_http_client.
    DATA lo_response    TYPE REF TO if_web_http_response.
    DATA lo_util        TYPE REF TO zpru_if_agent_util.

    lv_url = 'https://www.youtube.com/watch?v=bkCQK-rROWk'.

    TRY.

        lo_http_client = get_http_client( lv_url ).

        lo_http_client->get_http_request( )->set_header_fields(
            i_fields = VALUE #( value = if_web_http_header=>accept_application_json
                                ( name = if_web_http_header=>content_type )
                                ( name = if_web_http_header=>accept ) ) ).

        lo_response = lo_http_client->execute( if_web_http_client=>get ).

        DATA(lv_status) = lo_response->get_status( ).
        IF lv_status-code <> '200'.
          " raise exception
        ENDIF.

        DATA(lv_response_json) = lo_response->get_text( ).

        DATA(lv_input_json) = io_request->get_data( ).

        TRY.
            lo_util ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_AGENT_UTIL`
                                                                iv_context = zpru_if_agent_frw=>cs_context-standard ).
          CATCH zpru_cx_agent_core.
            RETURN.
        ENDTRY.

        DATA(lv_output) = lo_util->append_json_to_json( iv_field_4_append = 'http_result'
                                                        iv_json_4_append  = lv_response_json
                                                        iv_json_target    = lv_input_json->*  ).

        eo_response->set_data( ir_data = NEW string( lv_output ) ).

      CATCH cx_http_dest_provider_error
            cx_web_http_client_error.
    ENDTRY.
  ENDMETHOD.

  METHOD get_http_client.
    DATA lo_http_destination TYPE REF TO if_http_destination.

    IF zpru_cl_logic_switch=>get_logic( ) = abap_true.
      ro_http_client = NEW zpru_cl_web_http_client( ).
    ELSE.
      TRY.
          lo_http_destination = cl_http_destination_provider=>create_by_url( i_url = iv_url ).
          ro_http_client = cl_web_http_client_manager=>create_by_http_destination(
                               i_destination = lo_http_destination ).
        CATCH cx_http_dest_provider_error
              cx_web_http_client_error.
      ENDTRY.

    ENDIF.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_service_cons_model_tool DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zpru_if_tool_executor.
    INTERFACES zpru_if_service_model_consumer.
ENDCLASS.


CLASS lcl_service_cons_model_tool IMPLEMENTATION.
  METHOD zpru_if_service_model_consumer~consume_service_model.
    DATA lt_business_data         TYPE TABLE OF zpru_storage_bin=>tys_warehouse_storage_bin_type.
    DATA lo_http_client           TYPE REF TO if_web_http_client.
    DATA lo_client_proxy          TYPE REF TO /iwbep/if_cp_client_proxy.
    DATA lo_request               TYPE REF TO /iwbep/if_cp_request_read_list.
    DATA lo_response              TYPE REF TO /iwbep/if_cp_response_read_lst.
    DATA lo_filter_factory        TYPE REF TO /iwbep/if_cp_filter_factory.
    DATA lo_filter_node_1         TYPE REF TO /iwbep/if_cp_filter_node.
    DATA lo_filter_node_2         TYPE REF TO /iwbep/if_cp_filter_node.
    DATA lo_filter_node_root      TYPE REF TO /iwbep/if_cp_filter_node.
    DATA lt_range_ewmwarehouse    TYPE RANGE OF char4.
    DATA lt_range_ewmstorage_bin  TYPE RANGE OF char18.
    DATA lv_comm_scenario         TYPE if_com_management=>ty_cscn_id.
    DATA lv_service_id            TYPE if_com_management=>ty_cscn_outb_srv_id.
    DATA lv_comm_system_id        TYPE if_com_management=>ty_cs_id.
    DATA lv_repository_id         TYPE /iwbep/if_cp_runtime_types=>ty_proxy_model_repo_id.
    DATA lv_proxy_model_id        TYPE /iwbep/if_cp_runtime_types=>ty_proxy_model_id.
    DATA lv_proxy_model_version   TYPE /iwbep/if_cp_runtime_types=>ty_proxy_model_version.
    DATA lv_relative_service_root TYPE string.
    DATA lo_util                  TYPE REF TO zpru_if_agent_util.
    DATA lv_response_json         TYPE string.

    lv_comm_scenario = 'SAP_COM_0550'.
    lv_service_id = 'API_WHSE_STORAGE_BIN_2'.
    lv_comm_system_id = 'S4H_EXT_SYSTEM'.
    lv_repository_id = 'DEFAULT'.
    lv_proxy_model_id = 'ZPRU_STORAGE_BIN'.
    lv_proxy_model_version = '0001'.
    lv_relative_service_root = '/sap/opu/odata4/sap/api_whse_storage_bin_2/srvd_a2x/sap/warehousestoragebin/0001'.

    TRY.
        DATA(lo_destination) = cl_http_destination_provider=>create_by_comm_arrangement(
                                   comm_scenario  = lv_comm_scenario
                                   comm_system_id = lv_comm_system_id
                                   service_id     = lv_service_id ).
        lo_http_client = cl_web_http_client_manager=>create_by_http_destination( lo_destination ).
        lo_client_proxy = /iwbep/cl_cp_factory_remote=>create_v4_remote_proxy(
                              is_proxy_model_key       = VALUE #( repository_id       = lv_repository_id
                                                                  proxy_model_id      = lv_proxy_model_id
                                                                  proxy_model_version = lv_proxy_model_version )
                              io_http_client           = lo_http_client
                              iv_relative_service_root = lv_relative_service_root ).
        ASSERT lo_http_client IS BOUND.

        lo_request = lo_client_proxy->create_resource_for_entity_set( 'WAREHOUSE_STORAGE_BIN' )->create_request_for_read( ).

        lo_filter_factory = lo_request->create_filter_factory( ).
        lo_filter_node_1  = lo_filter_factory->create_by_range( iv_property_path = 'EWMWAREHOUSE'
                                                                it_range         = lt_range_ewmwarehouse ).
        lo_filter_node_2  = lo_filter_factory->create_by_range( iv_property_path = 'EWMSTORAGE_BIN'
                                                                it_range         = lt_range_ewmstorage_bin ).
        lo_filter_node_root = lo_filter_node_1->and( lo_filter_node_2 ).
        lo_request->set_filter( lo_filter_node_root ).
        lo_request->set_top( 50 )->set_skip( 0 ).

        lo_response = lo_request->execute( ).
        lo_response->get_business_data( IMPORTING et_business_data = lt_business_data ).

        DATA(lv_input_json) = io_request->get_data( ).

        TRY.
            lo_util ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_AGENT_UTIL`
                                                                iv_context = zpru_if_agent_frw=>cs_context-standard ).
          CATCH zpru_cx_agent_core.
            RETURN.
        ENDTRY.

        lo_util->convert_to_string( EXPORTING ir_abap   = REF #( lt_business_data )
                                    CHANGING  cr_string = lv_response_json ).

        DATA(lv_output) = lo_util->append_json_to_json( iv_field_4_append = 'csm_result'
                                                        iv_json_4_append  = lv_response_json
                                                        iv_json_target    = lv_input_json->*  ).

        eo_response->set_data( ir_data = NEW string( lv_output ) ).

      CATCH /iwbep/cx_cp_remote INTO DATA(lx_remote).
        RAISE SHORTDUMP lx_remote.
      CATCH /iwbep/cx_gateway INTO DATA(lx_gateway).
        RAISE SHORTDUMP lx_gateway.
      CATCH cx_http_dest_provider_error INTO DATA(lx_dest_provider_error).
        RAISE SHORTDUMP lx_dest_provider_error.
      CATCH cx_web_http_client_error INTO DATA(lx_web_http_client_error).
        RAISE SHORTDUMP lx_web_http_client_error.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.


" Local class for Call LLM tool
CLASS lcl_call_llm_tool DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zpru_if_tool_executor.
    INTERFACES zpru_if_llm_caller.

    TYPES: BEGIN OF ts_result_payload,
             llm_response               TYPE string,
             llm_total_tokens           TYPE i,
             llm_finish_reason          TYPE aic_finish_reason=>type,
             llm_original_finish_reason TYPE string,
           END OF ts_result_payload.

  PROTECTED SECTION.
    METHODS get_llm_api_factory
      RETURNING VALUE(ro_llm_api_factory) TYPE REF TO if_aic_islm_compl_api_factory.

    METHODS prepare_prompt
      IMPORTING io_llm_api           TYPE REF TO if_aic_completion_api
                iv_system_role       TYPE string
                iv_user_message      TYPE string
                iv_assistant_message TYPE string
                iv_user_message_2    TYPE string
      RETURNING VALUE(ro_message)    TYPE REF TO if_aic_message_container.

    METHODS get_response_schema
      RETURNING VALUE(rv_response_schema) TYPE  string.

    METHODS preprocess_llm_request
      IMPORTING io_controller    TYPE REF TO zpru_if_agent_controller
                io_request       TYPE REF TO zpru_if_payload
                iv_islm_scenario TYPE aic_islm_scenario_id=>type
      EXPORTING eo_message       TYPE REF TO if_aic_message_container
                eo_llm_api       TYPE REF TO if_aic_completion_api
                ev_error_flag    TYPE abap_boolean.

    METHODS process_llm_request
      IMPORTING io_controller TYPE REF TO zpru_if_agent_controller
                io_request    TYPE REF TO zpru_if_payload
                io_message    TYPE REF TO if_aic_message_container
                io_llm_api    TYPE REF TO if_aic_completion_api
      EXPORTING eo_response   TYPE REF TO zpru_if_payload
                ev_error_flag TYPE abap_boolean.
ENDCLASS.


CLASS lcl_call_llm_tool IMPLEMENTATION.
  METHOD zpru_if_llm_caller~call_large_language_model.
    DATA lo_llm_api TYPE REF TO if_aic_completion_api.
    DATA lo_message TYPE REF TO if_aic_message_container.

    preprocess_llm_request( EXPORTING iv_islm_scenario = `MY_GEMINI_3`
                                      io_controller    = io_controller
                                      io_request       = io_request
                            IMPORTING eo_message       = lo_message
                                      eo_llm_api       = lo_llm_api
                                      ev_error_flag    = ev_error_flag ).

    IF ev_error_flag = abap_true.
      RETURN.
    ENDIF.

    process_llm_request( EXPORTING io_controller = io_controller
                                   io_request    = io_request
                                   io_message    = lo_message
                                   io_llm_api    = lo_llm_api
                         IMPORTING eo_response   = eo_response
                                   ev_error_flag = ev_error_flag ).
  ENDMETHOD.

  METHOD get_llm_api_factory.
    IF zpru_cl_logic_switch=>get_logic( ) = abap_true.
      ro_llm_api_factory = NEW zpru_cl_islm_compl_api_factory( ).
    ELSE.
      TRY.
          ro_llm_api_factory = cl_aic_islm_compl_api_factory=>get( ).
        CATCH cx_aic_api_factory.
          RETURN.
      ENDTRY.
    ENDIF.
  ENDMETHOD.

  METHOD prepare_prompt.
    ro_message = io_llm_api->create_message_container( ).
    ro_message->set_system_role( iv_system_role ).
    ro_message->add_user_message( iv_user_message ).
    ro_message->add_assistant_message( iv_assistant_message ).
    ro_message->add_user_message( iv_user_message_2 ).
  ENDMETHOD.

  METHOD get_response_schema.
    " {
    "   "type": "object",
    "   "properties": {
    "     "explanation": {
    "       "type": "string",
    "       "description": "Short technical explanation of the solution."
    "     },
    "     "abap_code": {
    "       "type": "string",
    "       "description": "The executable ABAP code block."
    "     },
    "     "objects_used": {
    "       "type": "array",
    "       "items": { "type": "string" },
    "       "description": "List of SAP standard tables or classes mentioned."
    "     },
    "     "confidence_score": {
    "       "type": "number",
    "       "description": "Model's certainty in this answer from 0 to 1."
    "     }
    "   },
    "   "required": ["explanation", "abap_code", "objects_used"]
    " }

    rv_response_schema =
       |\{ | &&
       |  "type": "object", | &&
       |  "properties": \{ | &&
       |    "explanation": \{ "type": "string" \}, | &&
       |    "abap_code": \{ "type": "string" \}, | &&
       |    "objects_used": \{ "type": "array", "items": \{ "type": "string" \} \} | &&
       |  \}, | &&
       |  "required": ["explanation", "abap_code", "objects_used"] | &&
       |\}|.
  ENDMETHOD.

  METHOD preprocess_llm_request.
    " TODO: parameter IO_CONTROLLER is never used (ABAP cleaner)
    " TODO: parameter IO_REQUEST is never used (ABAP cleaner)

    DATA lo_llm_parameter TYPE REF TO if_aic_completion_parameters.

    CLEAR: eo_message,
           eo_llm_api.

    ev_error_flag = abap_false.

    DATA(lo_llm_api_factory) = get_llm_api_factory( ).

    TRY.
        eo_llm_api = lo_llm_api_factory->create_instance( iv_islm_scenario ).
      CATCH cx_aic_api_factory.
        ev_error_flag = abap_true.
        RETURN.
    ENDTRY.

    IF eo_llm_api IS NOT BOUND.
      ev_error_flag = abap_true.
      RETURN.
    ENDIF.

    eo_message = prepare_prompt( io_llm_api           = eo_llm_api
                                 iv_system_role       = `You are an ABAP expert`
                                 iv_user_message      = `Does ABAP support OO programming?`
                                 iv_assistant_message = `Yes`
                                 iv_user_message_2    = `Can you build RESTful applications in ABAP?` ).

    lo_llm_parameter = eo_llm_api->get_parameter_setter( ).
    lo_llm_parameter->set_temperature( `1.0` ).
    lo_llm_parameter->set_maximum_tokens( 2000 ).
    lo_llm_parameter->set_any_parameter( name  = `responseMimeType`
                                         value = `application/json` ).
    lo_llm_parameter->set_any_parameter( name  = `thinking_level`
                                         value = `high` ).

    DATA(lv_schema) = get_response_schema( ).
    lo_llm_parameter->set_any_parameter( name  = `responseSchema`
                                         value = lv_schema ).

    lo_llm_parameter->set_any_parameter( name  = `tools`
                                         value = `[{ "google_search": {} }]` ).
  ENDMETHOD.

  METHOD process_llm_request.
    " TODO: parameter IO_CONTROLLER is never used (ABAP cleaner)
    " TODO: parameter EO_RESPONSE is never cleared or assigned (ABAP cleaner)
    " TODO: parameter EV_ERROR_FLAG is never cleared or assigned (ABAP cleaner)

    DATA ls_result_payload TYPE ts_result_payload.
    DATA lo_llm_result     TYPE REF TO if_aic_completion_api_result.
    DATA lo_util           TYPE REF TO zpru_if_agent_util.
    DATA lv_json_2_append  TYPE zpru_if_agent_frw=>ts_json.

    TRY.
        lo_llm_result = io_llm_api->execute_for_messages( io_message ).
        ls_result_payload-llm_response = lo_llm_result->get_completion( ).

      CATCH cx_aic_completion_api.
        RETURN.
    ENDTRY.

    ls_result_payload-llm_total_tokens           = lo_llm_result->get_total_token_count( ).
    ls_result_payload-llm_finish_reason          = lo_llm_result->get_finish_reason( ).
    ls_result_payload-llm_original_finish_reason = lo_llm_result->get_original_finish_reason( ).

    lo_util->convert_to_string( EXPORTING ir_abap   = REF #( ls_result_payload )
                                CHANGING  cr_string = lv_json_2_append ).

    DATA(lv_input_json) = io_request->get_data( ).

    TRY.
        lo_util ?= zpru_cl_agent_service_mngr=>get_service( iv_service = `ZPRU_IF_AGENT_UTIL`
                                                            iv_context = zpru_if_agent_frw=>cs_context-standard ).
      CATCH zpru_cx_agent_core.
        RETURN.
    ENDTRY.

    DATA(lv_output) = lo_util->append_json_to_json( iv_field_4_append = 'llm_result'
                                                    iv_json_4_append  = lv_json_2_append
                                                    iv_json_target    = lv_input_json->*  ).

    eo_response->set_data( ir_data = NEW string( lv_output ) ).
  ENDMETHOD.
ENDCLASS.


CLASS lcl_dynamic_abap_code_tool DEFINITION INHERITING FROM zpru_cl_dynamic_abap_base CREATE PUBLIC.
ENDCLASS.


CLASS lcl_dynamic_abap_code_tool IMPLEMENTATION.
ENDCLASS.


CLASS lcl_ml_model_inference DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zpru_if_tool_executor.
    INTERFACES zpru_if_ml_model_inference.
ENDCLASS.


CLASS lcl_ml_model_inference IMPLEMENTATION.
  METHOD zpru_if_ml_model_inference~get_machine_learning_inference.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_user_tool DEFINITION CREATE PUBLIC.
  PUBLIC SECTION.
    INTERFACES zpru_if_tool_executor.
    INTERFACES zpru_if_user_tool.
ENDCLASS.


CLASS lcl_user_tool IMPLEMENTATION.
  METHOD zpru_if_user_tool~execute_user_tool.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_tool_provider DEFINITION
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zpru_if_tool_provider.
ENDCLASS.


CLASS lcl_tool_provider IMPLEMENTATION.
  METHOD zpru_if_tool_provider~get_tool.
    zpru_cl_dummy_agent_logic=>ms_method_registr-get_tool = abap_true.

    CASE is_tool_master_data-tool_name.

      WHEN zpru_if_adf_type_and_constant=>cs_step_type-nested_agent.
        ro_executor = NEW lcl_nested_agent( ).

      WHEN zpru_if_adf_type_and_constant=>cs_step_type-knowledge_source.
        ro_executor = NEW lcl_knowledge( ).

      WHEN zpru_if_adf_type_and_constant=>cs_step_type-abap_code.
        ro_executor = NEW lcl_abap_code_tool( ).

      WHEN zpru_if_adf_type_and_constant=>cs_step_type-http_request.
        ro_executor = NEW lcl_http_request_tool( ).

      WHEN zpru_if_adf_type_and_constant=>cs_step_type-service_consumption_model.
        ro_executor = NEW lcl_service_cons_model_tool( ).

      WHEN zpru_if_adf_type_and_constant=>cs_step_type-call_llm.
        ro_executor = NEW lcl_call_llm_tool( ).

      WHEN zpru_if_adf_type_and_constant=>cs_step_type-dynamic_abap_code.
        ro_executor = NEW lcl_dynamic_abap_code_tool( ).

      WHEN zpru_if_adf_type_and_constant=>cs_step_type-infer_ml_model.
        ro_executor = NEW lcl_ml_model_inference( ).

      WHEN zpru_if_adf_type_and_constant=>cs_step_type-user_tool.
        ro_executor = NEW lcl_user_tool( ).

      WHEN OTHERS.
        RETURN.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
