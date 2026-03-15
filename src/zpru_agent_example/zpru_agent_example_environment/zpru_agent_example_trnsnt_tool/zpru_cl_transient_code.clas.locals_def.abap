
CLASS lcl_adf_abap_executor DEFINITION INHERITING FROM zpru_cl_abap_executor CREATE PUBLIC.
  PROTECTED SECTION.
    METHODS execute_code_int REDEFINITION.
ENDCLASS.


CLASS lcl_adf_tool_provider DEFINITION INHERITING FROM zpru_cl_tool_provider CREATE PUBLIC.
  PROTECTED SECTION.
    METHODS provide_tool_instance REDEFINITION.
ENDCLASS.


CLASS lcl_adf_tool_info_provider DEFINITION INHERITING FROM zpru_cl_tool_info_provider CREATE PUBLIC.
  PROTECTED SECTION.
    METHODS get_main_tool_info  REDEFINITION.
    METHODS set_tool_properties REDEFINITION.
    METHODS set_tool_parameters REDEFINITION.
ENDCLASS.


CLASS lcl_adf_schema_provider DEFINITION INHERITING FROM zpru_cl_tool_schema_provider CREATE PUBLIC.

  PROTECTED SECTION.
    METHODS get_input_abap_type    REDEFINITION.
    METHODS get_input_json_schema  REDEFINITION.
    METHODS get_output_abap_type   REDEFINITION.
    METHODS get_output_json_schema REDEFINITION.
        METHODS create_json_schema_example
    EXPORTING eV_json_schema      TYPE zpru_if_agent_frw=>ts_json
              eS_json_structure   TYPE zpru_s_json_schema
      RAISING   zpru_cx_agent_core.
ENDCLASS.
