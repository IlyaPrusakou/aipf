CLASS zpru_cl_adf_buffer DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
  INTERFACES zpru_if_agent_frw.
    TYPES: BEGIN OF ts_agent,
             instance TYPE zpru_agent,
             changed  TYPE abap_bool,
             deleted  TYPE abap_bool,
           END OF ts_agent.

    TYPES: BEGIN OF ts_tool,
             instance TYPE zpru_agent_tool,
             changed  TYPE abap_bool,
             deleted  TYPE abap_bool,
           END OF ts_tool.

    TYPES tt_agent TYPE TABLE OF ts_agent WITH EMPTY KEY.
    TYPES tt_tool  TYPE TABLE OF ts_tool WITH EMPTY KEY.

    CLASS-DATA agent_buffer TYPE tt_agent.
    CLASS-DATA tool_buffer  TYPE tt_tool.

    TYPES: BEGIN OF ts_agent_keys,
             agent_uuid TYPE zpru_agent-agent_uuid,
           END OF ts_agent_keys.

    TYPES: BEGIN OF ts_tool_keys,
             agent_uuid TYPE zpru_agent_tool-agent_uuid,
             tool_uuid  TYPE zpru_agent_tool-tool_uuid,
             full_key   TYPE abap_bool,
           END OF ts_tool_keys.

    TYPES tt_agent_keys TYPE TABLE OF ts_agent_keys WITH EMPTY KEY.
    TYPES tt_tool_keys  TYPE TABLE OF ts_tool_keys WITH EMPTY KEY.

    CLASS-METHODS prep_agent_buffer
      IMPORTING !keys TYPE tt_agent_keys.

    CLASS-METHODS prep_tool_buffer
      IMPORTING !keys TYPE tt_tool_keys.

ENDCLASS.

CLASS zpru_cl_adf_buffer IMPLEMENTATION.
  METHOD prep_agent_buffer.
    DATA ls_line TYPE zpru_agent.

    LOOP AT keys ASSIGNING FIELD-SYMBOL(<ls_key>).

      IF line_exists( zpru_cl_adf_buffer=>agent_buffer[ instance-agent_uuid = <ls_key>-agent_uuid ] ).
        " do nothing
      ELSE.
        SELECT SINGLE @abap_true FROM zpru_agent
          WHERE agent_uuid = @<ls_key>-agent_uuid
          INTO @DATA(lv_exists).
        IF lv_exists = abap_true.
          SELECT SINGLE * FROM zpru_agent
            WHERE agent_uuid = @<ls_key>-agent_uuid
            INTO CORRESPONDING FIELDS OF @ls_line.
          IF sy-subrc = 0.
            APPEND VALUE #( instance = ls_line ) TO zpru_cl_adf_buffer=>agent_buffer.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD prep_tool_buffer.
    DATA lt_child_tab  TYPE TABLE OF zpru_agent_tool WITH EMPTY KEY.
    DATA ls_child_line TYPE zpru_agent_tool.

    LOOP AT keys ASSIGNING FIELD-SYMBOL(<ls_key_child>).
      IF <ls_key_child>-full_key = abap_true.
        IF line_exists( zpru_cl_adf_buffer=>tool_buffer[ instance-agent_uuid = <ls_key_child>-agent_uuid
                                                         instance-tool_uuid  = <ls_key_child>-tool_uuid ] ).
          " do nothing
        ELSE.
          SELECT SINGLE @abap_true FROM zpru_agent_tool
            WHERE agent_uuid = @<ls_key_child>-agent_uuid
              AND tool_uuid  = @<ls_key_child>-tool_uuid
            INTO @DATA(lv_exists).
          IF lv_exists = abap_true.
            SELECT SINGLE * FROM zpru_agent_tool
              WHERE agent_uuid = @<ls_key_child>-agent_uuid
                AND tool_uuid  = @<ls_key_child>-tool_uuid
              INTO CORRESPONDING FIELDS OF @ls_child_line.
            IF sy-subrc = 0.
              APPEND VALUE #( instance = ls_child_line ) TO zpru_cl_adf_buffer=>tool_buffer.
            ENDIF.
          ENDIF.
        ENDIF.

      ELSE.
        IF     line_exists( zpru_cl_adf_buffer=>agent_buffer[ instance-agent_uuid = <ls_key_child>-agent_uuid ] )
           AND VALUE #( zpru_cl_adf_buffer=>agent_buffer[ instance-agent_uuid = <ls_key_child>-agent_uuid ]-deleted OPTIONAL ) IS NOT INITIAL.
          " do nothing
        ELSE.
          SELECT SINGLE @abap_true FROM zpru_agent_tool
            WHERE agent_uuid = @<ls_key_child>-agent_uuid
            INTO @DATA(lv_exists_ch).
          IF lv_exists_ch = abap_true.
            SELECT * FROM zpru_agent_tool
              WHERE agent_uuid = @<ls_key_child>-agent_uuid
              INTO CORRESPONDING FIELDS OF TABLE @lt_child_tab.
            IF sy-subrc = 0.
              LOOP AT lt_child_tab ASSIGNING FIELD-SYMBOL(<ls_child>).
                IF NOT line_exists( zpru_cl_adf_buffer=>tool_buffer[ instance-agent_uuid = <ls_child>-agent_uuid
                                                                     instance-tool_uuid  = <ls_child>-tool_uuid ] ).
                  APPEND VALUE #( instance = <ls_child> ) TO zpru_cl_adf_buffer=>tool_buffer.
                ENDIF.
              ENDLOOP.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
