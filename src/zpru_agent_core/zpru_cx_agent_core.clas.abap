CLASS zpru_cx_agent_core DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.

    INTERFACES if_t100_message.

    DATA:
      gv_data_id TYPE string,
      gv_reason  TYPE string.

    METHODS constructor
      IMPORTING
        !textid    LIKE if_t100_message=>t100key OPTIONAL
        !previous  LIKE previous                 OPTIONAL
        !i_data_id TYPE string                   OPTIONAL
        !i_reason  TYPE string                   OPTIONAL.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zpru_cx_agent_core IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.

    me->gv_data_id = i_data_id.
    me->gv_reason  = i_reason.

    IF textid IS INITIAL.
      if_t100_message~t100key =
        VALUE #( msgid = 'ZMSG'
                 msgno = '001'
                 attr1 = 'GV_DATA_ID'
                 attr2 = 'GV_REASON' ).
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
