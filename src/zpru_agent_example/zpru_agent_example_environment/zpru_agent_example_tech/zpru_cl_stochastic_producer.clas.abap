CLASS zpru_cl_stochastic_producer DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
INTERFACES zpru_if_agent_frw.
    CLASS-METHODS get_decision
      RETURNING VALUE(rv_result) TYPE i.

    CLASS-METHODS get_stochastic_value
      IMPORTING iv_max           TYPE i DEFAULT 10
      RETURNING VALUE(rv_result) TYPE i.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zpru_cl_stochastic_producer IMPLEMENTATION.
  METHOD get_decision.
    DATA(lo_rand) = cl_abap_random_int=>create( seed = cl_abap_random=>seed( )
                                                min  = 1
                                                max  = 10 ).

    DATA(lv_digit) = lo_rand->get_next( ).

    IF lv_digit MOD 2 = 0.
      rv_result = 2.
    ELSE.
      rv_result = 1.
    ENDIF.
  ENDMETHOD.

  METHOD get_stochastic_value.
    DATA(lo_rand) = cl_abap_random_int=>create( seed = cl_abap_random=>seed( )
                                                min  = 1
                                                max  = iv_max ).
    rv_result = lo_rand->get_next( ).
  ENDMETHOD.

ENDCLASS.
