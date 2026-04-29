CLASS zpru_cl_snro_intervals DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.

    CLASS-METHODS create_snro_intervals.
ENDCLASS.


CLASS zpru_cl_snro_intervals IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
    create_snro_intervals( ).
  ENDMETHOD.

  METHOD create_snro_intervals.
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
