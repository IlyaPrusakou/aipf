"! <p class="shorttext synchronized">Consumption model for client proxy - generated</p>
"! This class has been generated based on the metadata with namespace
"! <em>com.sap.gateway.srvd_a2x.api_warehouse_storage_bin_2.v0001</em>
CLASS zpru_storage_bin DEFINITION
  PUBLIC
  INHERITING FROM /iwbep/cl_v4_abs_pm_model_prov
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES:
      "! <p class="shorttext synchronized">Value Control Structure of WAREHOUSE_STORAGE_BIN_TYPE</p>
      BEGIN OF tys_value_controls_1,
        "! PINV_COUNTED_UTCDATE_TIME
        pinv_counted_utcdate_time  TYPE /iwbep/v4_value_control,
        "! EWMSTORAGE_BIN_LAST_CHG_DA
        ewmstorage_bin_last_chg_da TYPE /iwbep/v4_value_control,
        "! EWMSTORAGE_BIN_FIRST_PUTAW
        ewmstorage_bin_first_putaw TYPE /iwbep/v4_value_control,
        "! EWMSTORAGE_BIN_LAST_MVT_DA
        ewmstorage_bin_last_mvt_da TYPE /iwbep/v4_value_control,
        "! EWMSTORAGE_BIN_LAST_CLRG_D
        ewmstorage_bin_last_clrg_d TYPE /iwbep/v4_value_control,
      END OF tys_value_controls_1.

    TYPES:
      "! <p class="shorttext synchronized">WarehouseStorageBinType</p>
      BEGIN OF tys_warehouse_storage_bin_type,
        "! <em>Value Control Structure</em>
        value_controls             TYPE tys_value_controls_1,
        "! <em>Key property</em> EWMWarehouse
        ewmwarehouse               TYPE c LENGTH 4,
        "! <em>Key property</em> EWMStorageBin
        ewmstorage_bin             TYPE c LENGTH 18,
        "! EWMStorageType
        ewmstorage_type            TYPE c LENGTH 4,
        "! EWMStorageBinIsEmpty
        ewmstorage_bin_is_empty    TYPE abap_bool,
        "! EWMStorageBinIsFull
        ewmstorage_bin_is_full     TYPE abap_bool,
        "! EWMStorBinVerifiedByMobileID
        ewmstor_bin_verified_by_mo TYPE c LENGTH 18,
        "! EWMStorBinIsBlockedForRemoval
        ewmstor_bin_is_blocked_for TYPE abap_bool,
        "! EWMStorBinIsBlockedForPutaway
        ewmstor_bin_is_blocked_f_2 TYPE abap_bool,
        "! EWMStorBinIsBlockedDueToPInv
        ewmstor_bin_is_blocked_due TYPE abap_bool,
        "! EWMStorBinFreeDefinedAisleText
        ewmstor_bin_free_defined_a TYPE c LENGTH 18,
        "! EWMStorBinFreeDefinedStackText
        ewmstor_bin_free_defined_s TYPE c LENGTH 18,
        "! EWMStorBinFreeDefinedLevelText
        ewmstor_bin_free_defined_l TYPE c LENGTH 18,
        "! WeightUnit
        weight_unit                TYPE c LENGTH 3,
        "! EWMStorageBinMaximumWeight
        ewmstorage_bin_maximum_wei TYPE p LENGTH 8 DECIMALS 3,
        "! EWMStorageBinUsedWeight
        ewmstorage_bin_used_weight TYPE p LENGTH 8 DECIMALS 3,
        "! VolumeUnit
        volume_unit                TYPE c LENGTH 3,
        "! EWMStorageBinMaximumVolume
        ewmstorage_bin_maximum_vol TYPE p LENGTH 8 DECIMALS 3,
        "! EWMStorageBinOccupiedVolume
        ewmstorage_bin_occupied_vo TYPE p LENGTH 8 DECIMALS 3,
        "! EWMStorBinTotalCapacityValue
        ewmstor_bin_total_capacity TYPE p LENGTH 8 DECIMALS 3,
        "! EWMStorBinAvailCapacityValue
        ewmstor_bin_avail_capacity TYPE p LENGTH 8 DECIMALS 3,
        "! EWMStorBinWidthCoordinateValue
        ewmstor_bin_width_coordina TYPE p LENGTH 6 DECIMALS 3,
        "! EWMStorBinLengthCoordinateVal
        ewmstor_bin_length_coordin TYPE p LENGTH 6 DECIMALS 3,
        "! EWMStorBinHeightCoordinateVal
        ewmstor_bin_height_coordin TYPE p LENGTH 6 DECIMALS 3,
        "! EWMPhysicalInventoryType
        ewmphysical_inventory_type TYPE c LENGTH 2,
        "! PhysicalInventoryDocNumber
        physical_inventory_doc_num TYPE c LENGTH 20,
        "! PhysicalInventoryItemNumber
        physical_inventory_item_nu TYPE c LENGTH 6,
        "! PInvCountedUTCDateTime
        pinv_counted_utcdate_time  TYPE timestampl,
        "! EWMStorageBinLastChangedByUser
        ewmstorage_bin_last_change TYPE c LENGTH 12,
        "! EWMStorageBinLastChgDateTime
        ewmstorage_bin_last_chg_da TYPE timestamp,
        "! EWMStorageBinLastWarehouseTask
        ewmstorage_bin_last_wareho TYPE c LENGTH 12,
        "! EWMStorageBinFirstPutawayDate
        ewmstorage_bin_first_putaw TYPE datn,
        "! EWMStorageBinLastMvtDateTime
        ewmstorage_bin_last_mvt_da TYPE timestamp,
        "! EWMStorageBinLastClrgDateTime
        ewmstorage_bin_last_clrg_d TYPE timestamp,
        "! EWMStorageBinFixedBinType
        ewmstorage_bin_fixed_bin_t TYPE c LENGTH 1,
        "! EWMStorageBinAngleValue
        ewmstorage_bin_angle_value TYPE p LENGTH 3 DECIMALS 1,
        "! EWMStorBinNumberOfHndlgUnits
        ewmstor_bin_number_of_hndl TYPE p LENGTH 4 DECIMALS 0,
        "! EWMStorBinMaxNmbrOfHndlgUnits
        ewmstor_bin_max_nmbr_of_hn TYPE c LENGTH 6,
        "! EWMStorageSection
        ewmstorage_section         TYPE c LENGTH 4,
        "! EWMStorageBinType
        ewmstorage_bin_type        TYPE c LENGTH 4,
        "! EWMStorageBinAccessType
        ewmstorage_bin_access_type TYPE c LENGTH 4,
        "! EWMStorageBinSection
        ewmstorage_bin_section     TYPE c LENGTH 1,
        "! EWMStorageBinPosition
        ewmstorage_bin_position    TYPE c LENGTH 2,
        "! EWMStorBinFreeDfndSectionText
        ewmstor_bin_free_dfnd_sect TYPE c LENGTH 18,
        "! EWMStorageBinSubdivision
        ewmstorage_bin_subdivision TYPE c LENGTH 1,
        "! EWMStorageGroup
        ewmstorage_group           TYPE c LENGTH 4,
        "! odata.etag
        etag                       TYPE string,
      END OF tys_warehouse_storage_bin_type,
      "! <p class="shorttext synchronized">List of WarehouseStorageBinType</p>
      tyt_warehouse_storage_bin_type TYPE STANDARD TABLE OF tys_warehouse_storage_bin_type WITH DEFAULT KEY.


    CONSTANTS:
      "! <p class="shorttext synchronized">Internal Names of the entity sets</p>
      BEGIN OF gcs_entity_set,
        "! WarehouseStorageBin
        "! <br/> Collection of type 'WarehouseStorageBinType'
        warehouse_storage_bin TYPE /iwbep/if_cp_runtime_types=>ty_entity_set_name VALUE 'WAREHOUSE_STORAGE_BIN',
      END OF gcs_entity_set .

    CONSTANTS:
      "! <p class="shorttext synchronized">Internal names for complex types</p>
      BEGIN OF gcs_complex_type,
         "! Dummy field - Structure must not be empty
         dummy TYPE int1 VALUE 0,
      END OF gcs_complex_type.

    CONSTANTS:
      "! <p class="shorttext synchronized">Internal names for entity types</p>
      BEGIN OF gcs_entity_type,
        "! <p class="shorttext synchronized">Internal names for WarehouseStorageBinType</p>
        "! See also structure type {@link ..tys_warehouse_storage_bin_type}
        BEGIN OF warehouse_storage_bin_type,
          "! <p class="shorttext synchronized">Navigation properties</p>
          BEGIN OF navigation,
            "! Dummy field - Structure must not be empty
            dummy TYPE int1 VALUE 0,
          END OF navigation,
        END OF warehouse_storage_bin_type,
      END OF gcs_entity_type.


    METHODS /iwbep/if_v4_mp_basic_pm~define REDEFINITION.


  PRIVATE SECTION.

    "! <p class="shorttext synchronized">Model</p>
    DATA mo_model TYPE REF TO /iwbep/if_v4_pm_model.


    "! <p class="shorttext synchronized">Define WarehouseStorageBinType</p>
    "! @raising /iwbep/cx_gateway | <p class="shorttext synchronized">Gateway Exception</p>
    METHODS def_warehouse_storage_bin_type RAISING /iwbep/cx_gateway.

ENDCLASS.



CLASS ZPRU_STORAGE_BIN IMPLEMENTATION.


  METHOD /iwbep/if_v4_mp_basic_pm~define.

    mo_model = io_model.
    mo_model->set_schema_namespace( 'com.sap.gateway.srvd_a2x.api_warehouse_storage_bin_2.v0001' ) ##NO_TEXT.

    def_warehouse_storage_bin_type( ).

  ENDMETHOD.


  METHOD def_warehouse_storage_bin_type.

    DATA:
      lo_complex_property    TYPE REF TO /iwbep/if_v4_pm_cplx_prop,
      lo_entity_type         TYPE REF TO /iwbep/if_v4_pm_entity_type,
      lo_entity_set          TYPE REF TO /iwbep/if_v4_pm_entity_set,
      lo_navigation_property TYPE REF TO /iwbep/if_v4_pm_nav_prop,
      lo_primitive_property  TYPE REF TO /iwbep/if_v4_pm_prim_prop.


    lo_entity_type = mo_model->create_entity_type_by_struct(
                                    iv_entity_type_name       = 'WAREHOUSE_STORAGE_BIN_TYPE'
                                    is_structure              = VALUE tys_warehouse_storage_bin_type( )
                                    iv_do_gen_prim_props         = abap_true
                                    iv_do_gen_prim_prop_colls    = abap_true
                                    iv_do_add_conv_to_prim_props = abap_true ).

    lo_entity_type->set_edm_name( 'WarehouseStorageBinType' ) ##NO_TEXT.
    lo_entity_type->create_complex_prop_for_vcs( 'VALUE_CONTROLS' ).


    lo_entity_set = lo_entity_type->create_entity_set( 'WAREHOUSE_STORAGE_BIN' ).
    lo_entity_set->set_edm_name( 'WarehouseStorageBin' ) ##NO_TEXT.


    lo_primitive_property = lo_entity_type->get_primitive_property( 'EWMWAREHOUSE' ).
    lo_primitive_property->set_edm_name( 'EWMWarehouse' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 4 ) ##NUMBER_OK.
    lo_primitive_property->set_is_key( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'EWMSTORAGE_BIN' ).
    lo_primitive_property->set_edm_name( 'EWMStorageBin' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 18 ) ##NUMBER_OK.
    lo_primitive_property->set_is_key( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'EWMSTORAGE_TYPE' ).
    lo_primitive_property->set_edm_name( 'EWMStorageType' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 4 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'EWMSTORAGE_BIN_IS_EMPTY' ).
    lo_primitive_property->set_edm_name( 'EWMStorageBinIsEmpty' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Boolean' ) ##NO_TEXT.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'EWMSTORAGE_BIN_IS_FULL' ).
    lo_primitive_property->set_edm_name( 'EWMStorageBinIsFull' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Boolean' ) ##NO_TEXT.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'EWMSTOR_BIN_VERIFIED_BY_MO' ).
    lo_primitive_property->set_edm_name( 'EWMStorBinVerifiedByMobileID' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 18 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'EWMSTOR_BIN_IS_BLOCKED_FOR' ).
    lo_primitive_property->set_edm_name( 'EWMStorBinIsBlockedForRemoval' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Boolean' ) ##NO_TEXT.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'EWMSTOR_BIN_IS_BLOCKED_F_2' ).
    lo_primitive_property->set_edm_name( 'EWMStorBinIsBlockedForPutaway' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Boolean' ) ##NO_TEXT.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'EWMSTOR_BIN_IS_BLOCKED_DUE' ).
    lo_primitive_property->set_edm_name( 'EWMStorBinIsBlockedDueToPInv' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Boolean' ) ##NO_TEXT.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'EWMSTOR_BIN_FREE_DEFINED_A' ).
    lo_primitive_property->set_edm_name( 'EWMStorBinFreeDefinedAisleText' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 18 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'EWMSTOR_BIN_FREE_DEFINED_S' ).
    lo_primitive_property->set_edm_name( 'EWMStorBinFreeDefinedStackText' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 18 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'EWMSTOR_BIN_FREE_DEFINED_L' ).
    lo_primitive_property->set_edm_name( 'EWMStorBinFreeDefinedLevelText' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 18 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'WEIGHT_UNIT' ).
    lo_primitive_property->set_edm_name( 'WeightUnit' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 3 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'EWMSTORAGE_BIN_MAXIMUM_WEI' ).
    lo_primitive_property->set_edm_name( 'EWMStorageBinMaximumWeight' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Decimal' ) ##NO_TEXT.
    lo_primitive_property->set_precision( 15 ) ##NUMBER_OK.
    lo_primitive_property->set_scale( 3 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'EWMSTORAGE_BIN_USED_WEIGHT' ).
    lo_primitive_property->set_edm_name( 'EWMStorageBinUsedWeight' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Decimal' ) ##NO_TEXT.
    lo_primitive_property->set_precision( 15 ) ##NUMBER_OK.
    lo_primitive_property->set_scale( 3 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'VOLUME_UNIT' ).
    lo_primitive_property->set_edm_name( 'VolumeUnit' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 3 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'EWMSTORAGE_BIN_MAXIMUM_VOL' ).
    lo_primitive_property->set_edm_name( 'EWMStorageBinMaximumVolume' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Decimal' ) ##NO_TEXT.
    lo_primitive_property->set_precision( 15 ) ##NUMBER_OK.
    lo_primitive_property->set_scale( 3 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'EWMSTORAGE_BIN_OCCUPIED_VO' ).
    lo_primitive_property->set_edm_name( 'EWMStorageBinOccupiedVolume' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Decimal' ) ##NO_TEXT.
    lo_primitive_property->set_precision( 15 ) ##NUMBER_OK.
    lo_primitive_property->set_scale( 3 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'EWMSTOR_BIN_TOTAL_CAPACITY' ).
    lo_primitive_property->set_edm_name( 'EWMStorBinTotalCapacityValue' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Decimal' ) ##NO_TEXT.
    lo_primitive_property->set_precision( 15 ) ##NUMBER_OK.
    lo_primitive_property->set_scale( 3 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'EWMSTOR_BIN_AVAIL_CAPACITY' ).
    lo_primitive_property->set_edm_name( 'EWMStorBinAvailCapacityValue' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Decimal' ) ##NO_TEXT.
    lo_primitive_property->set_precision( 15 ) ##NUMBER_OK.
    lo_primitive_property->set_scale( 3 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'EWMSTOR_BIN_WIDTH_COORDINA' ).
    lo_primitive_property->set_edm_name( 'EWMStorBinWidthCoordinateValue' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Decimal' ) ##NO_TEXT.
    lo_primitive_property->set_precision( 10 ) ##NUMBER_OK.
    lo_primitive_property->set_scale( 3 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'EWMSTOR_BIN_LENGTH_COORDIN' ).
    lo_primitive_property->set_edm_name( 'EWMStorBinLengthCoordinateVal' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Decimal' ) ##NO_TEXT.
    lo_primitive_property->set_precision( 10 ) ##NUMBER_OK.
    lo_primitive_property->set_scale( 3 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'EWMSTOR_BIN_HEIGHT_COORDIN' ).
    lo_primitive_property->set_edm_name( 'EWMStorBinHeightCoordinateVal' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Decimal' ) ##NO_TEXT.
    lo_primitive_property->set_precision( 10 ) ##NUMBER_OK.
    lo_primitive_property->set_scale( 3 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'EWMPHYSICAL_INVENTORY_TYPE' ).
    lo_primitive_property->set_edm_name( 'EWMPhysicalInventoryType' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 2 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'PHYSICAL_INVENTORY_DOC_NUM' ).
    lo_primitive_property->set_edm_name( 'PhysicalInventoryDocNumber' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 20 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'PHYSICAL_INVENTORY_ITEM_NU' ).
    lo_primitive_property->set_edm_name( 'PhysicalInventoryItemNumber' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 6 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'PINV_COUNTED_UTCDATE_TIME' ).
    lo_primitive_property->set_edm_name( 'PInvCountedUTCDateTime' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'DateTimeOffset' ) ##NO_TEXT.
    lo_primitive_property->set_precision( 7 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).
    lo_primitive_property->create_vcs_value_control( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'EWMSTORAGE_BIN_LAST_CHANGE' ).
    lo_primitive_property->set_edm_name( 'EWMStorageBinLastChangedByUser' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 12 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'EWMSTORAGE_BIN_LAST_CHG_DA' ).
    lo_primitive_property->set_edm_name( 'EWMStorageBinLastChgDateTime' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'DateTimeOffset' ) ##NO_TEXT.
    lo_primitive_property->set_is_nullable( ).
    lo_primitive_property->create_vcs_value_control( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'EWMSTORAGE_BIN_LAST_WAREHO' ).
    lo_primitive_property->set_edm_name( 'EWMStorageBinLastWarehouseTask' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 12 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'EWMSTORAGE_BIN_FIRST_PUTAW' ).
    lo_primitive_property->set_edm_name( 'EWMStorageBinFirstPutawayDate' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Date' ) ##NO_TEXT.
    lo_primitive_property->set_is_nullable( ).
    lo_primitive_property->create_vcs_value_control( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'EWMSTORAGE_BIN_LAST_MVT_DA' ).
    lo_primitive_property->set_edm_name( 'EWMStorageBinLastMvtDateTime' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'DateTimeOffset' ) ##NO_TEXT.
    lo_primitive_property->set_is_nullable( ).
    lo_primitive_property->create_vcs_value_control( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'EWMSTORAGE_BIN_LAST_CLRG_D' ).
    lo_primitive_property->set_edm_name( 'EWMStorageBinLastClrgDateTime' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'DateTimeOffset' ) ##NO_TEXT.
    lo_primitive_property->set_is_nullable( ).
    lo_primitive_property->create_vcs_value_control( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'EWMSTORAGE_BIN_FIXED_BIN_T' ).
    lo_primitive_property->set_edm_name( 'EWMStorageBinFixedBinType' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'EWMSTORAGE_BIN_ANGLE_VALUE' ).
    lo_primitive_property->set_edm_name( 'EWMStorageBinAngleValue' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Decimal' ) ##NO_TEXT.
    lo_primitive_property->set_precision( 4 ) ##NUMBER_OK.
    lo_primitive_property->set_scale( 1 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'EWMSTOR_BIN_NUMBER_OF_HNDL' ).
    lo_primitive_property->set_edm_name( 'EWMStorBinNumberOfHndlgUnits' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Decimal' ) ##NO_TEXT.
    lo_primitive_property->set_precision( 6 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'EWMSTOR_BIN_MAX_NMBR_OF_HN' ).
    lo_primitive_property->set_edm_name( 'EWMStorBinMaxNmbrOfHndlgUnits' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 6 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'EWMSTORAGE_SECTION' ).
    lo_primitive_property->set_edm_name( 'EWMStorageSection' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 4 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'EWMSTORAGE_BIN_TYPE' ).
    lo_primitive_property->set_edm_name( 'EWMStorageBinType' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 4 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'EWMSTORAGE_BIN_ACCESS_TYPE' ).
    lo_primitive_property->set_edm_name( 'EWMStorageBinAccessType' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 4 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'EWMSTORAGE_BIN_SECTION' ).
    lo_primitive_property->set_edm_name( 'EWMStorageBinSection' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'EWMSTORAGE_BIN_POSITION' ).
    lo_primitive_property->set_edm_name( 'EWMStorageBinPosition' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 2 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'EWMSTOR_BIN_FREE_DFND_SECT' ).
    lo_primitive_property->set_edm_name( 'EWMStorBinFreeDfndSectionText' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 18 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'EWMSTORAGE_BIN_SUBDIVISION' ).
    lo_primitive_property->set_edm_name( 'EWMStorageBinSubdivision' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'EWMSTORAGE_GROUP' ).
    lo_primitive_property->set_edm_name( 'EWMStorageGroup' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 4 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'ETAG' ).
    lo_primitive_property->set_edm_name( 'ETAG' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->use_as_etag( ).
    lo_primitive_property->set_is_technical( ).

  ENDMETHOD.
ENDCLASS.
