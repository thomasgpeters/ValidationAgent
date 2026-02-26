package com.workflow.client.naf.validate;  

import java.lang.reflect.Field;

public class TransModeImpl extends PassListBase {

    // Field sizes
    public static final int MOT_SIZE                        = 200;  // offset - 897
    public static final int MOT_ALPHA_SIZE                              = 2;    // offset - 897
    public static final int MOT_DESC_SIZE                               = 35;   // offset - 899
    public static final int TRANSPORTATION_TYPE_SIZE                    = 1;    // offset - 934
    public static final int CUSTOM_TRANSPORTATION_TYPE_SIZE         = 1;    // offset - 935
    public static final int TIME_UPDATED_02_SIZE                        = 4;    // offset - 936
    public static final int DATE_UPDATED_02_SIZE                        = 8;    // offset - 940
    public static final int MODIFIED_BY_02_SIZE                     = 12;   // offset - 948
    public static final int SCAC_REQUIRED_SIZE                          = 1;    // offset - 960
    public static final int MASTER_REQUIRED_SIZE                        = 1;    // offset - 961
    public static final int MASTER_CHECK_DIGIT_SIZE                 = 1;    // offset - 962
    public static final int MOT_PORT_LADING_REQUIRED_SIZE               = 1;    // offset - 963
    public static final int CARRIER_REQUIRED_SIZE                       = 1;    // offset - 964
    public static final int VESSEL_NAME_REQUIRED_SIZE                   = 1;    // offset - 965
    public static final int VALIDATE_FLIGHT_SIZE                        = 1;    // offset - 966
    public static final int HOUSE_BILL_EIGHT_SIZE                       = 1;    // offset - 967
    public static final int VOYAGE_FLIGHT_NO_REQUIRED_SIZE              = 1;    // offset - 968
    public static final int CONTAINERS_SCREEN_SIZE                      = 1;    // offset - 969
    public static final int VALID_IN_BOND_SIZE                          = 1;    // offset - 970
    public static final int ARRIVAL_SYSTEM_DATE_SIZE                    = 1;    // offset - 971
    public static final int SELECTIVITY_RESULTS_SIZE                    = 1;    // offset - 972
    public static final int CONTAINERIZED_SIZE                          = 1;    // offset - 973
    public static final int ALLOW_AES_SIZE                              = 1;    // offset - 974
    public static final int ARRIVAL_EXPORT_DATE_SIZE                    = 1;    // offset - 975
    public static final int FILLER_02_SIZE                              = 121;  // offset - 976

    // Array offsets
    public static final int MOT_OFFSET                  = 897;
    public static final int MOT_ALPHA_OFFSET                            = 897;
    public static final int MOT_DESC_OFFSET                         = 899;
    public static final int TRANSPORTATION_TYPE_OFFSET                  = 934;
    public static final int CUSTOM_TRANSPORTATION_TYPE_OFFSET           = 935;
    public static final int TIME_UPDATED_02_OFFSET                      = 936;
    public static final int DATE_UPDATED_02_OFFSET                      = 940;
    public static final int MODIFIED_BY_02_OFFSET                       = 948;
    public static final int SCAC_REQUIRED_OFFSET                        = 960;
    public static final int MASTER_REQUIRED_OFFSET                      = 961;
    public static final int MASTER_CHECK_DIGIT_OFFSET                   = 962;
    public static final int MOT_PORT_LADING_REQUIRED_OFFSET         = 963;
    public static final int CARRIER_REQUIRED_OFFSET                 = 964;
    public static final int VESSEL_NAME_REQUIRED_OFFSET             = 965;
    public static final int VALIDATE_FLIGHT_OFFSET                      = 966;
    public static final int HOUSE_BILL_EIGHT_OFFSET                 = 967;
    public static final int VOYAGE_FLIGHT_NO_REQUIRED_OFFSET            = 968;
    public static final int CONTAINERS_SCREEN_OFFSET                    = 969;
    public static final int VALID_IN_BOND_OFFSET                        = 970;
    public static final int ARRIVAL_SYSTEM_DATE_OFFSET                  = 971;
    public static final int SELECTIVITY_RESULTS_OFFSET                  = 972;
    public static final int CONTAINERIZED_OFFSET                        = 973;
    public static final int ALLOW_AES_OFFSET                            = 974;
    public static final int ARRIVAL_EXPORT_DATE_OFFSET                  = 975;
    public static final int FILLER_02_OFFSET                            = 976;

    // Data types
    public static final String MOT_TYPE                                 = CHARACTER_TYPE;
    public static final String MOT_ALPHA_TYPE                           = CHARACTER_TYPE;
    public static final String MOT_DESC_TYPE                            = CHARACTER_TYPE;
    public static final String TRANSPORTATION_TYPE_TYPE                 = CHARACTER_TYPE;
    public static final String CUSTOM_TRANSPORTATION_TYPE_TYPE          = CHARACTER_TYPE;
    public static final String TIME_UPDATED_02_TYPE                     = ZONED_NUMERIC_TYPE;
    public static final String DATE_UPDATED_02_TYPE                     = ZONED_NUMERIC_TYPE;
    public static final String MODIFIED_BY_02_TYPE                      = CHARACTER_TYPE;
    public static final String SCAC_REQUIRED_TYPE                       = CHARACTER_TYPE;
    public static final String MASTER_REQUIRED_TYPE                     = CHARACTER_TYPE;
    public static final String MASTER_CHECK_DIGIT_TYPE                  = CHARACTER_TYPE;
    public static final String MOT_PORT_LADING_REQUIRED_TYPE            = CHARACTER_TYPE;
    public static final String CARRIER_REQUIRED_TYPE                    = CHARACTER_TYPE;
    public static final String VESSEL_NAME_REQUIRED_TYPE                = CHARACTER_TYPE;
    public static final String VALIDATE_FLIGHT_TYPE                     = CHARACTER_TYPE;
    public static final String HOUSE_BILL_EIGHT_TYPE                    = CHARACTER_TYPE;
    public static final String VOYAGE_FLIGHT_NO_REQUIRED_TYPE           = CHARACTER_TYPE;
    public static final String CONTAINERS_SCREEN_TYPE                   = CHARACTER_TYPE;
    public static final String VALID_IN_BOND_TYPE                       = CHARACTER_TYPE;
    public static final String ARRIVAL_SYSTEM_DATE_TYPE                 = CHARACTER_TYPE;
    public static final String SELECTIVITY_RESULTS_TYPE                 = CHARACTER_TYPE;
    public static final String CONTAINERIZED_TYPE                       = CHARACTER_TYPE;
    public static final String ALLOW_AES_TYPE                           = CHARACTER_TYPE;
    public static final String ARRIVAL_EXPORT_DATE_TYPE                 = CHARACTER_TYPE;
    public static final String FILLER_02_TYPE                           = CHARACTER_TYPE;
    
    public TransModeImpl() {
        this(new char[MOT_SIZE]);
    }
    
    public TransModeImpl(char[] data_elements) {
        
        super();
        if (data_elements.length != MOT_SIZE)
            data_elements = new char[MOT_SIZE];
            
        for (int i = 0; i < MOT_SIZE; i++) {
            data_elements[i] = ' ';
        }
        
        DATA_ELEMENT_SIZE = MOT_SIZE;
        this.data_elements = data_elements;
    }
}
