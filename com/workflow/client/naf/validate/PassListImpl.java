package com.workflow.client.naf.validate; 

public class PassListImpl extends PassListBase {
    
    protected GenericValidationBufferImpl   generic_validation_buffer;
    protected BrokerControlImpl             broker_control;
    protected ShipmentHeaderImpl            shipment_header;
    
    // Field sizes
    public static final int PASS_LIST_SIZE                   = 4612; // offset - 0
    public static final int BROKER_CONTROL_SIZE             = 1000; // offset - 0
    public static final int T_ELEMENT_SIZE                  = 32;   // offset - 1000
    public static final int T_FILE_SIZE                     = 32;   // offset - 1032
    public static final int T_MODE_SIZE                     = 1;    // offset - 1064
    public static final int T_FILLER_01_SIZE                = 3;    // offset - 1065
    public static final int T_VALIDATE_NO_SIZE              = 5;    // offset - 1068
    public static final int T_FILLER_02_SIZE                = 3;    // offset - 1073
    public static final int T_VALID_STATUS_SIZE             = 1;    // offset - 1076
    public static final int T_FILLER_03_SIZE                = 3;    // offset - 1077
    public static final int T_ERROR_NO_SIZE                 = 5;    // offset - 1080
    public static final int T_FILLER_04_SIZE                = 3;    // offset - 1085
    public static final int T_ERROR_STATUS_SIZE             = 1;    // offset - 1088
    public static final int T_FILLER_05_SIZE                = 3;    // offset - 1089
    public static final int T_VALID_DATA_SIZE               = 150;  // offset - 1092
    public static final int T_FILLER_06_SIZE                = 3;    // offset - 1242
    public static final int T_DATE_STRING_SIZE              = 78;   // offset - 1245
    public static final int T_FILLER_07_SIZE                = 2;    // offset - 1323
    public static final int T_PROG_ID_SIZE                  = 8;    // offset - 1325
    public static final int T_ERROR_COUNTER_SIZE            = 3;    // offset - 1333
    public static final int T_FILLER_08_SIZE                = 1;    // offset - 1336
    public static final int GENERIC_VALIDATION_BUFFER_SIZE  = 1500; // offset - 1337
    public static final int SHIPMENT_HEADER_SIZE            = 1750; // offset - 2837
    public static final int T_FILLER_09_SIZE                = 2;    // offset - 4587
    public static final int T_FILE_NO_SIZE                  = 10;   // offset - 4589
    public static final int LOGONID_SIZE                    = 12;   // offset - 4599

    // Array offsets
    public static final int PASS_LIST_OFFSET                    = 0;
    public static final int BROKER_CONTROL_OFFSET               = 0;
    public static final int T_ELEMENT_OFFSET                    = 1000;
    public static final int T_FILE_OFFSET                       = 1032;
    public static final int T_MODE_OFFSET                       = 1064;
    public static final int T_FILLER_01_OFFSET                  = 1065;
    public static final int T_VALIDATE_NO_OFFSET                = 1068;
    public static final int T_FILLER_02_OFFSET                  = 1073;
    public static final int T_VALID_STATUS_OFFSET               = 1076;
    public static final int T_FILLER_03_OFFSET                  = 1077;
    public static final int T_ERROR_NO_OFFSET                   = 1080;
    public static final int T_FILLER_04_OFFSET                  = 1085;
    public static final int T_ERROR_STATUS_OFFSET               = 1088;
    public static final int T_FILLER_05_OFFSET                  = 1089;
    public static final int T_VALID_DATA_OFFSET                 = 1092;
    public static final int T_FILLER_06_OFFSET                  = 1242;
    public static final int T_DATE_STRING_OFFSET                = 1245;
    public static final int T_FILLER_07_OFFSET                  = 1323;
    public static final int T_PROG_ID_OFFSET                    = 1325;
    public static final int T_ERROR_COUNTER_OFFSET              = 1333;
    public static final int T_FILLER_08_OFFSET                  = 1336;
    public static final int GENERIC_VALIDATION_BUFFER_OFFSET    = 1337;
    public static final int SHIPMENT_HEADER_OFFSET              = 2837;
    public static final int T_FILLER_09_OFFSET                  = 4587;
    public static final int T_FILE_NO_OFFSET                    = 4589;
    public static final int LOGONID_OFFSET                      = 4599;

    // Data types
    public static final String PASS_LIST_TYPE                   = RECORD_TYPE;
    public static final String BROKER_CONTROL_TYPE              = RECORD_TYPE;
    public static final String T_ELEMENT_TYPE                   = CHARACTER_TYPE;
    public static final String T_FILE_TYPE                      = CHARACTER_TYPE;
    public static final String T_MODE_TYPE                      = CHARACTER_TYPE;
    public static final String T_FILLER_01_TYPE                 = CHARACTER_TYPE;
    public static final String T_VALIDATE_NO_TYPE               = CHARACTER_TYPE;
    public static final String T_FILLER_02_TYPE                 = CHARACTER_TYPE;
    public static final String T_VALID_STATUS_TYPE              = CHARACTER_TYPE;
    public static final String T_FILLER_03_TYPE                 = CHARACTER_TYPE;
    public static final String T_ERROR_NO_TYPE                  = CHARACTER_TYPE;
    public static final String T_FILLER_04_TYPE                 = CHARACTER_TYPE;
    public static final String T_ERROR_STATUS_TYPE              = CHARACTER_TYPE;
    public static final String T_FILLER_05_TYPE                 = CHARACTER_TYPE;
    public static final String T_VALID_DATA_TYPE                = CHARACTER_TYPE;
    public static final String T_FILLER_06_TYPE                 = CHARACTER_TYPE;
    public static final String T_DATE_STRING_TYPE               = CHARACTER_TYPE;
    public static final String T_FILLER_07_TYPE                 = CHARACTER_TYPE;
    public static final String T_PROG_ID_TYPE                   = CHARACTER_TYPE;
    public static final String T_ERROR_COUNTER_TYPE             = CHARACTER_TYPE;
    public static final String T_FILLER_08_TYPE                 = CHARACTER_TYPE;
    public static final String GENERIC_VALIDATION_BUFFER_TYPE   = RECORD_TYPE;
    public static final String SHIPMENT_HEADER_TYPE             = RECORD_TYPE;
    public static final String T_FILLER_09_TYPE                 = CHARACTER_TYPE;
    public static final String T_FILE_NO_TYPE                   = CHARACTER_TYPE;
    public static final String LOGONID_TYPE                     = CHARACTER_TYPE;

    public PassListImpl() {
        this(new char[PASS_LIST_SIZE]);
    }
    
    public PassListImpl(char[] data_elements) {
        
        super();
        if (data_elements.length != PASS_LIST_SIZE)
            data_elements = new char[PASS_LIST_SIZE];
            
        for (int i = 0; i < PASS_LIST_SIZE; i++) {
            data_elements[i] = ' ';
        }
        
        DATA_ELEMENT_SIZE = PASS_LIST_SIZE;
        super.data_elements = data_elements;
      
        // initialize child class data elements.
        char[] broker_control_data_elements = new char[BROKER_CONTROL_SIZE];
        for(int i = 0; i < BROKER_CONTROL_SIZE; i++) {
            broker_control_data_elements[i] = super.data_elements[i + BROKER_CONTROL_OFFSET];
        }
        
        broker_control = new BrokerControlImpl(broker_control_data_elements);

        char[] generic_validation_buffer_data_elements = new char[GENERIC_VALIDATION_BUFFER_SIZE];
        for(int i = 0; i < GENERIC_VALIDATION_BUFFER_SIZE; i++) {
            generic_validation_buffer_data_elements[i] = super.data_elements[i + GENERIC_VALIDATION_BUFFER_OFFSET];
        }
        
        generic_validation_buffer = new GenericValidationBufferImpl(generic_validation_buffer_data_elements);

        char[] shipment_header_data_elements = new char[SHIPMENT_HEADER_SIZE];
        for(int i = 0; i < SHIPMENT_HEADER_SIZE; i++) {
            shipment_header_data_elements[i] = super.data_elements[i + SHIPMENT_HEADER_OFFSET];
        }
                
        shipment_header = new ShipmentHeaderImpl(shipment_header_data_elements);
    }
    
    public GenericValidationBufferImpl getGenericValidationBuffer() {
        return generic_validation_buffer;
    }

    public BrokerControlImpl getBrokerControl() {
        return broker_control;
    }

    public ShipmentHeaderImpl getShipmentHeader() {
        return shipment_header;
    }
}
