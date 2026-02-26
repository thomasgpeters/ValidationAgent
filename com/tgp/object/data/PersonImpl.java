package com.tgp.object.data;   

import java.lang.reflect.Field;
import com.tgp.object.DataObjectBase;

public class PersonImpl extends DataObjectBase {

    
    protected PhoneImpl   home_phone_buffer;
    protected PhoneImpl   work_phone_buffer;

    //    TYPES
    // RECORD_TYPE
    // CHARACTER_TYPE
    // ZONED_NUMERIC_TYPE
    
    // Field sizes
    public static final int OBJECT_SIZE                     =   115;       // offset     0
    public static final int ID_SIZE                         =   12;         // offset     0
    public static final int FIRST_NAME_SIZE                 =   32;         // offset     0
    public static final int LAST_NAME_SIZE                  =   32;         // offset     0
    public static final int MIDDLE_INITIAL_SIZE             =   1;          // offset     0
    public static final int AGE_SIZE                        =   3;          // offset     0
    public static final int SEX_SIZE                        =   1;          // offset     0
    public static final int DATE_OF_BIRTH_SIZE              =   10;         // offset     0
    public static final int HOME_PHONE_SIZE                 =   12;         // offset     0
    public static final int WORK_PHONE_SIZE                 =   12;         // offset     0

    // Array offsets
    public static final int OBJECT_OFFSET                   =  0;
    public static final int ID_OFFSET                       =  0;
    public static final int FIRST_NAME_OFFSET               =  12;
    public static final int LAST_NAME_OFFSET                =  44;
    public static final int MIDDLE_INITIAL_OFFSET           =  76;
    public static final int AGE_OFFSET                      =  77;
    public static final int SEX_OFFSET                      =  80;
    public static final int DATE_OF_BIRTH_OFFSET            =  81;
    public static final int HOME_PHONE_OFFSET               =  91;
    public static final int WORK_PHONE_OFFSET               =  103;

    // Data Types
    public static final String OBJECT_TYPE                  =  CHARACTER_TYPE;
    public static final String ID_TYPE                      =  CHARACTER_TYPE;
    public static final String FIRST_NAME_TYPE              =  CHARACTER_TYPE;
    public static final String LAST_NAME_TYPE               =  CHARACTER_TYPE;
    public static final String MIDDLE_INITIAL_TYPE          =  CHARACTER_TYPE;
    public static final String AGE_TYPE                     =  CHARACTER_TYPE;
    public static final String SEX_TYPE                     =  CHARACTER_TYPE;
    public static final String DATE_OF_BIRTH_TYPE           =  CHARACTER_TYPE;
    public static final String HOME_PHONE_TYPE              =  RECORD_TYPE;
    public static final String WORK_PHONE_TYPE              =  RECORD_TYPE;

    public PersonImpl() {
        this(new char[OBJECT_SIZE]);
    }
    
    public PersonImpl(char[] data_elements) {
        
        super();
        if (data_elements.length != OBJECT_SIZE)
            data_elements = new char[OBJECT_SIZE];
            
        for (int i = 0; i < OBJECT_SIZE; i++) {
            data_elements[i] = ' ';
        }
        
        DATA_ELEMENT_SIZE = OBJECT_SIZE;
        this.data_elements = data_elements;

        // initialize child class data elements.
        char[] home_phone_data_elements = new char[HOME_PHONE_SIZE];
        for(int i = 0; i < HOME_PHONE_SIZE; i++) {
            home_phone_data_elements[i] = super.data_elements[i + HOME_PHONE_SIZE];
        }

        home_phone_buffer = new PhoneImpl(home_phone_data_elements);

        // initialize child class data elements.
        char[] work_phone_data_elements = new char[WORK_PHONE_SIZE];
        for(int i = 0; i < HOME_PHONE_SIZE; i++) {
            work_phone_data_elements[i] = super.data_elements[i + WORK_PHONE_SIZE];
        }
        
        work_phone_buffer = new PhoneImpl(work_phone_data_elements);

            // initialize child class data elements.
        char[] work_address_data_elements = new char[WORK_PHONE_SIZE];
        for(int i = 0; i < HOME_PHONE_SIZE; i++) {
            work_phone_data_elements[i] = super.data_elements[i + WORK_PHONE_SIZE];
        }
        
        work_phone_buffer = new PhoneImpl(work_phone_data_elements);

            // initialize child class data elements.
        char[] home_address_data_elements = new char[WORK_PHONE_SIZE];
        for(int i = 0; i < HOME_PHONE_SIZE; i++) {
            work_phone_data_elements[i] = super.data_elements[i + WORK_PHONE_SIZE];
        }
        
        work_phone_buffer = new PhoneImpl(work_phone_data_elements);
}
}
