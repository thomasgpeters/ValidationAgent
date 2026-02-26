package com.workflow.client.naf.validate;   

import java.lang.reflect.Field;

public class PersonImpl extends PassListBase {

    
    //    TYPES
    // RECORD_TYPE
    // CHARACTER_TYPE
    // ZONED_NUMERIC_TYPE
    
    // Field sizes
    public static final int PERSON_SIZE                     =   91;       // offset     0
    public static final int ID_SIZE                         =   12;         // offset     0
    public static final int FIRST_NAME_SIZE                 =   32;         // offset     0
    public static final int LAST_NAME_SIZE                  =   32;         // offset     0
    public static final int MIDDLE_INITIAL_SIZE             =   1;          // offset     0
    public static final int AGE_SIZE                        =   3;          // offset     0
    public static final int SEX_SIZE                        =   1;          // offset     0
    public static final int DATE_OF_BIRTH_SIZE              =   10;         // offset     0

    // Array offsets
    public static final int SEARCH_PERSIN_OFFSET            =  0;
    public static final int ID_OFFSET                       =  0;
    public static final int FIRST_NAME_OFFSET               =  12;
    public static final int LAST_NAME_OFFSET                =  44;
    public static final int MIDDLE_INITIAL_OFFSET           =  76;
    public static final int AGE_OFFSET                      =  77;
    public static final int SEX_OFFSET                      =  80;
    public static final int DATE_OF_BIRTH_OFFSET            =  81;

    // Data Types
    public static final String SEARCH_PERSON_TYPE           =  CHARACTER_TYPE;
    public static final String ID_TYPE                      =  CHARACTER_TYPE;
    public static final String FIRST_NAME_TYPE              =  CHARACTER_TYPE;
    public static final String LAST_NAME_TYPE               =  CHARACTER_TYPE;
    public static final String MIDDLE_INITIAL_TYPE          =  CHARACTER_TYPE;
    public static final String AGE_TYPE                     =  CHARACTER_TYPE;
    public static final String SEX_TYPE                     =  CHARACTER_TYPE;
    public static final String DATE_OF_BIRTH_TYPE           =  CHARACTER_TYPE;

    public PersonImpl() {
        this(new char[PERSON_SIZE]);
    }
    
    public PersonImpl(char[] data_elements) {
        
        super();
        if (data_elements.length != PERSON_SIZE)
            data_elements = new char[PERSON_SIZE];
            
        for (int i = 0; i < PERSON_SIZE; i++) {
            data_elements[i] = ' ';
        }
        
        DATA_ELEMENT_SIZE = PERSON_SIZE;
        this.data_elements = data_elements;
    }
}
