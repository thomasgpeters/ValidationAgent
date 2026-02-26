package com.tgp.object.data;   

import java.lang.reflect.Field;
import com.tgp.object.DataObjectBase;

public class PhoneImpl extends DataObjectBase {

    
    //    TYPES
    // RECORD_TYPE
    // CHARACTER_TYPE
    // ZONED_NUMERIC_TYPE
    
    // Field sizes
    public static final int OBJECT_SIZE                     =   12;       // offset     0
    public static final int NUMBER_SIZE                     =   12;         // offset     0

    // Array offsets
    public static final int OBJECT_OFFSET                   =  0;
    public static final int NUMBER_OFFSET                   =  0;

    // Data Types
    public static final String OBJECT_TYPE                  =  CHARACTER_TYPE;
    public static final String NUMBER_TYPE                  =  CHARACTER_TYPE;

    public PhoneImpl() {
        this(new char[OBJECT_SIZE]);
    }
    
    public PhoneImpl(char[] data_elements) {
        
        super();
        if (data_elements.length != OBJECT_SIZE)
            data_elements = new char[OBJECT_SIZE];
            
        for (int i = 0; i < OBJECT_SIZE; i++) {
            data_elements[i] = ' ';
        }
        
        DATA_ELEMENT_SIZE = OBJECT_SIZE;
        this.data_elements = data_elements;
    }
}
