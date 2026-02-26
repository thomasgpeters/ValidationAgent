package com.tgp.object; 

/**
 * Write a description of class DataObjectBase here.
 * 
 * @author Thomas Peters 
 * @version 1.0
 */

import java.lang.reflect.Field;

public class DataObjectBase {
    
    // Valid Data types
    public static final String CHARACTER_TYPE           = "CHARACTER";
    public static final String ZONED_NUMERIC_TYPE       = "ZONED NUMERIC";
    public static final String ZONED_UNSIGNED_TYPE      = "ZONED_UNSIGNED";
    public static final String RECORD_TYPE              = "RECORD";
    public int DATA_ELEMENT_SIZE;

    // Data
    protected char[] data_elements;

    public DataObjectBase() {
        // initialise instance variables
    }

    public void setData(String fieldName, String value) throws Exception {
        try {
            Field sizeField = this.getClass().getField(fieldName + "_SIZE");
            Field offsetField = this.getClass().getField(fieldName + "_OFFSET");
            Field typeField = this.getClass().getField(fieldName + "_TYPE");
        
            int sizeValue = sizeField.getInt(this);
            int offsetValue = offsetField.getInt(this);
            String typeValue = (String)typeField.get(this);
        
            // adjust the size of the passed value.
            if(value.length() < sizeValue) {
                for (int i = value.length(); i < sizeValue; i++) {
                    value = value.concat("+");  
                }
            } else if (value.length() > sizeValue) {
                value = value.substring(0, sizeValue - 1);
            }
    
            //System.out.println("Passed Value:" + value);
            //System.out.println("Field Name:" + fieldName);
            //System.out.println("Field Size:" + sizeValue);
            //System.out.println("Field Offset:" + offsetValue);
            //System.out.println("Field Type:" + typeValue);

            if (value.length() > sizeValue)
                throw new Exception("Exception thrown because there is a size missmatch error.");
            
            char[] newValue = value.toCharArray();
        
            int j = offsetValue;
            for (int i = 0; i <= sizeValue - 1; i++) {
                try {
                    data_elements[j++] = newValue[i];
                } catch (ArrayIndexOutOfBoundsException aioobe) {
                    System.out.println("ArrayIndexOutOfBoundsException occurred while going through the passed value.");
                    aioobe.printStackTrace();
                }
            }   
        
    // } catch (NoSuchFieldException nsfe) {
    // } catch (SecurityException se)
    // } catch (IllegalArgumentException iarge) {
    // } catch (IllegalAccessException iacce) {
    // } catch {NullPointerException nps) {
    // } catch (ExceptionInInitializerError eiie) {
        
        } catch (Exception e) {
            System.out.println("Exception of type " + e.getClass().getName() + " occurred while trying to set data in " + this.getClass().getName());
            e.printStackTrace();
        }
    }

    public String getData() {
        return String.copyValueOf(data_elements);
    }
    
    public String getData(String fieldName) {
    
        String newValue = new String();
    
        try {
            Field sizeField = this.getClass().getField(fieldName + "_SIZE");
            Field offsetField = this.getClass().getField(fieldName + "_OFFSET");
            Field typeField = this.getClass().getField(fieldName + "_TYPE");

            int sizeValue = sizeField.getInt(this);
            int offsetValue = offsetField.getInt(this);
            String typeValue = (String)typeField.get(this);

        //System.out.println("Field Name:" + fieldName);
        //System.out.println("Field Size:" + sizeValue);
        //System.out.println("Field Offset:" + offsetValue);
        //System.out.println("Field Type:" + typeValue);

            try {
                newValue = new String(data_elements, offsetValue, sizeValue);
                    
            } catch (IndexOutOfBoundsException ioobe) {
                System.out.println("IndexOutOfBoundsException occurred while going through the passed value.");
                ioobe.printStackTrace();
            } catch (NullPointerException npe) {
                System.out.println("NullPointerException occurred while going through the passed value.");
                npe.printStackTrace();
            }

    // } catch (NoSuchFieldException nsfe) {
    // } catch (SecurityException se)
    // } catch (IllegalArgumentException iarge) {
    // } catch (IllegalAccessException iacce) {
    // } catch {NullPointerException nps) {
    // } catch (ExceptionInInitializerError eiie) {
        
        } catch (Exception e) {
            System.out.println("Exception of type " + e.getClass().getName() + " occurred while trying to get data in " + this.getClass().getName());
            e.printStackTrace();
        }

        return newValue;    
    }
}
