package com.workflow.client.naf.validate;  

public class ValidateImpl {
	
	public static boolean LIB_LOADED = false;
	public static final String VALIDATIONS_LIBRARY = "VALIDATIONS";
	public static native char[] validate(char[] pass_list);
	
	static {
		try {
			System.loadLibrary(VALIDATIONS_LIBRARY);
			LIB_LOADED = true;
			
		} catch (UnsatisfiedLinkError ule) {
			System.out.println("UnsatisfiedLinkError occurred while trying to load library '" + VALIDATIONS_LIBRARY + "'. Library does not exist.");
		} catch (SecurityException se) {
			System.out.println("SecurityException occurred while trying to load " + VALIDATIONS_LIBRARY + " library.");
		}
	}
	
	public static char[] execute(char[] pass_list) {
		
		if (LIB_LOADED)
			return validate(pass_list);
			
		char[] returnValue = new char[11];
		returnValue[0] = 'H';
		returnValue[1] = 'e';
		returnValue[2] = 'l';
		returnValue[3] = 'l';
		returnValue[4] = 'o';
		returnValue[5] = ' ';
		returnValue[6] = 'J';
		returnValue[7] = 'e';
		returnValue[8] = 'r';
		returnValue[9] = 'r';
		returnValue[10] = 'y';
		return returnValue;
	}
}
