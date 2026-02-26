package com.workflow.client.naf.validate;

public class TestValidate {
	
	public TestValidate() {
		
	}
	
	public static void main(String[] args) {
		
		String param1 = new String();
		String param2 = new String();
		try {
			param1 = args[0];
			param2 = args[1];

			if (param1 != null && ValidateImpl.LIB_LOADED) {
				
				String valueReturned = new String(ValidateImpl.execute(param1.toCharArray()));
				System.out.println("Value Returned form calling 'ValidateImpl.execute()': " + valueReturned);
			}
		} catch (ArrayIndexOutOfBoundsException aioobe) {
			System.out.println("ArrayIndexOutOfBoundsException occurred while running 'TestValidate.main()'");
			aioobe.printStackTrace();
		}
		
		PassListImpl passList = new PassListImpl();
		try {
				
			passList.setData("T_VALIDATE_NO", param1);
			passList.setData("T_ELEMENT", param2);
			
		} catch (Exception e) {
			System.out.println("Exception of type " + e.getClass().getName() + " occurred.");
			System.out.println(e.getMessage());
			e.printStackTrace();
		}
		System.out.println("T_VALIDATE_NO = " + passList.getData("T_VALIDATE_NO"));
		System.out.println("T_ELEMENT = " + passList.getData("T_ELEMENT"));
	}
}
