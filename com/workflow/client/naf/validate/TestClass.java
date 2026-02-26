package com.workflow.client.naf.validate; 

/**
 * The test class TestClass.
 *
 * @author  (your name)
 * @version (a version number or a date)
 */
public class TestClass extends junit.framework.TestCase
{
	String param1 = new String();
	String param2 = new String();

    /**
     * Default constructor for test class TestClass
     */
    
    
    

    public TestClass()
    {
    }

    /**
     * Sets up the test fixture.
     *
     * Called before every test case method.
     */
    protected void setUp()
    {
		param1 = "Hello";
        param2 = "Testing 123";
		param2.toString();
	}

    /**
     * Tears down the test fixture.
     *
     * Called after every test case method.
     */
    protected void tearDown()
    {
    }

    public void test1()
    {
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
        System.out.println("T_ELEMENT = " + passList.getData("T_ELEMENT")); }


	public void testMytest()
	{
	}
}




