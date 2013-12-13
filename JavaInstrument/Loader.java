package JavaInstrument;

public class Loader {
	public static void main (String [] args) throws Exception
	{
		if(args.length > 0){
			for(String clazz : args){
				Object testobj = Class.forName(clazz).newInstance();
				System.out.println(clazz+" "+ObjectSize.getObjectSize(testobj));
			}
		}
		else throw new RuntimeException("Specify target class name");
	}

}
