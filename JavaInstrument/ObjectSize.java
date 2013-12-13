package JavaInstrument;

import java.lang.instrument.Instrumentation;

public class ObjectSize {

	private static Instrumentation instr;
	public static void premain(String args, Instrumentation inst){
		instr = inst;
	}
	public static long getObjectSize(Object o){
		return instr.getObjectSize(o);
	}
}
