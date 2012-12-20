package headlesspevi;

import org.nlogo.headless.HeadlessWorkspace;

public class HeadlessPEVI {
	public static void main(String[] argv) {
		System.setProperty("java.awt.headless", "true");

		HeadlessWorkspace workspace = HeadlessWorkspace.newInstance();
		try {
			workspace.open("../netlogo/PEVI.nlogo");			
			workspace.command("setup");
			workspace.command("go") ;
			workspace.command("summarize") ;
			System.out.println(workspace.report("sum [ sum itin-delay-amount  ] of drivers"));
			workspace.dispose();
		}
		catch(Exception ex) {
			ex.printStackTrace();
		}
	}
}