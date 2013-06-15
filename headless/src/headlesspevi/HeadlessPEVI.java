package headlesspevi;

import org.nlogo.headless.HeadlessWorkspace;

public class HeadlessPEVI {
	public static void main(String[] argv) {
		System.setProperty("java.awt.headless", "true");

		HeadlessWorkspace workspace = HeadlessWorkspace.newInstance();
		try {
			workspace.open("../netlogo/PEVI.nlogo");
			workspace.command("setup-and-fix-seed");
			workspace.command("set go-until-time 30.0");
			workspace.command("go-until") ;
			workspace.command("__clear-all-and-reset-ticks") ;
			System.out.println("run complete");
			
			Thread.sleep(400000);
			workspace.dispose();
		}
		catch(Exception ex) {
			ex.printStackTrace();
		}
	}
}