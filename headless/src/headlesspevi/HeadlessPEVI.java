package headlesspevi;

import org.nlogo.headless.HeadlessWorkspace;

public class HeadlessPEVI {
	public static void main(String[] argv) {
		System.setProperty("java.awt.headless", "true");

		HeadlessWorkspace workspace = HeadlessWorkspace.newInstance();
		try {
			workspace.open("../netlogo/PEVI.nlogo");
			workspace.command("__clear-all-and-reset-ticks");
			workspace.command("set parameter-file \"../../../pev-shared/data/inputs/sensitivity/base/params.txt\"");
			workspace.command("setup");
			workspace.command("go") ;
			workspace.command("summarize") ;
			workspace.dispose();
		}
		catch(Exception ex) {
			ex.printStackTrace();
		}
	}
}