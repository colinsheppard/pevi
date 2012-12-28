package testpevi;

import static org.junit.Assert.*;

import javax.xml.ws.Action;

import junit.framework.Assert;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.nlogo.api.ErrorSource;
import org.nlogo.headless.HeadlessWorkspace;

public class TestDriver {
	HeadlessWorkspace workspace = HeadlessWorkspace.newInstance();

	@Before
	public void setUp() throws Exception {
		try {
			workspace.open("/Users/critter/Dropbox/serc/pev-colin/pevi/netlogo/PEVI.nlogo");
			workspace.command("setup");
			workspace.command("set test-driver min-one-of drivers [who]");
		}catch(Exception ex) {
			ex.printStackTrace();
		}
	}
	@After
	public void tearDown() throws Exception {
		try {
			workspace.dispose();
		}catch(Exception ex) {
			ex.printStackTrace();
		}
	}

	@Test
	public void updateSOC() {
		Double actualSOC = 0.0;
		Double expectedSOC = 0.0;
		try{
			workspace.command("ask test-driver [ " +
					"set total-trip-dist 1 " +
					"update-soc ]");
			actualSOC = (Double) workspace.report("[state-of-charge] of test-driver");
			expectedSOC = (Double) workspace.report("[1 - electric-fuel-consumption / battery-capacity] of test-driver");
			Assert.assertEquals(expectedSOC,actualSOC);
		} catch(Exception ex) {
			ex.printStackTrace();
		}
	}
	@Test
	public void test2() {
		Double stateOfCharge = 0.0;
		try{
			workspace.command("ask min-one-of drivers [who] [ depart arrive ]");
			stateOfCharge = (Double) workspace.report("[state-of-charge] of min-one-of drivers [who]");
			Assert.assertEquals(0.71025,stateOfCharge);
		} catch(Exception ex) {
			ex.printStackTrace();
		}
	}
}