library('colinmisc')
load.libraries(c('plyr','RNetLogo'))
Sys.setenv(NOAWT=TRUE) 

path.to.hevi <- '~/Dropbox/serc/pev-colin/hevi'
path.to.data <- '~/Dropbox/serc/pev-colin/data'
nl.path <- '/Applications/NetLogo 5.0'
nl.version <- 5

set.dir('',path.to.hevi)

# start RNetLogo

# Note, cannot run in GUI mode on OS X, got the following exception:
#java.awt.HeadlessException
	#at java.awt.GraphicsEnvironment.checkHeadless(GraphicsEnvironment.java:159)
	#at java.awt.Window.<init>(Window.java:431)
	#at java.awt.Frame.<init>(Frame.java:403)
	#at java.awt.Frame.<init>(Frame.java:368)
	#at javax.swing.SwingUtilities$SharedOwnerFrame.<init>(SwingUtilities.java:1739)
	#at javax.swing.SwingUtilities.getSharedOwnerFrame(SwingUtilities.java:1816)
	#at javax.swing.JOptionPane.getRootFrame(JOptionPane.java:1673)
	#at javax.swing.JOptionPane.showOptionDialog(JOptionPane.java:846)
	#at javax.swing.JOptionPane.showMessageDialog(JOptionPane.java:650)
	#at javax.swing.JOptionPane.showMessageDialog(JOptionPane.java:621)
	#at nlcon.NLink_v5.<init>(NLink_v5.java:108)

NLStart(nl.path,gui=F,obj.name='nl.handle',nl.version=nl.version)

NLLoadModel(paste(path.to.hevi,'/HEVI.nlogo',sep=''),nl.handle)

NLCommand("set driver-input-file 'p1r1.txt'",nl.handle)
