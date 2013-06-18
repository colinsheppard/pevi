
# install colin's miscellaneous r functions
install.packages('colinmisc_0.1.tar.gz',repos=NULL,type="source")

# speciall command to install correct version of RNetLogo
install.packages("RNetLogo", repos="http://R-Forge.R-project.org")

# install other R dependencies
install.packages(c('yaml','RNetLogo','plyr','reshape','stringr','snow'),repos='http://cran.us.r-project.org')
# if the above fails, try this
#install.packages(c('yaml','RNetLogo','plyr','reshape','stringr','snow'),repos='http://cran.us.r-project.org',type='source')
