
# Setting PATH for Python 2.7
# The orginal version is saved in .bash_profile.pysave
PATH="/Library/Frameworks/Python.framework/Versions/2.7/bin:${PATH}"
export PATH

##
# Your previous /Users/abinsimon/.bash_profile file was backed up as /Users/abinsimon/.bash_profile.macports-saved_2015-10-23_at_16:04:25
##

# MacPorts Installer addition on 2015-10-23_at_16:04:25: adding an appropriate PATH variable for use with MacPorts.
export PATH="/opt/local/bin:/opt/local/sbin:$PATH"
# Finished adapting your PATH environment variable for use with MacPorts.

#setting up the profile for recognising the bsh profile
source ~/.bashrc

#colouring up the terminal session
export CLICOLOR=1
export LSCOLORS=ExFxBxDxCxegedabagacad
alias ls='ls -GFh'
