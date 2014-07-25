export EDITOR=vim

export PS1="\[\033[01;32m\]\u@\h \[\033[01;34m\]\w \$(if [ \$? -gt 0 ]; 
then echo '\[\033[01;31m\]:(' ; else echo '\[\033[01;32m\]:)'; fi) 
\[\033[01;34m\]\$\[\033[00m\] "

if [ -f $(brew --prefix)/etc/bash_completion ]; then
    . $(brew --prefix)/etc/bash_completion
fi

# pkgconfig for brew installs
export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig/

# grab some bins
homebin=~/bin
homebrew=/usr/local/bin
cabal=~/.cabal/bin
gems="$(brew --prefix ruby)/bin"
coreutils="$(brew --prefix coreutils)/libexec/gnubin"

export PATH=$homebin:$cabal:$homebrew:$gems:$coreutils:$PATH

export MANPATH="$(brew --prefix coreutils)/libexec/gnuman:$MANPATH"

ssh-add ~/.ssh/github
ssh-add ~/.ssh/heroku

fortune | cowsay
