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
gems="$(brew --prefix ruby)/bin"
coreutils="$(brew --prefix coreutils)/libexec/gnubin"
go="$(brew --prefix go)/libexec/bin"
sed="$(brew --prefix gnu-sed)/libexec/gnubin"

export GOPATH=/Users/waskew/Programming/Go
export PATH=$homebin:$homebrew:$gems:$coreutils:$GOPATH/bin:$go:$sed:$PATH
export MANPATH="$(brew --prefix coreutils)/libexec/gnuman:$(brew --prefix gnu-sed)/libexec/gnuman:$MANPATH"

# set up rbenv
eval "$(rbenv init -)"

fortune | cowsay
