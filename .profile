export EDITOR=vim

export PS1="\[\033[00;32m\]\u@\h \[\033[00;34m\]\w \$(if [ \$? -gt 0 ];
then echo '\[\033[00;31m\]:(' ; else echo '\[\033[00;32m\]:)'; fi)
\[\033[00;34m\]\$\[\033[00m\] "

function stopwatch(){
  date1=`date +%s`;
   while true; do
    echo -ne "$(date -u --date @$((`date +%s` - $date1)) +%H:%M:%S)\r";
    sleep 0.1
   done
}

if [ -d $(brew --prefix)/etc/bash_completion.d ]; then
    for f in $(brew --prefix)/etc/bash_completion.d/*
    do
        source $f
    done
fi

# pkgconfig for brew installs
export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig/

# grab some bins
homebin=~/bin
homebrew=/usr/local/bin
coreutils="$(brew --prefix coreutils)/libexec/gnubin"
sed="$(brew --prefix gnu-sed)/libexec/gnubin"

export PATH=$homebin:$homebrew:$coreutils:$sed:$PATH
export MANPATH="$(brew --prefix coreutils)/libexec/gnuman:$(brew --prefix gnu-sed)/libexec/gnuman:$MANPATH"

ssh-add ~/.ssh/github
ssh-add ~/.ssh/heroku

fortune | cowsay
