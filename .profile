export EDITOR=vim

export PS1="\[\033[00;32m\]\u@\h \[\033[00;34m\]\w \$(if [ \$? -gt 0 ];
then echo '\[\033[00;31m\]:(' ; else echo '\[\033[00;32m\]:)'; fi)
\[\033[00;34m\]\$\[\033[00m\] "

stopwatch(){
  date1=$(date +%s);
  while true; do
    echo -ne "$(date -u --date @$(($(date +%s) - date1)) +%H:%M:%S)\r";
    sleep 0.1
  done
}

[[ -r "/usr/local/etc/profile.d/bash_completion.sh" ]] && . "/usr/local/etc/profile.d/bash_completion.sh"

# pkgconfig for brew installs
export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig/

# grab some bins
homebin=~/bin
homebrew=/usr/local/bin
coreutils="$(brew --prefix coreutils)/libexec/gnubin"
sed="$(brew --prefix gnu-sed)/libexec/gnubin"

export PATH=$homebin:$homebrew:$coreutils:$sed:$PATH

coreutils_man="$(brew --prefix coreutils)/libexec/gnuman"
sed_man="$(brew --prefix gnu-sed)/libexec/gnuman"
export MANPATH="$coreutils_man:$sed_man:$MANPATH"

ssh-add ~/.ssh/github

fortune | cowsay
