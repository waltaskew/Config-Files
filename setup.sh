#! /usr/bin/env bash

DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

for file in .emacs .emacs.d .gitconfig .tmux.conf .vim .vimrc
do
  ln -s $DIR/$file  ~/$file
done

if [ -e ~/.profile ]
then
    echo '' >> ~/.profile
    cat $DIR/.profile >> ~/.profile
else
    ln -s $DIR/.profile ~/.profile
fi
