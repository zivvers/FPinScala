FROM davisengeler/docker-scala
RUN apt-get update
RUN apt-get -y install vim 
WORKDIR /work
RUN apt-get -y install wget
RUN wget -O ~/.vimrc https://raw.githubusercontent.com/zivvers/reference_files/master/vimrc
RUN mkdir -p ~/.vim/syntax
RUN wget -P ~/.vim/syntax https://raw.githubusercontent.com/zivvers/reference_files/master/scala.vim
