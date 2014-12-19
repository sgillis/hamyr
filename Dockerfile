FROM haskell:7.8

RUN apt-get update && apt-get install -y \
    tmux \
    curl \
    exuberant-ctags \
    git \
    make \
    libcurl4-gnutls-dev \
    libncurses5-dev \
    libgnome2-dev \
    libgnomeui-dev \
    libatk1.0-dev \
    libbonoboui2-dev \
    libcairo2-dev \
    libx11-dev \
    libxpm-dev \
    libxt-dev \
    python-dev \
    ruby-dev \
    mercurial

RUN hg clone https://code.google.com/p/vim/
WORKDIR /vim
RUN ./configure --with-features=huge \
            --enable-multibyte \
            --enable-rubyinterp \
            --enable-pythoninterp \
            --with-python-config-dir=/usr/lib/python2.7/config \
            --enable-perlinterp \
            --enable-luainterp \
            --enable-cscope --prefix=/usr && \
    make VIMRUNTIMEDIR=/usr/share/vim/vim74 && \
    make install && \
    update-alternatives --install /usr/bin/editor editor /usr/bin/vim 1 && \
    update-alternatives --set editor /usr/bin/vim && \
    update-alternatives --install /usr/bin/vi vi /usr/bin/vim 1 && \
    update-alternatives --set vi /usr/bin/vim
WORKDIR /

RUN curl -o - https://raw.githubusercontent.com/begriffs/haskell-vim-now/master/install.sh | bash

#RUN cabal update && cabal install yesod-bin && \
#    echo "export PATH=$PATH" >> /root/.bashrc
