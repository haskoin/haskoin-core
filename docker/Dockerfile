FROM ubuntu:14.04
MAINTAINER Jean-Pierre Rupp <root@haskoin.com>

RUN apt-get update
RUN apt-get install -y \
  git \
  wget \
  libleveldb-dev \
  libzmq3-dev \
  libsnappy-dev \
  pkg-config \
  zlib1g-dev
RUN wget -q -O- \
  https://s3.amazonaws.com/download.fpcomplete.com/ubuntu/fpco.key \
  | apt-key add -
RUN echo 'deb http://download.fpcomplete.com/ubuntu/trusty stable main' \
  > /etc/apt/sources.list.d/fpco.list
RUN apt-get update && apt-get install -y stack
WORKDIR /usr/local/src
RUN git clone https://github.com/haskoin/haskoin-wallet.git
WORKDIR /usr/local/src/haskoin-wallet
RUN stack setup
RUN stack install
RUN mv /root/.local/bin/hw /usr/local/bin
RUN rm -rf /root/.stack /root/.local
WORKDIR /root
CMD [ "/usr/local/bin/hw" ]
