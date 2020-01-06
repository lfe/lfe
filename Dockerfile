# Base image
#
# VERSION 0.2
FROM debian:latest
MAINTAINER LFE Maintainers <maintainers@lfe.io>

ENV DEBIAN_FRONTEND noninteractive
RUN apt-get update
RUN apt-get install -y --no-install-recommends \
    apt-utils \
    build-essential
RUN apt-get install -y --no-install-recommends \
    ca-certificates \
    libcurl4-openssl-dev \
    curl \
    wget \
    git
RUN apt-get install -y --no-install-recommends \
    libsctp1 libsctp-dev lksctp-tools
RUN apt-get install -y --no-install-recommends \
    pandoc

ENV ERLANG_DEB1 erlang-solutions_1.0_all.deb
ENV ERLANG_DEB2 esl-erlang_19.1.3-1~debian~jessie_amd64.deb
ENV ERLANG_HOST https://packages.erlang-solutions.com
ENV ERLANG_PATH erlang/esl-erlang/FLAVOUR_1_general
RUN curl -L -O $ERLANG_HOST/$ERLANG_DEB1
RUN dpkg -i $ERLANG_DEB1 && rm $ERLANG_DEB1
RUN apt-get update
RUN curl -L -O $ERLANG_HOST/$ERLANG_PATH/$ERLANG_DEB2
RUN dpkg -i --force-depends $ERLANG_DEB2 && rm $ERLANG_DEB2

ENV REBAR_REPO https://github.com/rebar/rebar.git
RUN git clone $REBAR_REPO && cd rebar && \
    git checkout tags/2.6.0 && \
    ./bootstrap && \
    cp rebar /usr/local/bin

ADD . /opt/erlang/lfe
RUN cd /opt/erlang/lfe && make install

ENV ERL_LIBS=$ERL_LIBS:/opt/erlang/lfe
ENV DEBIAN_FRONTEND ""
CMD /usr/bin/lfe -eval "(io:format \"~p~n\"  (list (* 2 (lists:foldl #'+/2 0 (lists:seq 1 6)))))"
