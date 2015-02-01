# Base image
#
# VERSION 0.1
FROM debian:jessie
MAINTAINER LFE Maintainers <maintainers@lfe.io>

RUN apt-get update && apt-get install -y --no-install-recommends \
    apt-utils \
    build-essential \
    erlang \
    ca-certificates \
    libcurl4-openssl-dev \
    curl \
    git

ENV REBAR_REPO https://github.com/rebar/rebar.git
RUN git clone $REBAR_REPO && cd rebar && \
    git checkout tags/2.5.1 && \
    ./bootstrap && \
    cp rebar /usr/local/bin

ADD . /opt/erlang/lfe
RUN cd /opt/erlang/lfe && make install

ENV ERL_LIBS=$ERL_LIBS:/opt/erlang/lfe
CMD /usr/bin/lfe -eval "(io:format \"~p~n\"  (list (* 2 (lists:foldl (lambda (n acc) (+ n acc)) 0 (lists:seq 1 6)))))"
