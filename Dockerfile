# Base image
#
# VERSION 0.1
FROM debian:jessie
MAINTAINER LFE Maintainers <maintainers@lfe.io>

RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential \
    curl \
    erlang \
    git

ENV REBAR_BALL https://github.com/rebar/rebar/archive/2.5.1.tar.gz
RUN curl $REBAR_BALL | tar xvz && \
    cd 2.5.1 && \
    ./bootstrap && \
    cp rebar /usr/local/bin

ADD . /opt/erlang/lfe
RUN cd /opt/erlang/lfe && make install

CMD ["lfe"]
