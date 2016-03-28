# Base image
#
# VERSION 0.2
FROM lfex/lfe
MAINTAINER LFE Maintainers <maintainers@lfe.io>

ENV DEBIAN_FRONTEND noninteractive
RUN apt-get update
RUN apt-get -f install -y
RUN apt-get install -y pandoc groff groff-base bsdmainutils
RUN apt-get install -y locales
RUN locale-gen en_US.UTF-8 UTF-8
RUN sh -c "echo -e 'en_US.UTF-8 UTF-8' >> /etc/locale.gen"
RUN dpkg-reconfigure locales

ENV LANG en_US.UTF-8
ENV LANGUAGE en_US.UTF-8
ENV LC_CTYPE en_US.UTF-8

CMD cd /opt/erlang/lfe && make docs && cp -r doc/* /docs
