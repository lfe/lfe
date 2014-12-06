# Base image
#
# VERSION 0.1
FROM debian
MAINTAINER LFE Maintainers <maintainers@lfe.io>
ADD . /opt/erlang/lfe
RUN cd /opt/erlang/lfe && make install
CMD ["lfe"]
