# LFE Docker images are now based entirely upon the official Erlang Docker
# images which include both Debian and Alpine images.
#
# Resources:
# * Erlang images: https://hub.docker.com/_/erlang
# * LFE images: https://hub.docker.com/r/lfex/lfe/tags
# * LFE Dockerfiles repo: https://github.com/lfex/dockerfiles
#
# Note: The last link has lots of usage examples in the README file.
# Note: The latest version is an image which contains the most recently 
#       released LFE and Erlang versions.
#
FROM lfex/lfe:latest
