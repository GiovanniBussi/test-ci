# Start from latest ubuntu
FROM ubuntu:18.04
ARG DEBIAN_FRONTEND=noninteractive

# this is home for root user
WORKDIR /root

# import plumed code.
# assumes a file plumed2.tgz is present in the Dockerfile directory
COPY plumed2.tgz /root/

# install libraries and plumed
RUN buildDeps="git" \
 && runtimeDeps="gawk make vim zlib1g git g++ xxd" \
 && apt-get -yq update \
 && apt-get -yq upgrade \
 && apt -yq install $buildDeps $runtimeDeps --no-install-recommends \
 && tar xzf plumed2.tgz \
 && cd plumed2 \
 && ./configure --enable-modules=all CXXFLAGS=-O3 \
 && make -j 4 \
 && make install \
 && cd ../ \
 && rm -fr plumed2 \
 && rm -f plumed2.tgz \
 && apt-get purge -y --auto-remove $buildDeps \
 && apt -yq install $runtimeDeps --no-install-recommends \
 && rm -rf /var/lib/apt/lists/*

# switch to plumed user
RUN useradd -ms /bin/bash plumed
USER plumed
WORKDIR /home/plumed

# by default enter bash
CMD /bin/bash

