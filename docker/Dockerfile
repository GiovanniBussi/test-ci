# Start from latest ubuntu
FROM centos:7

# this is home for root user
WORKDIR /root

# import plumed code.
# assumes a file plumed2.tgz is present in the Dockerfile directory
COPY plumed2.tgz /root/

# install libraries and plumed
RUN \
    yum -y update \
 && yum -y group install "Development Tools" \
 && yum -y group install gawk xxd git \
 && tar xzf plumed2.tgz \
 && cd plumed2 \
 && ./configure \
 && make -j 4 \
 && make install \
 && cd ../ \
 && rm -fr plumed2 \
 && rm -f plumed2.tgz

# switch to plumed user
RUN useradd -ms /bin/bash plumed
USER plumed
WORKDIR /home/plumed

# by default enter bash
CMD /bin/bash

