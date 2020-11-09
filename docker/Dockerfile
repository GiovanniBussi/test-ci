# Start from latest ubuntu
FROM centos:7

RUN \
    sudo yum -y update \
 && sudo yum -y group install "Development Tools" \
 && sudo yum -y install gawk vim zlib gsl-devel fftw-devel

RUN useradd -ms /bin/bash plumed
USER plumed
WORKDIR /home/plumed

# import plumed code.
# assumes a file plumed2.tgz is present in the Dockerfile directory
COPY plumed2.tgz /home/plumed

# install and test plumed
RUN \
 && tar xzf plumed2.tgz \
 && cd plumed2 \
 && ./configure \
 && make -j 4 \
 && make install \
 && make installcheck

