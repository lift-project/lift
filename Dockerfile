FROM ubuntu:15.10

RUN (echo "deb http://dl.bintray.com/sbt/debian /" | tee -a /etc/apt/sources.list.d/sbt.list) && \
    apt-get update && apt-get install -y --force-yes \
     clang \
     cmake \
     g++ \
     libclang-dev \
     llvm \
     sbt \
     unzip

ADD . lift_src/

RUN cd lift_src && echo y | ./skelcl.sh

# RUN sbt compile
