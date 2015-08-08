FROM ubuntu:15.10

RUN (echo "deb http://dl.bintray.com/sbt/debian /" | tee -a /etc/apt/sources.list.d/sbt.list) && \
    apt-get update && apt-get install -y --force-yes \
     clang \
     cmake \
     g++ \
     libclang-dev \
     libedit-dev \
     libssl-dev \
     llvm \
     ocl-icd-opencl-dev \
     openjdk-8-jdk \
     sbt \
     unzip \
     wget \
     zlib1g-dev

ADD . lift_src/

RUN mkdir /root/.ssh/

ADD id_rsa /root/.ssh/id_rsa

RUN chmod 0600 /root/.ssh/id_rsa

RUN touch /root/.ssh/known_hosts

RUN ssh-keyscan bitbucket.org >> /root/.ssh/known_hosts

RUN cd lift_src && git submodule init && git submodule update

RUN cd lift_src && (export JAVA_HOME=/usr/lib/jvm/java-8-openjdk-amd64 && echo y | ./skelcl.sh)

RUN cd lift_src && sbt compile
