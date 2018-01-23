FROM java:8-jdk
LABEL website = "http://www.lift-project.org"
LABEL maintainer = "michel.steuwer@glasgow.ac.uk"

# install opencl, dependencies for executor library, and sbt
RUN sed -e 's/$/ contrib non-free/' -i /etc/apt/sources.list \
  && apt-get update && apt-get install --no-install-recommends -y \
    opencl-headers \
    ocl-icd-opencl-dev \
    amd-opencl-icd \
    clinfo \
    cmake \
    make \
    g++ \
    wget \
    apt-transport-https \
  && ( echo "deb https://dl.bintray.com/sbt/debian /" \
     | tee -a /etc/apt/sources.list.d/sbt.list) \
  && apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 \
                 --recv 2EE0EA64E40A89B84B2DF73499E82A75642AC823 \
  && apt-get update && apt-get install -y \
    sbt

RUN git clone https://github.com/lift-project/lift.git /lift \
  && ( cd /lift \
    && git submodule init \
    && git submodule update \
    && ./buildExecutor.sh )