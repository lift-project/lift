FROM java:8-jdk

# install opencl, dependencies for SkelCL, and prepare installation of sbt
RUN sed -e 's/$/ contrib non-free/' -i /etc/apt/sources.list \
  && apt-get update && apt-get install -y \
    opencl-headers \
    ocl-icd-opencl-dev \
    amd-opencl-icd \
    clinfo \
    cmake \
    g++ \
    wget \
    libclang-dev \
    libssl-dev \
    llvm \
    apt-transport-https

# install sbt
RUN (  echo "deb https://dl.bintray.com/sbt/debian /" \
     | tee -a /etc/apt/sources.list.d/sbt.list) \
  && apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 \
                 --recv 2EE0EA64E40A89B84B2DF73499E82A75642AC823 \
  && apt-get update && apt-get install -y \
    sbt

