FROM lift:dev

ENTRYPOINT [ "/opt/intellij/bin/idea.sh" ]

RUN wget http://download.jetbrains.com/idea/ideaIC-2017.3-no-jdk.tar.gz -O /tmp/intellij.tar.gz -q && \
    echo 'Installing IntelliJ IDEA' && \
    mkdir -p /opt/intellij && \
    tar -xf /tmp/intellij.tar.gz --strip-components=1 -C /opt/intellij && \
    rm /tmp/intellij.tar.gz