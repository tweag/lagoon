FROM tweag/lagoon

RUN mkdir -p /app

ADD docker-build/lagoon-server docker-boot-server.sh /app/

EXPOSE 1234

CMD /app/docker-boot-server.sh
