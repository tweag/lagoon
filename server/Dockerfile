FROM tweag/datalake

RUN mkdir -p /app

ADD docker-build/datalake-server docker-boot-server.sh /app/

EXPOSE 1234

CMD /app/docker-boot-server.sh
