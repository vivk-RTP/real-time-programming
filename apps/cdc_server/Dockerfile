FROM erlang:latest as cdc_server

RUN mkdir /cdc_server
WORKDIR /cdc_server

COPY src src/
COPY rebar.config ./

RUN rebar3 compile