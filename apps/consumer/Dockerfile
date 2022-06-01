FROM erlang:latest as consumer

RUN mkdir /consumer
WORKDIR /consumer

COPY src src/
COPY rebar.config ./

RUN rebar3 compile