FROM erlang:latest as rtp

RUN mkdir /rtp
WORKDIR /rtp

COPY src src/
COPY rebar.config ./

RUN rebar3 compile