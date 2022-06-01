FROM erlang:24-alpine as message_broker

RUN mkdir /message_broker
WORKDIR /message_broker

COPY src src/
COPY rebar.config ./

RUN rebar3 compile