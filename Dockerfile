FROM erlang:21 AS builder


WORKDIR /app/src

ADD . /app/src

RUN make deps app

RUN make rel



FROM debian:stretch

RUN apt-get update && apt-get install -y openssl && apt-get clean

COPY --from=builder /app/src/_rel/scalerl_release/scalerl_release-1.tar.gz /app.tar

WORKDIR /app

RUN tar -xzf /app.tar

CMD ["/app/bin/scalerl_release", "foreground"]
