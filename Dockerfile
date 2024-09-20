FROM ubuntu:22.04
RUN mkdir -p /opt/landcons \
        && apt-get update \
        && apt-get install -y --no-install-recommends build-essential zlib1g-dev libpq-dev libicu-dev \
        && apt-get clean \
        && rm -rf /var/lib/apt/lists/*

ARG YESOD_DEMO_LANG=EN

WORKDIR       /opt/landcons
COPY landcons /opt/landcons
COPY static   /opt/landcons/static
COPY config   /opt/landcons/config
COPY demo     /opt/landcons/demo

ENV YESOD_PORT=8080
ENV YESOD_DEMO_LANG=${YESOD_DEMO_LANG}

EXPOSE 8080
CMD ["./landcons"]
