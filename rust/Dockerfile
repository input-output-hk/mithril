FROM ubuntu
RUN apt-get update && apt-get install -y \
    curl \
    gcc \
    && rm -rf /var/lib/apt/lists/*

RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y

COPY . /mithril

RUN cd /mithril \
    && /root/.cargo/bin/cargo build
