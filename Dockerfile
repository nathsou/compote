FROM ubuntu:22.04

ENV DEBIAN_FRONTEND=noninteractive

# Base dependencies: build tools, curl, git, and SSL certs
RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        build-essential \
        curl \
        ca-certificates \
        git \
        pkg-config \
        libssl-dev \
    && rm -rf /var/lib/apt/lists/*

ARG RUST_VERSION=1.88.0
# Install a specific Rust version via rustup
RUN curl -sSf https://sh.rustup.rs | sh -s -- -y --default-toolchain ${RUST_VERSION}
ENV PATH="/root/.cargo/bin:${PATH}"

# Install a specific MoonBit CLI version
RUN curl -fsSL https://cli.moonbitlang.com/install/unix.sh | bash -s '0.6.25+d6913262c'
ENV PATH="/root/.moon/bin:${PATH}"

# Default workdir; actual repo will be bind-mounted by the workflow
WORKDIR /workspace

# Show versions for easier debugging in CI logs
RUN rustc -V && cargo -V && python3 -V && moonc -v || true
