# Use Ubuntu 22.04 as base image
FROM ubuntu:22.04

# Set environment variable for non-interactive installation
ENV DEBIAN_FRONTEND=noninteractive

# Update apt-get and install dependencies
RUN apt-get update && apt-get install -y \
    build-essential \
    curl \
    git \
    software-properties-common \
    unzip \
    wget \
    ca-certificates \
    libssl-dev \
    libncurses5-dev \
    libbz2-dev \
    libreadline-dev \
    libsqlite3-dev \
    zlib1g-dev \
    libgmp-dev \
    && rm -rf /var/lib/apt/lists/*

# Install OCaml
RUN apt-get update && apt-get install -y ocaml opam

# Install Rust (via rustup)
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y

# Install Go (via official Go binary release)
RUN wget https://go.dev/dl/go1.20.5.linux-amd64.tar.gz \
    && tar -C /usr/local -xzf go1.20.5.linux-amd64.tar.gz \
    && rm go1.20.5.linux-amd64.tar.gz

# Set Go environment variables
ENV PATH="/usr/local/go/bin:${PATH}"

# Clean up apt caches to reduce image size
RUN apt-get clean

# Copy project files into container
WORKDIR /freak
COPY . /freak
# Install freak dependencies
RUN opam install dune base64 eio_main

# Default command (runs bash)
CMD ["/bin/bash"]
