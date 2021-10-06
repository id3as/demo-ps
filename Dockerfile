FROM ubuntu:20.10

# Sigh
RUN apt update

# Erlang 22-3
RUN DEBIAN_FRONTEND="noninteractive" apt-get install -y git bash erlang curl build-essential

RUN groupadd --gid 1000 dev \
  && useradd --uid 1000 --gid dev --shell /bin/bash --create-home dev

# Rebar3
RUN cd  /opt/ \
  && curl https://rebar3.s3.amazonaws.com/rebar3 > /usr/local/bin/rebar3 \
  && chmod +x /usr/local/bin/rebar3 

# Purescript 
RUN cd /opt/ \
    && curl -L https://github.com/purescript/purescript/releases/download/v0.14.4/linux64.tar.gz > purescript.tar.gz \
    && tar -xvf purescript.tar.gz \
    && cp purescript/purs /usr/local/bin/purs  \
    && rm purescript.tar.gz

# Purerl
RUN cd /opt/ \
    && curl -L https://github.com/purerl/purerl/releases/download/v0.0.12/linux64.tar.gz > purerl.tar.gz \
    && tar -xvf purerl.tar.gz \
    && cp purerl/purerl /usr/local/bin/purerl  \
    && rm purerl.tar.gz

# Spago
RUN cd /opt/ \
    && curl -L https://github.com/purescript/spago/releases/download/0.20.3/linux.tar.gz > spago.tar.gz \
    && tar -xvf spago.tar.gz \
    && cp spago /usr/local/bin/spago  \
    && rm spago.tar.gz

# Dhall
RUN cd /opt/ \
    && curl -L https://github.com/dhall-lang/dhall-haskell/releases/download/1.33.1/dhall-1.33.1-x86_64-linux.tar.bz2 > dhall-json.tar.bz2 \
    && tar -xjvf dhall-json.tar.bz2 \
    && cp bin/dhall /usr/local/bin/dhall  \
    && rm dhall-json.tar.bz2
