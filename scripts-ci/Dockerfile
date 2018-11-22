# Docker image to test the static analyzer for the TAS course
# Build with: docker build -t tas .
#
###############################################

FROM debian:stable

ENV TERM xterm-256color

RUN \
  apt-get update && \
  apt-get install -y --no-install-recommends m4 libgmp3-dev libmpfr-dev ocaml-nox ocaml-native-compilers wget aspcud ca-certificates opam && \
  adduser --disabled-password --gecos 'TAS Project' tas

COPY . /home/tas
RUN chown -R tas:tas /home/tas

USER tas
WORKDIR /home/tas

RUN \
    opam init -y && \
    opam config setup -a && \
    eval `opam config -y env` && \
    opam update -y && \
    opam switch -y 4.07.0 && \
    eval `opam config -y env` && \
    opam install -y menhir zarith mlgmpidl apron && \
    echo && \
    echo "All done!" && \
    uname -a && \
    echo opam `opam --version` && \
    ocamlc -v
