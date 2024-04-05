FROM rust

WORKDIR /usr/jpp

RUN apt-get update
RUN apt-get -y upgrade
RUN apt-get install gnat -y
RUN apt-get install golang -y
RUN apt-get install ghc ghc-prof ghc-doc -y
RUN apt-get install swi-prolog -y

COPY . .
