FROM rust:1.67

WORKDIR /usr/jpp

RUN apt-get update
RUN apt-get -y upgrade
RUN apt-get install gnat -y
RUN apt-get install -y gprbuild
RUN apt-get install golang -y
RUN apt-get install ghc ghc-prof ghc-doc -y
RUN apt-get install swi-prolog -y
RUN apt-get install smlnj -y
RUN apt-get install clisp -y

COPY . .
