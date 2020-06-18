FROM ubuntu:20.04

ARG UID=1000
ARG GID=1000

RUN groupadd -g $GID -o kontrakcja
RUN useradd -m -u $UID -g $GID -o -s /bin/bash kontrakcja

RUN apt-get update
RUN apt-get upgrade -y
RUN DEBIAN_FRONTEND="noninteractive" apt-get -y install tzdata
RUN apt-get install -y git curl wget make zutils openssh-client

RUN wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb
RUN apt-get install -y ./google-chrome-stable_current_amd64.deb
RUN rm google-chrome-stable_current_amd64.deb

ENV USER kontrakcja
ENV SSH_AUTH_SOCK /var/run/ssh-agent.sock

USER kontrakcja
WORKDIR /data/kontrakcja
