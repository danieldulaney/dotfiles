FROM debian:buster AS baseline

ARG user=daniel
ARG user_uid=1000

RUN apt-get update
RUN apt-get install -y apt-utils
RUN apt-get install -y curl openssh-client
RUN apt-get install -y vim git tmux procps
RUN apt-get install -y zsh

RUN apt-get install -y texlive
RUN apt-get install -y make cmake
RUN apt-get install -y gcc

RUN useradd --create-home --uid "$user_uid" --shell /usr/bin/zsh "$user"

ADD scripts/setup.sh /tmp/docker/
RUN chmod +x /tmp/docker/*.sh

WORKDIR "/home/$user"
RUN runuser --command=/tmp/docker/setup.sh "$user"

RUN chmod +x /tmp/docker/*.sh
ADD scripts/docker-init.sh /tmp/docker/

USER "$user"
CMD ["/tmp/docker/docker-init.sh"]

