FROM debian

RUN apt-get update
RUN apt-get install -y zsh procps
RUN apt-get install -y git curl

ARG USERNAME
ARG GROUPNAME
ARG UID
ARG GID

RUN groupadd --gid $GID $GROUPNAME
RUN useradd --uid $UID --gid $GID --create-home --no-user-group --shell /usr/bin/zsh $USERNAME

WORKDIR /home/$USERNAME

RUN su $USERNAME -c="git clone --recurse-submodules https://github.com/danieldulaney/dotfiles.git .dotfiles"
RUN su $USERNAME -c="/home/$USERNAME/.dotfiles/scripts/setup.sh"
RUN su $USERNAME -c="mkdir working"

CMD ["/usr/bin/zsh"]

