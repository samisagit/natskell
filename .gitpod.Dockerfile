FROM gitpod/workspace-full

SHELL ["/bin/bash", "-c"]

USER root

# Install requirements for adding PPAs
RUN apt-get update
RUN apt-get install gnupg software-properties-common -y

# Add PPA for kr
RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys C4A05888A1C4FA02E1566F859F2A29A569653940
RUN add-apt-repository "deb http://kryptco.github.io/deb kryptco main"

# Install packages we don't care about versions of
RUN apt-get update
RUN apt-get install -y tmux curl zsh git build-essential curl libffi-dev libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5 dirmngr apt-transport-https kr

# Install recent neovim
RUN curl -LO https://github.com/neovim/neovim/releases/download/v0.7.0/nvim-linux64.deb && apt-get install ./nvim-linux64.deb

# create root directories that stack uses
RUN mkdir /.stack-work
RUN chown gitpod /.stack-work

USER gitpod
# Install ohmyzsh
RUN sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
RUN echo "export GPG_TTY=$(tty)" >> ~/.zshrc

# Install haskell stack
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 bash

RUN source ~/.ghcup/env && source ~/.bashrc && ghcup install ghc 9.2.2
RUN source ~/.ghcup/env && source ~/.bashrc && ghcup set ghc 9.2.2
RUN source ~/.ghcup/env && source ~/.bashrc && ghcup install stack
RUN source ~/.ghcup/env && source ~/.bashrc && ghcup install hls
RUN source ~/.ghcup/env && source ~/.bashrc && cabal update

# Install project dependencies
COPY natskell.cabal .
COPY stack.yaml .
COPY stack.yaml.lock .
RUN source ~/.ghcup/env && source ~/.bashrc && stack build --dependencies-only --system-ghc
RUN echo "source ~/.ghcup/env" >> ~/.zshrc

# Add trailing user command to ensure non root
USER gitpod

ENV STACK_ROOT=/workspace/natskell/.stack
