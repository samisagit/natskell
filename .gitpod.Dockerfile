FROM gitpod/workspace-base

RUN sudo apt-get update
RUN sudo apt-get install -y tmux

RUN sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"

RUN curl -LO https://github.com/neovim/neovim/releases/download/v0.7.0/nvim-linux64.deb && sudo apt-get install ./nvim-linux64.deb
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 bash

RUN bash -c "source ~/.ghcup/env && source ~/.bashrc && ghcup install stack"
RUN bash -c "source ~/.ghcup/env && source ~/.bashrc && ghcup install hls"
RUN bash -c "source ~/.ghcup/env && source ~/.bashrc && cabal update"

COPY natskell.cabal .
COPY stack.yaml .
COPY stack.yaml.lock .
RUN bash -c "source ~/.ghcup/env && source ~/.bashrc && stack build --dependencies-only --system-ghc"

USER root

RUN sudo echo "StreamLocalBindUnlink yes" > /etc/ssh/sshd_config

USER gitpod
