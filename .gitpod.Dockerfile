FROM gitpod/workspace-base

# Install packages we don't care about versions of
RUN sudo apt-get update
RUN sudo apt-get install -y tmux

# Install zsh
RUN sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"

# Install recent neovim
RUN curl -LO https://github.com/neovim/neovim/releases/download/v0.7.0/nvim-linux64.deb && sudo apt-get install ./nvim-linux64.deb

# Install haskell stack
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 bash

RUN bash -c "source ~/.ghcup/env && source ~/.bashrc && ghcup install stack"
RUN bash -c "source ~/.ghcup/env && source ~/.bashrc && ghcup install hls"
RUN bash -c "source ~/.ghcup/env && source ~/.bashrc && cabal update"

# Install project dependencies
COPY natskell.cabal .
COPY stack.yaml .
COPY stack.yaml.lock .
RUN bash -c "source ~/.ghcup/env && source ~/.bashrc && stack build --dependencies-only --system-ghc"

# Set up GPG forwarding requirements

RUN mkdir ~/.gnupg
RUN sudo mkdir /run/user/$(id -u gitpod)
RUN sudo chown gitpod /run/user/$(id -u gitpod)
RUN gpgconf --create-socketdir

USER root
RUN echo "StreamLocalBindUnlink yes" > /etc/ssh/sshd_config
USER gitpod
