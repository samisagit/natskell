FROM ubuntu

SHELL ["/bin/bash", "-c"]
RUN useradd -ms /bin/bash -u 33333 gitpod

# Install packages we don't care about versions of
RUN apt-get update
RUN apt-get install -y tmux curl zsh git build-essential curl libffi-dev libffi8ubuntu1 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5

# Install ohmyzsh
RUN sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"

# Install recent neovim
RUN curl -LO https://github.com/neovim/neovim/releases/download/v0.7.0/nvim-linux64.deb && apt-get install ./nvim-linux64.deb

# create root directories that stack uses
RUN mkdir /.stack-work
RUN chown gitpod /.stack-work

USER gitpod
# Install haskell stack
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 bash

RUN source ~/.ghcup/env && source ~/.bashrc && ghcup install stack
RUN source ~/.ghcup/env && source ~/.bashrc && ghcup install hls
RUN source ~/.ghcup/env && source ~/.bashrc && cabal update

# Install project dependencies
COPY natskell.cabal .
COPY stack.yaml .
COPY stack.yaml.lock .
RUN bash -c "source ~/.ghcup/env && source ~/.bashrc && stack build --dependencies-only --system-ghc"

# Set up GPG forwarding requirements
USER root
RUN echo "StreamLocalBindUnlink yes" >> /etc/ssh/sshd_config
RUN echo "AllowTcpForwarding yes" >> /etc/ssh/sshd_config
RUN echo "AllowStreamLocalForwarding yes" >> /etc/ssh/sshd_config
RUN echo "LogLevel DEBUG3" >> /etc/ssh/sshd_config
RUN echo "DenyUsers gitpod" >> /etc/ssh/sshd_config
RUN echo "Here be a banner" >> /etc/motd

USER gitpod
RUN mkdir ~/.gnupg

