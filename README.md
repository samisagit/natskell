# Natskell

Natskell is a client library for [NATS](https://docs.nats.io/) written in haskell

# Project

See roadmap [here](https://github.com/users/samisagit/projects/1)

## Installation

Natskell is still pre alpha, so there is no candidate at present.

## Usage

The API is not set in stone (yet). In the mean time the tests give a decent idea of what is currently possible.

## Contributing
Pull requests are welcome. Please open an issue first to discuss what you would like to change.

Please make sure to add tests.

Please make sure all commits are signed.

#### Working in Gitpod
The development environment is set up for use with Gitpod, however there are some steps that should be taken to get started.

Dependencies:
1. GPG
2. SSH client

Local set up:

1. Add an SSH private key to Gitpods env vars under the name `PRIVATE_KEY` and add the public key to your github profile. (As soon as another option is available we should change this, but for now it works).

```
ssh-keygen
```
2. Generate an ECDSA private key (SSH access into Gitpod workspaces relies on a private key existing locally of this type).

```
ssh-keygen -t ECDSA
```
3. Update `~/.gnupg/gpg-agent.conf` with

```
extra-socket ~/.gnupg/S.gpg-agent.extra
```
4. Reload your gpg agent with `gpg-connect-agent reloadagent /bye`
5. Add/update `~/.ssh/config` with

```
Host *.gitpod.io
RemoteForward /home/gitpod/.gnupg/S.gpg-agent ~/.gnupg/S.gpg-agent.extra
```
