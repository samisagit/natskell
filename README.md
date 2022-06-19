# Natskell

Naskell is a client library for [NATS](https://docs.nats.io/) written in haskell

# Project

See roadmap [here](https://github.com/users/samisagit/projects/1)

## Installation

Naskell is still pre alpha, so there is no candidate at present.

## Usage

The API is not set in stone (yet). In the mean time the tests give a decent idea of what is currently possible.

## Contributing
Pull requests are welcome. Please open an issue first to discuss what you would like to change.

Please make sure to add tests.

Please make sure all commits are signed.

#### Working in Gitpod
The development environment is set up for use with [Gitpod](https://gitpod.io/), however there are some steps that should be taken to get started.

Dependencies:
1. SSH client

Local set up:
1. Generate an ECDSA private key (SSH access into Gitpod workspaces relies on a private key existing locally of this type).

```
ssh-keygen -t ECDSA
```

2. Install [Krypton](https://krypt.co/developers/) either on mobile or desktop
3. Enable devloper mode in Krypton
4. Once in an SSH session in Gitpod run `kr pair` and scan the QR code in Krypton (each time you open a new work space)
5. Run `kr me` to output your SSH public key, add this to Github (First time only)
6. Run `kr me pgp` to output your GPG public key, add this to Github (First time only)
7. Start contributing
