# Documentation

Canonical documentation index for this repository. Standalone reference material
lives in this directory. Component runbooks stay next to the code they describe
so they remain visible during maintenance, and every human-facing document is
linked here.

`README.md` and `AGENTS.md` remain at the repository root because GitHub and
coding agents require those entry points.

## Operations and policy

- [Security](SECURITY.md): threat model, trust boundaries, kernel and boot
  hardening, network exposure, known gaps.
- [Sandboxing](SANDBOXING.md): the nixpak sandboxes, what each can reach, their
  limits and how to add one.
- [Backups](BACKUPS.md): what is backed up, how to check it and how to restore.
- [Secrets](SECRETS.md): sops-nix, the private secrets repository, the age key,
  editing secrets and adding a host.
- [Workarounds](WORKAROUNDS.md): temporary workarounds, pins and sandbox
  exceptions, with their removal conditions.

## Configuration structure

- [Dendritic modules](../modules/README.md).
