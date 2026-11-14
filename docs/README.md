# Documentation

Canonical documentation index for this repository. Standalone reference material
lives in this directory. Component runbooks stay next to the code they describe
so they remain visible during maintenance, and every human-facing document is
linked here.

`README.md` and `AGENTS.md` remain at the repository root because GitHub and
coding agents require those entry points.

## Operations and policy

- [Security](SECURITY.md): threat model, trust boundaries, kernel and boot
  hardening, network exposure, known gaps and how to check the live system.
- [Sandboxing](SANDBOXING.md): the nixpak application sandboxes, what each one
  is allowed to reach, the limits of the approach and how to add or inspect a
  sandbox.
- [Backups](BACKUPS.md): what is backed up, how to check it, how to restore, and
  the restore drill log.
- [Workarounds](WORKAROUNDS.md): temporary workarounds, pins and sandbox
  exceptions, each with the condition for removing it.

## Configuration structure

- [Dendritic modules](../modules/README.md).
