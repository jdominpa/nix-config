# AGENTS.md

Dendritic NixOS / nix-darwin flake. See `README.md` for deployment and for how
to add a new host.

## Layout

Every file under `modules/` is a flake-parts module, auto-imported by
`import-tree`. There is no central list to update when adding a file.

- `modules/features/` — per-program configuration (`emacs`, `niri`).
- `modules/hosts/` — `alpha` (NixOS), `beta` (nix-darwin).
- `modules/system/` — `core` and `desktop` system configuration.
- `modules/flake-parts/` — flake-level wiring (formatting, dev shell).
- `docs/` — design documentation: the security docs (`SECURITY.md`,
  `SANDBOXING.md`), `BACKUPS.md` and `WORKAROUNDS.md`. See "Documentation"
  below.

## Commands

- `just switch <host>` — rebuild and switch; the recipe is platform-gated, so
  the same invocation works on NixOS and darwin.
- `just check` — evaluate the flake and run its tests.
- `just fmt` — format the repo.
- `just update [inputs...]` — update flake inputs.

## Formatting

`treefmt` (nixfmt, deadnix, statix, shfmt) runs from a pre-commit hook, but it
**excludes `*.el` and `*.eld`**. Emacs Lisp is never auto-formatted — match the
style of the surrounding code by hand.

## Documentation

`docs/` describes what the configuration does, why, and what has been
verified. **Every code change that alters behavior the docs describe is
followed by a docs update, in a separate commit.** This matters most for the
security docs, because a document that overstates the protection is worse than
none.

The order is: commit the code change first, then check the table below and
commit the docs update right after it. Never mix documentation and code in one
commit, and do not postpone the docs update to "later". When asked to commit a
change that touches the first column, make both commits.

| If you change…                                                                | Update                                  |
| ----------------------------------------------------------------------------- | --------------------------------------- |
| `modules/system/core/hardening.nix` (sysctls, kernel parameters, modprobe)    | `docs/SECURITY.md`, kernel baseline     |
| Firewall or any service that opens a port or listens                          | `docs/SECURITY.md`, network exposure    |
| `secure-boot.nix`, `disko.nix`, the boot loader, fwupd, TPM or lockdown       | `docs/SECURITY.md`, architecture / boot |
| `nix.nix`, flake inputs that run with privileges, binary caches               | `docs/SECURITY.md`, supply chain        |
| `restic.nix`: backed-up paths, schedule, retention, monitoring, credentials   | `docs/BACKUPS.md`, and the summary in `docs/SECURITY.md` |
| `ssh.nix`, sudo, users and groups, login manager, screen lock                 | `docs/SECURITY.md`, architecture        |
| `modules/system/desktop/nixpak/` (the shared sandbox policy, the Wayland helper) | `docs/SANDBOXING.md`, shared boundaries and Wayland |
| A sandboxed app (`security.nixpak.apps.*`): paths, portals, D-Bus, network, sockets | `docs/SANDBOXING.md`, its table and section, plus limits |
| Sandboxing a new app, or dropping the sandbox of one                          | `docs/SANDBOXING.md` and the app list in `docs/SECURITY.md` |
| Adding or removing a workaround, a pinned input or a sandbox exception        | `docs/WORKAROUNDS.md`: a row with a removal condition, or delete the row |

Rules for the docs:

- Describe what the source configures and what was checked on a running system
  separately. Never write that a control is active or verified unless it was
  checked, and say how and when (date). Mark what is unverified.
- State limits and known gaps next to the control, not only the benefits.
- When a control is removed or reverted, delete or rewrite its text and record
  why in the relevant "Not sandboxed" / "Known gaps" section; do not leave
  stale claims.
- Update the "Validation" section of `docs/SANDBOXING.md` and the drill table
  of `docs/BACKUPS.md` when a check is run or a pending one is done.
- `docs/BACKUPS.md` must never contain secrets (passwords, SSH keys, the monitor
  URL) or say where their recovery copies are kept. The paths the machines read
  them from are already in `restic.nix` and may be mentioned.
- The docs commit is `docs(scope): …` with the scope naming the document or
  area, for example `docs(security): document the new sysctls`,
  `docs(sandboxing): add vesktop screen-share limits` or
  `docs(backups): record the restore drill`.

## Commits

Conventional Commits: lowercase subject, scope naming the area touched,
identifiers in backticks.

    feat(emacs): add `agent-shell` for llm integration with emacs
    fix(darwin): assert homebrew is enabled in modules before using it
    chore: update `flake.lock`
    docs(security): document the new sysctls

## Emacs configuration

Config sources live in `modules/features/emacs/config/`.

- **Changes require a rebuild.** `init.el` is inlined into the wrapper at build
  time and `+core-config-directory` points at a `/nix/store` path, so editing a
  file in this repo has no effect on a running Emacs until `just switch`.
- Naming conventions for Emacs Lisp symbols are documented at the top of
  `modules/features/emacs/config/init.el`. Follow them.
- `init.el` loads each module in `+core-module-list` with `load-file`. Modules
  are **not** byte-compiled, so `declare-function` forms are unnecessary.
- A new module file must be added to `+core-module-list`; it is not discovered
  automatically.
- Packages come from the `emacs-overlay` `epkgs` set in
  `modules/features/emacs/default.nix`. Anything not on ELPA/MELPA is built with
  `trivialBuild` from a flake input — see `math-delimiters` there for the
  pattern.
