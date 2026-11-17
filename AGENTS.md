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
  `SANDBOXING.md`, `SECRETS.md`), `BACKUPS.md` and `WORKAROUNDS.md`. See "Documentation"
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
verified. **Every code change that alters behavior the docs describe updates
them in the same commit.** This matters most for the security docs, because a
document that overstates the protection is worse than none.

Check the table below before committing. The code and the docs that describe it
are one commit, typed by the code (`fix(hardening): …`), so that one revert
undoes both and no commit leaves the docs out of step with the configuration.
Do not postpone the docs update to "later".

A check that needs the switched system cannot be in that commit. Describe the
new behavior as "not checked" and add the check to the "To check" list. After
the user has switched and the check has run, record it in a separate
`docs(scope): record …` commit.

| If you change…                                                                | Update                                  |
| ----------------------------------------------------------------------------- | --------------------------------------- |
| `modules/system/core/hardening.nix` (sysctls, kernel parameters, modprobe)    | `docs/SECURITY.md`, kernel baseline     |
| Firewall or any service that opens a port or listens                          | `docs/SECURITY.md`, network exposure    |
| `secure-boot.nix`, `disko.nix`, the boot loader, fwupd, TPM or lockdown       | `docs/SECURITY.md`, architecture / boot |
| `nix.nix`, flake inputs that run with privileges, binary caches               | `docs/SECURITY.md`, Nix and supply chain row |
| `restic.nix`: backed-up paths, schedule, retention, monitoring, credentials   | `docs/BACKUPS.md`, and the summary in `docs/SECURITY.md` |
| `btrfs.nix` (btrbk, scrub)                                                   | `docs/BACKUPS.md`, local snapshots      |
| `secrets.nix`, a `sops.secrets` declaration, the `secrets` or `sops-nix` input | `docs/SECRETS.md`, and the `docs/SECURITY.md` Nix and supply chain row |
| `ssh.nix`, sudo, users and groups, login manager, screen lock                 | `docs/SECURITY.md`, architecture        |
| `modules/system/desktop/nixpak/` (the shared sandbox policy, the Wayland helper) | `docs/SANDBOXING.md`, shared boundaries and Wayland |
| A sandboxed app (`security.nixpak.apps.*`): paths, portals, D-Bus, network, sockets | `docs/SANDBOXING.md`, its table and section, plus limits |
| Sandboxing a new app, or dropping the sandbox of one                          | `docs/SANDBOXING.md` and the app list in `docs/SECURITY.md` |
| Adding or removing a workaround, a pinned input or a sandbox exception        | `docs/WORKAROUNDS.md`: a row with a removal condition, or delete the row |

Rules for the docs:

- Each fact lives in one document: backups in `BACKUPS.md`, secrets in
  `SECRETS.md`, sandboxes in `SANDBOXING.md`, workarounds in `WORKAROUNDS.md`.
  `SECURITY.md` is the overview: it summarises and links, it does not repeat
  their details. Link only where the reader needs to go next.
- Be brief: state the fact and its limit, without background the reader does
  not need.
- Describe what the source configures and what was checked on a running system
  separately. Never write that a control is active or verified unless it was
  checked, and say how and when (date). Mark what is unverified.
- State limits next to the control, not only the benefits: in `SECURITY.md`
  the "Limitation" column of the architecture table.
- When a control is removed or reverted, delete or rewrite its text and record
  why in the relevant "Not sandboxed" section or "Limitation" cell; do not leave
  stale claims.
- Record every check in the "Audits" section at the end of the document it
  concerns. A check still to run is an item of its "To check" list: host, check,
  and how to run it or what is expected. When it is run, move it to the table
  with the date, host, check, how and findings. In the text keep only a short
  marker next to the claim ("checked" or "not checked", linking to Audits), not
  the evidence. A check holds for the configuration it ran against, so rerun it
  after a change that touches it.
- `docs/BACKUPS.md` must never contain secrets (passwords, SSH keys, the monitor
  URL) or say where their recovery copies are kept. The paths the machines read
  them from are already in `restic.nix` and may be mentioned.
- A commit that only changes docs is `docs(scope): …` with the scope naming the
  document or area, for example `docs(security): record the sysctl audit`,
  `docs(sandboxing): record the vesktop audit` or
  `docs(backups): record the restore drill`.

## Commits

Conventional Commits: lowercase subject, scope naming the area touched,
identifiers in backticks.

    feat(emacs): add `agent-shell` for llm integration with emacs
    fix(darwin): assert homebrew is enabled in modules before using it
    chore: update `flake.lock`
    docs(security): record the sysctl audit

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
