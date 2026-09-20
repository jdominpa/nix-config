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

## Commits

Conventional Commits: lowercase subject, scope naming the area touched,
identifiers in backticks.

    feat(emacs): add `agent-shell` for llm integration with emacs
    fix(darwin): assert homebrew is enabled in modules before using it
    chore: update `flake.lock`

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
