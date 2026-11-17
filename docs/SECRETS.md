# Secrets

All secrets are encrypted with [sops](https://github.com/getsops/sops) and age,
managed with [sops-nix](https://github.com/Mic92/sops-nix), and stored in a
separate **private repository**, `nix-secrets`, that this flake reads as its
`secrets` input (`flake = false`).

Every file is encrypted to a single personal age key. Each host keeps a copy of
it at the path where the `sops` CLI looks for it by default, and decrypts its
own file of the private repository with it:

|Platform|Key file                                         |
|--------|-------------------------------------------------|
|NixOS   |`~/.config/sops/age/keys.txt`                    |
|macOS   |`~/Library/Application Support/sops/age/keys.txt`|

The files stay encrypted when fetched and in the `/nix/store`. sops-nix decrypts
them at every activation into `/run/secrets`, which is in RAM (a tmpfs on NixOS,
a RAM disk on macOS), so the plain values are never on disk. sops-nix also
copies the age key there, as `/run/secrets.d/age-keys.txt`, readable by root
only.

The module [secrets.nix](../modules/system/core/secrets.nix), part of `core` on
both hosts, configures `sops-nix`. The modules that use a secret declare it next
to the code that reads it (for now only
[restic.nix](../modules/features/restic.nix)).

## Which keys go on a secret

**Every file is encrypted to the personal key only** (recipients in `.sops.yaml`
of `nix-secrets`). sops-nix and the `sops` CLI read the same key file.

- **One key, not one per host:** the key is on every machine where secrets are
  edited, readable by any process of the user, so per-host keys would add steps
  without narrowing anything. The tradeoff is that any process of the user can
  decrypt every file.
- **Per-host keys become worth it** for a host where secrets are not edited (a
  server, a CI runner), for secrets some hosts must not read, or before the
  first `rootOnly` secret, which otherwise protects only the decrypted file.
  Per host: create a root-only key in `/var/lib/sops-nix/key.txt`, add it as a
  recipient of the host's file, `sops updatekeys` it, and point
  `sops.age.keyFile` at it.
- **The private repository only hides the ciphertext.** With read access to it
  and a leaked key, every past version can be decrypted.
- No SSH keys: sshd is off (`sops.age.sshKeyPaths = [ ]`), and an SSH agent only
  signs, it cannot decrypt.

## Decrypted file permissions

sops-nix writes each value to `/run/secrets/<name>`. Every secret uses one of
two presets from `config.secretPresets`, defined in
[secrets.nix](../modules/system/core/secrets.nix):

|Preset        |Mode / owner   |Use for                                          |
|--------------|---------------|-------------------------------------------------|
|`rootOnly`    |`0400` `root`  |root-only consumers (system services, activation)|
|`userReadable`|`0400` `<user>`|services that run as the user, and the user      |

A module applies a preset to its secret instead of setting `owner` and `mode`
itself:

```nix
sops.secrets."<name>" = config.secretPresets.userReadable;
```

The group keeps the sops-nix default (the owner's primary group on NixOS,
`staff` on macOS), and with mode `0400` it gets no access. A `userReadable`
secret is readable by any process of the user.

## Adding or updating secrets

The host files hold nested keys, so a secret's name is the key path joined with
`/`:

```yaml
example:
  password: 1234
  ssh-key: |
    -----BEGIN OPENSSH PRIVATE KEY-----
    ...
    -----END OPENSSH PRIVATE KEY-----
```

Edit a host's secrets file with the `sops` CLI inside `nix-secrets`
(`.sops.yaml` is looked up from the current directory). For a new host file the
creation rule for its name picks the recipients:

```sh
cd ~/dev/nix-secrets
nix run nixpkgs#sops <host>.yaml
git commit -am "..." && git push
```

Then, in this repository, declare the secret in the module that needs it
(`sops.secrets."<name>"`, set to one of the
[presets](#decrypted-file-permissions)) and update `flake.lock`:

```sh
just update secrets
```

`flake.lock` pins `secrets`, so a new or changed value is invisible here until
the lock moves. An edit therefore takes two commits, one in `nix-secrets` and a
`flake.lock` bump here, then a switch on every host that reads the file.

## Deploying secrets

`modules/system/core/secrets.nix` imports the sops-nix module of each platform
and points it at the host's file and key. On NixOS it also sets
`fileSystems."/home".neededForBoot = true`, since secrets are decrypted at boot
before the other file systems are mounted, and the key is on `/home`.

Evaluation fetches `nix-secrets` over SSH. On NixOS, `just switch` evaluates as
the user (`nixos-rebuild --sudo`); on macOS, `darwin-rebuild` runs as root and
fetches it too (checked, [Audits](#audits)).

## Adding a new host

Installing the system itself, including how the installer gets the private
repository, is in the [README.md](../README.md#deployment).

1. **Add the personal key** from its recovery copy to the key file of the
   platform:

   ```sh
   # NixOS
   install -d -m 700 ~/.config/sops/age
   # silent prompt, never world-readable
   read -rs key && (umask 077; printf '%s\n' "$key" > ~/.config/sops/age/keys.txt); unset key

   # macOS
   install -d -m 700 "$HOME/Library/Application Support/sops/age"
   # silent prompt, never world-readable
   read -rs key && (umask 077; printf '%s\n' "$key" > "$HOME/Library/Application Support/sops/age/keys.txt"); unset key
   ```

   Check that the mode is `600` with `stat -c %a`.

2. **Add the host's file** in `nix-secrets`: add `<host>` to the `path_regex` of
   `.sops.yaml`, create `<host>.yaml` with `sops` (see [Adding or updating
   secrets](#adding-or-updating-secrets)), commit and push. Any machine with the
   key can do this, also before the host is installed.

3. **Update the input and switch**, here:

   ```sh
   just update secrets
   just switch <host>
   ```

Until the key is in place the host builds and boots without secrets:
there is no `/run/secrets` and decryption fails at activation (see
[Audits](#audits)).

## A host is lost or compromised

The host held the personal key, so assume every file is exposed, not only the
host's own:

1. Create a new personal key, replace the recipient in `.sops.yaml`, re-encrypt
   every file with `sops updatekeys`, commit and push, and copy the new key to
   the hosts you keep. Update its recovery copy.
2. Delete the lost host's file and its name from `.sops.yaml`. This stops
   nothing on its own: the old ciphertext is in the git history, and the old key
   can decrypt it.
3. Rotate every secret in the repository, starting with the lost host's.

## Troubleshooting

- **The switch fails fetching `secrets`:** check `ssh -T git@github.com`, or
  pass `--override-input secrets path:<clone>`.
- **Decryption fails during activation:** read the output of the switch. Check
  that the key file of the platform exists and that its public half (`age-keygen
  -y`) is among the `age` recipients listed at the bottom of `<host>.yaml`.
  After a reboot on NixOS, also check that `/home` was mounted in the initrd
  (`journalctl -b | grep -i sops`).
- **macOS, after a boot:** the `sops-install-secrets` daemon's output is
  discarded; `launchctl print system/org.nixos.sops-install-secrets` shows its
  `last exit code`.
- **A secret is missing in `/run/secrets`:** it is not declared in a module, or
  the switch has not run since the lock was bumped. `ls -l /run/secrets/…` shows
  the owner and mode.

## Audits

Each check holds for the configuration it ran against: rerun it after a change
to `secrets.nix`, the `sops-nix` input or the key handling.

|Date      |Host |Check                                              |How                                                                                                                 |Findings                                                                                                                                                                                                                    |
|----------|-----|---------------------------------------------------|--------------------------------------------------------------------------------------------------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|2026-11-14|alpha|Build without the key                              |Key file renamed, then built                                                                                        |Builds: the key is read only at activation                                                                                                                                                                                  |
|2026-11-14|alpha|Switch without the key                             |Key file renamed, `just switch`                                                                                     |Completes but exits non-zero: `setupSecrets` fails with `cannot read keyfile`, the other snippets run, no new generation is written and `/run/secrets` keeps generation `1`                                                 |
|2026-11-14|alpha|Switch after restoring the key                     |`just switch`                                                                                                       |Writes generation `2` and prunes `1`                                                                                                                                                                                        |
|2026-11-14|beta |Key only at the macOS path                         |`darwin-rebuild switch`, then `ls -l /run/secrets`                                                                  |Completes; `/run/secrets` points to `/run/secrets.d/2`, owned by root; no `~/.config/sops/age/keys.txt` exists                                                                                                              |
|2026-11-14|beta |Root fetches `nix-secrets`                         |`sudo ssh -T git@github.com`; `sudo nix flake prefetch --refresh` on the input URL, which bypasses the fetcher cache|Authenticates as `jdominpa` and fetches the input into the store                                                                                                                                                            |
|2026-11-14|beta |Metadata of the restic secrets                     |`stat` on `/run/secrets/restic/*`, without reading the values                                                       |Owner `jdominpa`, mode `400`, not empty: the `userReadable` preset                                                                                                                                                          |
|2026-11-14|beta |Switch without the key, then with it               |Key file renamed, `just switch`, key restored, `just switch`                                                        |As on `alpha`: the previous generation is kept, then a new one is written and the old one pruned                                                                                                                            |
|2026-11-14|beta |Key-restore commands of Adding a new host          |Run in zsh against a temporary `HOME` with a dummy key                                                              |`sops/age` created with mode `700`, `keys.txt` with `600`                                                                                                                                                                   |
|2026-11-14|beta |Reboot without the key                             |Key file renamed, reboot                                                                                            |No `/run/secrets`; `/run/secrets.d` remounted but empty; the system comes up normally                                                                                                                                       |
|2026-11-14|beta |`/run/secrets.d` is a RAM disk                     |`hdiutil info`; `sysctl vm.swapusage`                                                                               |`image-path : ram://131072` (64 MiB) and no shadow file; swap is `(encrypted)`                                                                                                                                              |
|2026-11-17|beta |Secret missing from the host's file                |`nix build` of the system with an extra `sops.secrets."audit/missing"` (`extendModules`), nothing activated         |The build fails in the manifest check: `the key 'audit' cannot be found`                                                                                                                                                    |
|2026-11-17|beta |Output of the `sops-install-secrets` daemon at boot|`/Library/LaunchDaemons/org.nixos.sops-install-secrets.plist`; `launchctl print`; `log show --last boot`            |No `StandardOutPath` or `StandardErrorPath`, so the output is discarded and not in the unified log. `launchctl print` shows `runs = 1`, `last exit code = 1` for the 2026-11-14 19:08 boot, the reboot without the key above|
|2026-11-17|beta |Copy of the age key in `/run/secrets.d`            |`ls -l /run/secrets.d`; the sops-nix source (`main.go`)                                                             |`age-keys.txt`, owner `root`, mode `600`; sops-install-secrets copies the key file there on every run                                                                                                                       |
|2026-11-17|alpha|Secret missing from the host's file                |`nix build` of the system with an extra declared secret                                                             |The build fails, as on `beta`                                                                                                                                                                                               |
|2026-11-17|alpha|Reboot without the key                             |Key file renamed to `keys.txt.off`, reboot                                                                          |No `/run/secrets`; the system boots normally                                                                                                                                                                                |

