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

The encrypted files stay encrypted when they are fetched and when they are
copied into `/nix/store`; sops-nix decrypts them at activation into
`/run/secrets`, a tmpfs, so the plain values are never on disk.

The module [secrets.nix](../modules/system/core/secrets.nix), part of `core` on
both hosts, configures `sops-nix`. The modules that use a secret declare it next
to the code that reads it (for now only
[restic.nix](../modules/features/restic.nix)).

## Which keys go on a secret

**Every secret is decryptable with the personal key and nothing else** (the
recipients are in `.sops.yaml` of `nix-secrets`). Sops-nix (`sops.age.keyFile`)
and the `sops` CLI (its default path, no `SOPS_AGE_KEY_FILE` needed) read the
same file, so a host that can edit secrets can also decrypt them, and the
reverse.

- **Why one key and not one per host:** the key has to be on every machine where
  secrets are edited, and there any process of the user can read it. With
  per-host keys a compromised user session could still decrypt every file, so
  they would add steps without narrowing anything while all hosts are edited on.
- **When per-host keys become worth it:** a host where secrets are not edited
  (a server, a VPS, a CI runner), secrets some hosts must not read, or the first
  secret that uses the `rootOnly` preset (see
  [Limits](#limits)). The migration is per host: create a root-only key in
  `/var/lib/sops-nix/key.txt`, add it as a recipient of that host's file,
  `sops updatekeys` the file, and point the host's `sops.age.keyFile` at it.
- The SSH host keys are **not** used (`sops.age.sshKeyPaths = [ ]`): sshd is off
  on both hosts. The SSH agent cannot stand in for the key either, because the
  agent only signs and never decrypts.

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

The group is not part of a preset and keeps the sops-nix default: the owner's
primary group on NixOS (`users` for the user, `root` for root) and `staff` on
macOS. With mode `0400` the group gets no access either way.

Every host that imports a module declares its secrets, so the host's file has to
contain every key the module declares. A missing key should make activation fail
rather than leave the secret out (not tested).

Decrypted secrets are readable by their owner. See [Limits](#limits).

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

Edit with the `sops` CLI from inside `nix-secrets`, because `.sops.yaml` is
looked up from the current directory. It opens an editor and encrypts on save;
for a new file the creation rule for its name picks the recipients:

```sh
cd ~/dev/nix-secrets
nix shell nixpkgs#sops -c sops <host>.yaml
git commit -am "..." && git push
```

Then, in this repository, declare the secret in the module that reads it
(`sops.secrets."<name>"`, set to one of the
[presets](#decrypted-file-permissions)) and move the lock:

```sh
just update secrets
```

`flake.lock` pins `secrets`, so a new or changed value is invisible here until
the lock moves. An edit therefore takes two commits, one in `nix-secrets` and a
`flake.lock` bump here, then a switch on every host that reads the file.

## Deploying secrets

`modules/system/core/secrets.nix` imports the sops-nix module of each platform
and points it at the host's file and key:

```nix
sops = {
  defaultSopsFile = "${inputs.secrets}/${config.networking.hostName}.yaml";
  age = {
    inherit keyFile; # the key file of the platform, see the table above
    sshKeyPaths = [ ]; # only the key above
  };
  gnupg.sshKeyPaths = [ ];
};
```

On NixOS it also sets `fileSystems."/home".neededForBoot = true`. At boot the
secrets are decrypted by the activation script, which runs before the other file
systems are mounted, and `/home` is a separate subvolume in the disko layout;
with this it is mounted in the initrd, from the same unlocked LUKS volume.

- Evaluation fetches `nix-secrets` over SSH, so it needs access to the private
  repository. On NixOS `just switch` runs `nixos-rebuild --sudo`, which
  evaluates as the user and uses sudo only to activate. On macOS `just switch`
  runs `darwin-rebuild` as root, and root fetches the input through SSH
  (checked, see [Audits](#audits)).
- At every activation sops-nix decrypts the host's file with the personal key and
  writes the secrets to `/run/secrets`, which is gone after a reboot and written
  again at the next activation.

## Adding a new host

Installing the system itself, including how the installer gets the private
repository, is in the [README](../README.md#deployment).

1. **Restore the personal key** from its recovery copy to the key file of the
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
`/run/secrets` stays empty and decryption fails at activation (see
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

- **The switch fails fetching `secrets`:** the SSH agent has no GitHub key or
  the repository does not exist yet. Run `ssh -T git@github.com` as the user,
  or pass `--override-input secrets path:<clone>`.
- **Decryption fails during activation:** read the output of the switch. Check
  that the key file of the platform exists and that its public half (`age-keygen
  -y`) is among the `age` recipients listed at the bottom of `<host>.yaml`.
  After a reboot on NixOS, also check that `/home` was mounted in the initrd
  (`journalctl -b | grep -i sops`).
- **A secret is missing in `/run/secrets`:** it is not declared in a module, or
  the switch has not run since the lock was bumped. `ls -l /run/secrets/…` shows
  the owner and mode.
- **macOS:** besides the activation script, a launchd daemon
  (`sops-install-secrets`) writes the secrets again at boot. Where its output
  goes has not been checked.

## Limits

- **Decrypted secrets are readable by their owner.** The restic secrets belong
  to the user, so any process of the user can still read them, exactly as with
  the plain files they replace. sops-nix keeps them off the disk and out of the
  Nix store; it does not isolate them from the user.
- **The key is readable by the user.** Any process of the user can read the key
  file and decrypt every file of every host. That makes the `rootOnly` preset
  protect only the decrypted file, not the value: before the first `rootOnly`
  secret, give that host a root-only key (see [Which keys go on a
  secret](#which-keys-go-on-a-secret)).
- **The private repository is defence in depth.** The age keys protect the
  values; GitHub access control only hides the ciphertext. Anyone with read
  access to `nix-secrets` and a leaked key can decrypt every past version.
- **Evaluation needs GitHub access.** A fresh install, `just check` and anything
  run without the SSH agent need the repository cloned and passed with
  `--override-input`. On macOS this includes `just switch` whenever the input is
  not already in the store: root fetches it through SSH.
- **`sops-nix` runs as root** at every activation (see
  [SECURITY.md](SECURITY.md#supply-chain)).

## Audits

Checks run on the hosts. Each holds for the configuration it ran against: rerun
it after a change to `secrets.nix`, the `sops-nix` input or the key handling. A
`pending` check has not been run yet and has no date.

|Date      |Host |Check                                              |How                                                                                                                 |Status |Findings                                                                                                                                                                   |
|----------|-----|---------------------------------------------------|--------------------------------------------------------------------------------------------------------------------|-------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|2026-11-14|alpha|Build without the key                              |Key file renamed, then built                                                                                        |checked|Builds: the key is read only at activation                                                                                                                                 |
|2026-11-14|alpha|Switch without the key                             |Key file renamed, `just switch`                                                                                     |checked|Completes but exits non-zero: `setupSecrets` fails with `cannot read keyfile`, the other snippets run, no new generation is written and `/run/secrets` keeps generation `1`|
|2026-11-14|alpha|Switch after restoring the key                     |`just switch`                                                                                                       |checked|Writes generation `2` and prunes `1`                                                                                                                                       |
|2026-11-14|beta |Key only at the macOS path                         |`darwin-rebuild switch`, then `ls -l /run/secrets`                                                                  |checked|Completes; `/run/secrets` points to `/run/secrets.d/2`, owned by root; no `~/.config/sops/age/keys.txt` exists                                                             |
|2026-11-14|beta |Root fetches `nix-secrets`                         |`sudo ssh -T git@github.com`; `sudo nix flake prefetch --refresh` on the input URL, which bypasses the fetcher cache|checked|Authenticates as `jdominpa` and fetches the input into the store                                                                                                           |
|2026-11-14|beta |Metadata of the restic secrets                     |`stat` on `/run/secrets/restic/*`, without reading the values                                                       |checked|Owner `jdominpa`, mode `400`, not empty: the `userReadable` preset                                                                                                         |
|2026-11-14|beta |Switch without the key, then with it               |Key file renamed, `just switch`, key restored, `just switch`                                                        |checked|As on `alpha`: the previous generation is kept, then a new one is written and the old one pruned                                                                           |
|2026-11-14|beta |Key-restore commands of Adding a new host          |Run in zsh against a temporary `HOME` with a dummy key                                                              |checked|`sops/age` created with mode `700`, `keys.txt` with `600`                                                                                                                  |
|2026-11-14|beta |Reboot without the key                             |Key file renamed, reboot                                                                                            |checked|No `/run/secrets`; `/run/secrets.d` remounted but empty; the system comes up normally                                                                                      |
|2026-11-14|beta |`/run/secrets.d` is a RAM disk                     |`hdiutil info`; `sysctl vm.swapusage`                                                                               |checked|`image-path : ram://131072` (64 MiB) and no shadow file; swap is `(encrypted)`                                                                                             |
|—         |alpha|Boot without the key                               |—                                                                                                                   |pending|Expected: empty `/run/secrets`, normal boot                                                                                                                                |
|—         |both |Key missing from the host's file                   |—                                                                                                                   |pending|Expected: activation fails rather than leaving the secret out                                                                                                              |
|—         |beta |Output of the `sops-install-secrets` daemon at boot|—                                                                                                                   |pending|—                                                                                                                                                                          |
|—         |beta |Install from scratch                               |—                                                                                                                   |pending|—                                                                                                                                                                          |
