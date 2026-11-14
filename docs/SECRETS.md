# Secrets

All secrets are encrypted with [sops](https://github.com/getsops/sops) and age,
managed with [sops-nix](https://github.com/Mic92/sops-nix), and stored in a
separate **private repository**, `nix-secrets`, that this flake reads as its
`secrets` input (`flake = false`).

Every file is encrypted to a single personal age key. Each host keeps a copy of
it in `~/.config/sops/age/keys.txt`, the same file the `sops` CLI uses to edit,
and decrypts its own file of the private repository with it. The encrypted files
stay encrypted when they are fetched and when they are copied into `/nix/store`;
sops-nix decrypts them at activation into `/run/secrets`, a tmpfs, so the plain
values are never on disk.

The module [secrets.nix](../modules/system/core/secrets.nix), part of `core` on
both hosts, configures `sops-nix`. The modules that use a secret declare it next
to the code that reads it (for now only
[restic.nix](../modules/features/restic.nix)).

## Which keys go on a secret

**Every secret is decryptable with the personal key and nothing else** (the
recipients are in `.sops.yaml` of `nix-secrets`). Sops-nix (`sops.age.keyFile`)
and the `sops` CLI (`SOPS_AGE_KEY_FILE`, set on both platforms) read the same
file, so a host that can edit secrets can also decrypt them, and the reverse.

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
    keyFile = "${home}/.config/sops/age/keys.txt";
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
  runs `darwin-rebuild` as root; whether root can fetch the input there is
  **unverified** (see [Limits](#limits)).
- At every activation sops-nix decrypts the host's file with the personal key and
  writes the secrets to `/run/secrets`, which is gone after a reboot and written
  again at the next activation.

### First setup

1. Create the personal key once, store its recovery copy, and put the public
   half (`age1…`) into `.sops.yaml` of `nix-secrets`:

   ```sh
   mkdir -p ~/.config/sops/age
   (umask 077; nix shell nixpkgs#age -c age-keygen -o ~/.config/sops/age/keys.txt)
   nix shell nixpkgs#age -c age-keygen -y ~/.config/sops/age/keys.txt   # public half
   ```

2. Create each host file and fill it in as above. `SOPS_AGE_KEY_FILE` is only
   set after the first switch, so on macOS export it by hand for this step.
3. Commit, create the private GitHub repository `nix-secrets` and push.
4. Copy the key file to the other host, at the same path with mode `600`.
5. Here: `just update secrets sops-nix`, then `just switch` on each host and
   check that `/run/secrets/restic/` holds the three files, owned by the user.

### Installing a host from scratch

The key is not needed for the install itself, only the encrypted files are:

1. `nixos-install` runs as root on the installer, which has no access to the
   private repository. Bring a clone of `nix-secrets` to `/tmp/nix-secrets`
   (copied from another machine, or cloned over HTTPS with a GitHub token; it
   holds only ciphertext), then add
   `--override-input secrets path:/tmp/nix-secrets` to the `nixos-install`
   command in the [README](../README.md#4-install-nixos).
2. Boot the new system and log in. Without the key, decryption at boot fails
   and `/run/secrets` stays empty; the system itself should come up normally
   (not tested). `nixos-install` does not run that step, so the install is not
   affected.
3. Restore the key from its recovery copy to `~/.config/sops/age/keys.txt`
   (mode `600`), then `just switch`. Once the password manager is set up, its
   SSH agent gives the switch access to the private repository.

On macOS it is the same without step 1: install Nix, restore the key, and
`just switch`.

Checked on 2026-11-14 on `alpha`, with the key file renamed:

- The system builds without the key: it is read only at activation.
- `just switch` completes the switch but exits non-zero: the `setupSecrets`
  activation snippet fails (`cannot read keyfile`) and the other snippets run.
  sops-nix stops before writing a new secrets generation, so `/run/secrets.d/`
  still held only generation `1` and `/run/secrets` kept the previous secrets.
- With the key restored, the next `just switch` wrote generation `2` and pruned
  `1`.

The empty `/run/secrets` after a reboot without the key (step 2) has not been
checked.

## Adding a new host

1. In `nix-secrets`, add `<host>` to the `path_regex` of `.sops.yaml`, create
   `<host>.yaml` with `sops`, commit and push.
2. Here: `just update secrets`, add the host to `modules/hosts/` with the
   modules it needs, and install it as above.

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
  that `~/.config/sops/age/keys.txt` exists and that its public half
  (`age-keygen -y`) is among the `age` recipients listed at the bottom of
  `<host>.yaml`. After a reboot on NixOS, also check that `/home` was mounted in
  the initrd (`journalctl -b | grep -i sops`).
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
- **The key is readable by the user.** Any process of the user can read
  `~/.config/sops/age/keys.txt` and decrypt every file of every host. That makes
  the `rootOnly` preset protect only the decrypted file, not the value: before
  the first `rootOnly` secret, give that host a root-only key (see
  [Which keys go on a secret](#which-keys-go-on-a-secret)).
- **The private repository is defence in depth.** The age keys protect the
  values; GitHub access control only hides the ciphertext. Anyone with read
  access to `nix-secrets` and a leaked key can decrypt every past version.
- **Evaluation needs GitHub access.** A fresh install, `just check` and anything
  run without the SSH agent need the repository cloned and passed with
  `--override-input`.
- **macOS is untested.** `darwin-rebuild` runs as root, and sudo drops
  `SSH_AUTH_SOCK`, so root may not be able to fetch the input when it is not
  already in the store. If that happens, run `just update secrets` (or
  `nix flake prefetch`) as the user first, or pass `--override-input` as above.
- **`sops-nix` runs as root** at every activation (see
  [SECURITY.md](SECURITY.md#supply-chain)).
