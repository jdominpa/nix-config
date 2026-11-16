# Backups

Off-site, encrypted, versioned backups of the data Syncthing keeps in sync. Each
host runs a daily backup into **the same repository** on a sub-account of a
Hetzner Storage Box. On `alpha`, btrbk also keeps local snapshots of `/home`
([Local snapshots](#local-snapshots-alpha)).

## What protects against what

|Threat                                      |Defence                                                                                                                   |
|--------------------------------------------|--------------------------------------------------------------------------------------------------------------------------|
|Accidental deletion, bad edit               |btrbk snapshots of `/home` on `alpha` (hourly, up to 4 weeks); restic snapshots (7 daily, 5 weekly, 12 monthly per host)  |
|Disk or host loss                           |restic: encrypted copies on the Storage Box. A failed system disk on `alpha` also leaves the btrbk copies on the data disk|
|A compromised Storage Box                   |Client-side encryption: the box only holds ciphertext                                                                     |
|A compromised device deleting the repository|Automatic Storage Box snapshots of the whole box, managed only in the Hetzner Console, not over SSH                       |
|A backup that fails or silently stops       |healthchecks.io alerts when a success ping is missing; `restic check` re-reads data on every run                          |

## Components

|Item                 |Value                                                                                                       |
|---------------------|------------------------------------------------------------------------------------------------------------|
|Repository           |`repository` in [restic.nix](../modules/features/restic.nix); the path is relative to the sub-account's home|
|Backed up            |`~/Documents`, `~/Pictures`, `~/GrapheneOS` (excluding `.stversions`)                                       |
|`alpha`              |systemd timer; daily 00:00: backup, `forget --prune`, `check --read-data-subset 500M`                       |
|`beta`               |launchd agent; daily 12:00: backup only                                                                     |
|Restic keys          |One per host, all unlocking the same master key; snapshots record the host name (`--host` filters)          |
|Storage Box snapshots|Daily, 10 kept, so damage must be noticed within **10 days** to be undone                                   |
|Runs as              |The user on both hosts; no `sudo`                                                                           |

- **Only `alpha` prunes.** Pruning needs the exclusive repository lock, so the
  Mac never runs `forget`. Retention groups by host, so each machine keeps its
  own 7/5/12 and a change of backed-up paths does not strand old snapshots.
- **Monitoring** is one healthchecks.io check per host (period 1 day, grace long
  enough for the machine being off or offline). `alpha` reports success only
  when backup, prune and check all pass. `beta` reports success only on exit
  status 0; status 3 (some files unreadable, e.g. a macOS TCC denial) counts as
  a failure. Without the ping URL secret the ping unit fails.
- **`restic-storagebox`** is a wrapper that supplies the repository, password
  file and SSH key, so plain restic arguments can be appended. It exists on both
  hosts.
- **A Storage Box snapshot covers the whole box**, so a rollback also restores
  the repository's `keys/` and the sub-account's `authorized_keys`, undoing any
  revocation done since. Hence the order in the compromise procedure below.

## Local snapshots (`alpha`)

[btrfs.nix](../modules/hosts/alpha/btrfs.nix) runs btrbk hourly, as root:

- **Snapshots:** read-only snapshots of `/home` in `/.snapshots`, on the system
  disk. All are kept for 6 hours, then hourly for 48 hours, daily for 7 days and
  weekly for 4 weeks.
- **Copies:** the first snapshot of each day is sent incrementally to
  `/data/backup` on the data disk and kept daily for 14 days, weekly for 8 weeks
  and monthly for 6 months.
- **Limits:** Without `/data/backup` mounted the run fails, local snapshots
  included. Nothing alerts on a failed run (`systemctl status btrbk-home`).

To restore, copy from `/.snapshots/home.<timestamp>/<user>/` or
`/data/backup/home.<timestamp>/<user>/`.

## Security model

1. **Reach (SSH keys):** who can connect to the sub-account. Cut at once by
   turning off the sub-account's SSH access in the Console; for good by
   *replacing* (never editing) its `authorized_keys`, because a device key has
   full SFTP access to the sub-account's home.
2. **Read (restic passwords):** who can open the repository. Removing a key stops
   that password working, but all keys unlock the same master key, which restic
   cannot rotate: whoever held a password and a copy of the repository can
   decrypt that copy for good. Layer 1 is what keeps a removed device away from
   later data.
3. **Undo (Storage Box snapshots):** even if a device with valid keys deletes
   the repository, which anything running as the user can do, the Storage Box
   snapshots can be used to recover the data. Reaches back only as far as the
   10-day window.

## Credentials

Each host's restic password, backup SSH key (no passphrase) and monitor ping
URL are sops secrets under `restic` ([SECRETS.md](SECRETS.md)). A lost backup
key is replaced, not recovered; a ping URL is copied again from the monitor's
website.

## Procedures

### Adding a host

Credentials first, configuration last, otherwise the service fails until the key
and password are in place.

1. Create the backup key, without a passphrase:
   `ssh-keygen -t ed25519 -f ~/.ssh/storagebox -N "" -C "restic <hostname>"`.
2. Install it on the sub-account (SSH access must be enabled in the Console):
   `ssh-copy-id -s -p 23 -i ~/.ssh/storagebox <sub-account>@<sub-account>.your-storagebox.de`.
3. From a host that already works, add a restic key:
   `restic-storagebox key add --host <hostname> --user <user>`.
4. Put the private backup key, the password and a new monitor URL in the host's
   secrets ([SECRETS.md](SECRETS.md#adding-or-updating-secrets)), then delete
   `~/.ssh/storagebox` and its `.pub`.
5. Add `restic` to the host's imports in `modules/hosts/<host>` and
   `just switch <host>`. This also pins the Storage Box host key.
6. Run it by hand and confirm the snapshot and the success ping:

   ```sh
   # NixOS
   sudo systemctl start --no-block restic-backups-storagebox.service
   journalctl -fu restic-backups-storagebox

   # macOS
   launchctl kickstart gui/$(id -u)/org.nixos.restic-backups-storagebox
   tail -f ~/Library/Logs/restic-backups-storagebox.log

   # both
   restic-storagebox snapshots --host <hostname>
   ```

Notes: a second NixOS host needs `pruneOpts` removed (two pruners contend for
the lock). Until the service has run once, `restic-storagebox` on NixOS needs
`--no-cache`. On macOS no Full Disk Access grant has been needed so far
([Audits](#audits)), but it may be tied to the Nix store path of `restic`: after
a restic update check the log for "operation not permitted" (exit status 3).

### Verifying

```sh
restic-storagebox snapshots --latest 1          # newest snapshot per host
restic-storagebox check                         # structure only
restic-storagebox check --read-data-subset 5%   # also re-read data, slow
```

Last run: `journalctl -u restic-backups-storagebox` on `alpha`,
`~/Library/Logs/restic-backups-storagebox.log` on `beta`.

### Test restore

Every few months and after configuration changes, restore a folder and compare:

```sh
t=$(mktemp -d)
restic-storagebox restore "latest:$HOME/Documents/<folder>" \
  --host <hostname> --target "$t"
diff -r "$t" ~/Documents/<folder> && echo identical; rm -rf "$t"
```

Only files changed since the last backup should differ. Record each run in
[Audits](#audits).

### A host is stolen or compromised

Cut reach, roll back if needed, and only then revoke: a rollback restores
removed keys. Steps 2–4 run from a machine you keep, as the *main account*
(which sees the sub-account's home as `restic/`):

```sh
main=sftp://<main-account>@<main-account>.your-storagebox.de:23/restic/restic
```

1. **Cut reach:** turn off the sub-account's SSH access.
2. **Inspect, roll back if needed:** `restic-storagebox -r "$main" snapshots`
   and `... check`. If the repository was damaged or deleted, or you cannot
   tell, roll the box back in the Console to the newest snapshot before the loss
   and run both again.
3. **Replace `restic/.ssh/authorized_keys`** with a file holding only the public
   keys of the hosts you keep (`sftp -P 23 <main-account>@…`, then
   `put <pubkey> restic/.ssh/authorized_keys`).
4. **Remove the lost host's restic key:**
   `restic-storagebox -r "$main" key list`, then `key remove <key id>`. Restic
   will not remove the key you are authenticated with, hence the other machine.
5. **Restore reach:** turn SSH access back on, run a backup by hand and confirm.
6. Set up the replacement (above) and restore.

### Restoring

Pass the **old** host's name to `--host`. The path after `latest:` is the old
home directory (`/home/<user>` on NixOS, `/Users/<user>` on macOS); restic
restores its contents into the target, so it works across operating systems.

```sh
# NixOS replacement: straight into the new home
restic-storagebox restore "latest:<old home>" --host <old host> --target ~

# macOS replacement: stage first, then move files in by hand
restic-storagebox restore "latest:<old home>" --host <old host> --target ~/restore
```

With no host left that has `restic-storagebox`, use any machine with `restic`
and `ssh`: create and install a backup key as in "Adding a host" (steps 1–2),
then

```sh
restic -o sftp.args="-i $HOME/.ssh/storagebox" \
  -r <repository URL from restic.nix> \
  restore "latest:<old home>" --host <old host> --target ~/restore
```

It asks for a restic password. On a fresh machine, compare the Storage Box host
key it offers with the one pinned in `restic.nix` before accepting it.

## Audits

Checks and restore drills. Each holds for the configuration it ran against:
rerun it after a change to `restic.nix` or `btrfs.nix`.

To check:

- both: restore drill ([Test restore](#test-restore)).

|Date      |Host |Check                                                 |How                                                                             |Findings                                                                                     |
|----------|-----|------------------------------------------------------|--------------------------------------------------------------------------------|---------------------------------------------------------------------------------------------|
|2026-11-09|beta |The agent reads `~/Documents` without Full Disk Access|First scheduled runs; `restic-storagebox ls latest --host beta`; the agent's log|397 entries under `~/Documents` in the snapshot and no permission errors, with no grant given|
|2026-11-15|alpha|btrbk snapshot and copy                               |`systemctl start btrbk-home`, then the timer run; `journalctl -u btrbk-home`    |checked                                                                                      |
