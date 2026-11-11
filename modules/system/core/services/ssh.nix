{
  # Remote logins are off on both hosts. To turn sshd back on for a host, enable
  # `services.openssh` for that host and add a public key under
  # `users.users.<name>.openssh.authorizedKeys.keys`: the `hardening.nix` module
  # only allows key-based logins.
  flake.modules.nixos.ssh = {
    services.openssh.enable = false;
  };

  flake.modules.darwin.ssh = {
    # Must be an explicit `false`: nix-darwin's default of `null` leaves remote
    # login in whatever state it is in, which is currently "on".
    services.openssh.enable = false;
  };
}
