{
  self,
  ...
}:
{
  flake.modules.nixos.proton = {
    imports = [ self.modules.nixos.proton-vpn ];
  };

  flake.modules.darwin.proton = {
    imports = with self.modules.darwin; [
      proton-drive
      proton-vpn
    ];
  };
}
