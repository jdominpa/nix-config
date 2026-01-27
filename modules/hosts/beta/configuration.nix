{
  self,
  ...
}:
{
  flake.modules.darwin.beta = {
    imports =
      with self.modules.darwin;
      [
        cli-tools
        emacs
        jdominpa
        nix
        shell
      ]
      ++ [
        {
          home-manager.users.jdominpa = {
            imports = with self.modules.homeManager; [
              emacs
            ];
          };
        }
      ];
  };
}
