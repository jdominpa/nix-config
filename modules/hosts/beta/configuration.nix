{
  inputs,
  ...
}:
{
  flake.modules.darwin.beta = {
    imports = with inputs.self.modules.darwin; [
      jdominpa
    ];
  };
}
