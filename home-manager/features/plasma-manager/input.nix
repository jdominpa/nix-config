{
  programs.plasma = {
    input = {
      keyboard = {
        repeatDelay = 300;
        layouts = [
          {
            layout = "us";
            displayName = "us";
          }
          {
            layout = "us";
            variant = "intl";
            displayName = "int";
          }
        ];
      };
    };
  };
}
