{
  description = "Flake for local-vars";

  outputs = { self, ... }: {
    default = {
      username = "<username>";
      hostname = "<hostname>";

      home-modules = [];
      darwin-modules = [];

    };
  };
}
