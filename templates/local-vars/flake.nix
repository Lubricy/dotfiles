{
  description = "A very basic flake";

  outputs = { self, ... }: {
    default = {
      username = "<username>";
      hostname = "<hostname>";

      home-modules = [];
      darwin-modules = [];

    }
  };
}
