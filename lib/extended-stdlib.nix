{
  lib,
  inputs,
}:
lib.extend (self: super: {
  dot = import ./. {
    inherit inputs;
    lib = self;
  };
})
