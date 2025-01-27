{lib, ...} @ args:
lib.extend (self: super: {
  dot = import ./. args;
})
