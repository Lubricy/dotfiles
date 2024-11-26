{config, ...}: {
  users.users.${config.vars.username}.extraGroups = ["dialout" "input"];
}
