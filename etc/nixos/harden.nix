{ config, pkgs, lib, ... }:

{
  # TODO: finish the job cjb!
  # harden systemd services. See: systemd-analyze security
  # start by disabling any service and deps that we don't need:
  system.nssModules = lib.mkForce [];
  services.nscd.enable = false;
  users.ldap.nsswitch = false;
}
