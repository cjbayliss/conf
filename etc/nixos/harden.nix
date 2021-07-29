{ config, pkgs, lib, ... }:

{
  # import the activly maintained hardend profile, you can find it here:
  # https://github.com/NixOS/nixpkgs/blob/master/nixos/modules/profiles/hardened.nix
  imports = [
    <nixpkgs/nixos/modules/profiles/hardened.nix>
  ];

  # restore memoryAllocator to default or many programs crash:
  environment.memoryAllocator.provider = lib.mkForce "libc";

  # TODO: finish the job cjb!
  # harden systemd services. See: systemd-analyze security start by
  # disabling any service and deps that we don't need:
  system.nssModules = lib.mkForce [];
  services.nscd.enable = false;
  users.ldap.nsswitch = false;

  services.dbus.apparmor = "enabled";
}
