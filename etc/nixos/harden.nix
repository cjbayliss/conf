{ config, pkgs, lib, ... }:

{
  # import the activly maintained hardend profile, you can find it here:
  # https://github.com/NixOS/nixpkgs/blob/master/nixos/modules/profiles/hardened.nix
  imports = [ <nixpkgs/nixos/modules/profiles/hardened.nix> ];

  # restore memoryAllocator to default or many programs crash:
  environment.memoryAllocator.provider = "libc";
  # IMPORTANT: don't block modules from being loaded
  security.lockKernelModules = false;

  # don't use the 'hardened' kernel
  boot.kernelPackages = let
    linux_5_14_pkg = { fetchurl, buildLinux, callPackage, ... } @ args:

      buildLinux (args // rec {
        patches = callPackage <nixpkgs/pkgs/os-specific/linux/kernel/patches.nix> { };
        version = "5.14.21";
        modDirVersion = version;

        src = fetchurl {
          url = "mirror://kernel/linux/kernel/v5.x/linux-${version}.tar.xz";
          sha256 = "1cr381c179nfdrq95l4j56c4ygw09sxv493553ix4b80naf2a6pl";
        };
        kernelPatches = [
          patches.bridge_stp_helper
          patches.request_key_helper
        ];

      } // (args.argsOverride or {}));
    linux_5_14 = pkgs.callPackage linux_5_14_pkg{};
  in
    pkgs.recurseIntoAttrs (pkgs.linuxPackagesFor linux_5_14);

  # don't disable multi-threading (yeah, ik multi-threading has risks)
  security.allowSimultaneousMultithreading = true;

  # TODO: finish the job cjb!
  # harden systemd services. See: systemd-analyze security start by
  # disabling any service and deps that we don't need:
  system.nssModules = lib.mkForce [ ];
  services.nscd.enable = false;
  users.ldap.nsswitch = false;

  services.dbus.apparmor = "enabled";
}
