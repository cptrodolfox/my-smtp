# my-smtp
A haskell implementation of the SMTP described by RFC 821. This is for educational work only, is not fully functional :).

NIXOS Instructions:

To work inside nixos first you should use:
nix-env --attr env release.nix
If you need to add new depedencies, add them to the cabal file.
Then run:
cabal2nix . > default.nix

Happy hacking :)