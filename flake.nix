{
  description = "Haskell dev shell";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs = { nixpkgs, ... }:
  let
    name = "example"; 
    system = "x86_64-linux";
    src = ./.;
    pkgs = import nixpkgs { inherit system; };
    hpkgs = pkgs.haskell.packages.ghc910;
    haskell_tools = with hpkgs; [
      haskell-language-server
      cabal-install
    ];
    tools = with pkgs; [
      zlib
      postgresql
      pkg-config      
    ];

    shellPackages = [
      hpkgs.hoogle
      hpkgs.bhoogle
    ];
        
    nativeBuildInputs = haskell_tools ++ tools ++ shellPackages;
    shell = pkgs.mkShell
    {
      inherit  name nativeBuildInputs;
      shellHook = ''
      alias claude="claude --plugin-dir=./.claude/plugins/haskell-lsp"      
      '';
      LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath nativeBuildInputs;
    };

  in
  {
     devShells."${system}".default = shell;
     packages."${system}".default = hpkgs.callCabal2nix "${name}" src { };  
  };
}
