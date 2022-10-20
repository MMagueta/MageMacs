{ pkgs, ... }:

let
  emacsPackaging = pkgs.emacsPackagesNg;
  myEmacs = pkgs.emacs;
  emacsWithPackages = (pkgs.emacsPackagesFor myEmacs).emacsWithPackages;
in
emacsWithPackages (epkgs: [
  (
    emacsPackaging.trivialBuild {
      pname = "myInit";
      version = "1970-01-01";

      src = ./.;
      packageRequires =
        (with epkgs.melpaPackages; [
          all-the-icons
          cider
          clojure-mode
          company-coq
          company-erlang
          company-quickhelp
          csharp-mode
          dashboard
          diff-hl
          doom-modeline
          elsa
          erlang
          eshell-syntax-highlighting
          flycheck
          flycheck-elsa
          flymake-flycheck
          fsharp-mode
          fstar-mode
          haskell-mode
          haskell-snippets
          helm
          hy-mode
          lfe-mode
          linum-relative
          lsp-haskell
          lsp-metals
          lsp-mode
          lsp-treemacs
          lsp-ui
          magit
          multi-term
          multiple-cursors
          nix-mode
          ob-sml
          org-bullets
          org-super-agenda
          pastelmac-theme
          plantuml-mode
          projectile
          rainbow-delimiters
          request
          sly
          sml-basis
          sml-modeline
          transpose-frame
          tuareg
          use-package
          yasnippet
          yasnippet-snippets
        ]) ++ (with epkgs.elpaPackages; [
          yasnippet-classic-snippets
          sml-mode
        ]);
    })
])
