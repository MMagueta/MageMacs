{pkgs, ...}:
let
            myEmacs = pkgs.emacs;
            initEl = pkgs.stdenv.mkDerivation {
              name = "MageMacsElisp";
              src = ./.;
              installPhase = ''
                mkdir $out
                mkdir $out/sources
                cp $src/sources/* $out/sources/
                cp *.el $out
              '';
            };
            emacsWithPackages = (pkgs.emacsPackagesFor myEmacs).emacsWithPackages;
          in
          emacsWithPackages (epkgs: (with epkgs.melpaPackages; [
            (pkgs.runCommand "default.el" { } ''
              mkdir -p $out/share/emacs/site-lisp
              cp -r ${initEl} $out/share/emacs/site-lisp
            '')
            yasnippet-snippets
            yasnippet
            use-package
            tuareg
            transpose-frame
            sml-modeline
            sml-basis
            sly
            request
            rainbow-delimiters
            projectile
            plantuml-mode
            pastelmac-theme
            org-super-agenda
            org-bullets
            ob-sml
            nix-mode
            multiple-cursors
            multi-term
            magit
            lsp-ui
            lsp-treemacs
            lsp-mode
            lsp-metals
            lsp-haskell
            linum-relative
            lfe-mode
            hy-mode
            helm
            haskell-snippets
            haskell-mode
            fsharp-mode
            fstar-mode
            flymake-flycheck
            flycheck-elsa
            flycheck
            eshell-syntax-highlighting
            erlang
            elsa
            doom-modeline
            diff-hl
            dashboard
            csharp-mode
            company-quickhelp
            company-erlang
            company-coq
            clojure-mode
            cider
            all-the-icons
          ]) ++ (with epkgs.elpaPackages; [
            yasnippet-classic-snippets
            sml-mode
          ]))