;;; packages.el --- Coq Layer Packages for Spacemacs
;;
;; Copyright (c) 2019 Nathaniel Yazdani
;; Copyright (c) 2016 Tej Chajed
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Based on modified version by Tej Chajed.
;; Based on original version by Olivier Verdier.
;;
;; Author: Nathaniel Yazdani <nyazdani@ccs.neu.edu>
;; URL: https://github.com/nateyazdani/spacemacs-coq
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq coq-packages
      '(proof-general
        company-coq
        smartparens
        vi-tilde-fringe))

(setq coq-excluded-packages '())

(defvar coq/showing-implicits nil)
(defun coq/init-company-coq ()
  "Initialize Company Coq upon entering coq-mode."
  (use-package company-coq
    :defer t
    :init
    (progn
      (add-hook 'coq-mode-hook #'company-coq-mode)
      (add-to-list 'spacemacs-jump-handlers-coq-mode
                   'company-coq-jump-to-definition)
      (setq company-coq-disabled-features '(hello keybindings snippets)))
    :config
    (progn
      (spacemacs/declare-prefix-for-mode 'coq-mode "mx" "source")
      (spacemacs/set-leader-keys-for-major-mode 'coq-mode
        "." 'proof-goto-point
        ;"=" 'company-coq-eval-last-sexp

        ;; Source manipulation (text).
        "x-" 'company-coq-fold
        "x+" 'company-coq-unfold
        "x*" 'company-coq-toggle-definition-overlay
        "x/" 'company-coq-grep-symbol
        "xo" 'company-coq-occur
        "xg" 'company-coq-goto-occurence
        "xd" 'company-coq-diff-dwim
        "xj" 'company-coq-jump-to-definition
        "xs" 'company-coq-maybe-exit-snippet

        "il" 'company-coq-lemma-from-goal
        "im" 'company-coq-insert-match-construct
        "go" 'company-coq-occur
        "he" 'company-coq-document-error
        "hE" 'company-coq-browse-error-messages
        "hh" 'company-coq-doc

        ;; Documentation retrieval.
        "d" 'company-coq-doc

        ;; Prover help.
        "?" 'company-coq-document-error
        ))))

(defun coq/init-proof-general ()
  "Initialize Proof General."
  (use-package proof-site
    :defer t
    :init
    (progn
      (setq coq/proof-general-load-path
            (concat (configuration-layer/get-elpa-package-install-directory
                     'proof-general)
                    "generic"))
      (add-to-list 'load-path coq/proof-general-load-path)
      (load "~/.emacs.d/private/coq/pg-undo-tree.el")
      (setq proof-three-window-mode-policy 'hybrid
            proof-keep-response-history t
            proof-output-tooltips t
            proof-toolbar-enable nil
            proof-splash-enable nil))
    :config
    (progn
      (defun proof-search-location ()
        (interactive)
        (funcall-interactively 'coq-LocateConstant))
      (defun proof-search-notation ()
        (interactive)
        (funcall-interactively 'coq-LocateNotation))
      (defun proof-search-rewrite ()
        (interactive)
        (funcall-interactively 'coq-SearchRewrite))
      (defun proof-search-about ()
        (interactive)
        (funcall-interactively 'coq-SearchConstant))
      (defun proof-search-pattern ()
        (interactive)
        (funcall-interactively 'coq-SearchIsos))

      (defun proof-toggle-showing-implicits ()
        (interactive)
        (setq coq/showing-implicits (not coq/showing-implicits)))

      (defun proof-print ()
        (interactive)
        (if coq/showing-implicits
            (funcall-interactively 'coq-Print)
          (funcall-interactively 'coq-Print-with-implicits)))
      (defun proof-check ()
        (interactive)
        (if coq/showing-implicits
            (funcall-interactively 'coq-Check)
          (funcall-interactively 'coq-Check-show-implicits)))
      (defun proof-about ()
        (interactive)
        (if coq/showing-implicits
            (funcall-interactively 'coq-About)
          (funcall-interactively 'coq-About-with-implicits)))

      (defun proof-print-full ()
        (interactive)
        (funcall-interactively 'coq-Print-with-all))
      (defun proof-check-full ()
        (interactive)
        (funcall-interactively 'coq-Check-show-all))
      (defun proof-about-full ()
        (interactive)
        (funcall-interactively 'coq-About-with-all))

      (spacemacs/declare-prefix-for-mode 'coq-mode "ml" "layout")
      (spacemacs/declare-prefix-for-mode 'coq-mode "mp" "prover")
      (spacemacs/declare-prefix-for-mode 'coq-mode "ms" "search")
      (spacemacs/declare-prefix-for-mode 'coq-mode "mi" "inspect")
      (spacemacs/declare-prefix-for-mode 'coq-mode "mg" "goto")

      (spacemacs/set-leader-keys-for-major-mode 'coq-mode
        ;; Prover interaction.
        "]" 'proof-assert-next-command-interactive
        "[" 'proof-undo-last-successful-command
        "/" 'proof-find-theorems ; same as 'coq-SearchConstant

        ;; Window layout
        "ll" 'proof-layout-windows
        "lt" 'proof-tree-external-display-toggle
        "lr" 'proof-display-some-buffers
        "lc" 'pg-response-clear-displays
        "lp" 'proof-prf

        ;; Prover management.
        "pc" 'proof-ctxt
        "ps" 'proof-toggle-active-scripting
        "pb" 'proof-process-buffer
        "pr" 'proof-retract-buffer
        "pk" 'proof-interrupt-process
        "pq" 'proof-shell-exit

        ;; Context search.
        "sl" 'proof-search-location
        "sn" 'proof-search-notation
        "sr" 'proof-search-rewrite
        "sp" 'proof-search-pattern
        "sa" 'proof-search-about

        ;; Context inspection.
        "c" 'proof-check
        "a" 'proof-about
        "ip" 'proof-print
        "iP" 'proof-print-full
        "ic" 'proof-check
        "iC" 'proof-check-full
        "ia" 'proof-about
        "iA" 'proof-about-full
        "i!" 'proof-toggle-showing-implicits

        ;; Point motion (goto).
        "g." 'proof-goto-end-of-locked
        "g0" 'proof-goto-command-start
        "g$" 'proof-goto-command-end
        "gb" 'proof-backward-command
        "gw" 'proof-forward-command
        ))))

(defun coq/post-init-smartparens ()
  (add-hook 'coq-mode-hook #'spacemacs//activate-smartparens))

(defun coq/post-init-vi-tilde-fringe ()
  (spacemacs/add-to-hooks 'spacemacs/disable-vi-tilde-fringe
                          '(coq-response-mode-hook
                            coq-goals-mode-hook)))
