;;; packages.el --- coq Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Olivier Verdier <olivier.verdier@gmail.com>
;; URL: https://github.com/olivierverdier/spacemacs-coq
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq coq-packages
    '(
      company-coq
      (proof-general :location local)
      ))

;; List of packages to exclude.
(setq coq-excluded-packages '())

;; For each package, define a function coq/init-<package-name>
;;
;; (defun coq/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

(defun setup-coq-keys-for-map (state)
  (evil-define-key state coq-mode-map
    (kbd "<f3>") #'proof-assert-next-command-interactive)
  (evil-define-key state coq-mode-map
    (kbd "<f4>") #'company-coq-proof-goto-point)
  (evil-define-key state coq-mode-map
    (kbd "<f2>") #'proof-undo-last-successful-command))

(defun setup-coq-keys ()
  (setup-coq-keys-for-map 'normal)
  (setup-coq-keys-for-map 'insert))

(defun hide-mode-statuses ()
  (diminish 'company-mode)
  (diminish 'yas-minor-mode)
  (diminish 'hs-minor-mode)
  (diminish 'outline-minor-mode)
  (diminish 'company-mode)
  (diminish 'holes-mode))

(defun coq/init-company-coq ()
  (use-package company-coq
    :defer t
    :init
    (add-hook 'coq-mode-hook #'company-coq-mode)
    (add-hook 'coq-mode-hook #'setup-coq-keys)
    (add-hook 'coq-mode-hook #'hide-mode-statuses)
    :config
    (progn
      (spacemacs/declare-prefix-for-mode
       'coq-mode
       "mi" "company-coq/insert")
      (spacemacs/set-leader-keys-for-major-mode 'coq-mode
        "il" 'company-coq-lemma-from-goal
        "im" 'company-coq-insert-match-construct))))

(defun coq/init-proof-general ()
  "Initialize Proof General."
  ;; Setup from Proof General README, using a Homebrew path. Proof General
  ;; lazily loads from proof-site, so there's no need to use-package it.
  (load "/usr/local/share/emacs/site-lisp/proof-general/generic/proof-site")
  (dolist (prefix '(("ml" . "pg/layout")
                    ("mp" . "pg/prover")
                    ("ma" . "pg/ask-prover")
                    ("mai" . "show-implicits")
                    ("man" . "show-all") ; n is for notation
                    ("mg" . "pg/goto")))
    (spacemacs/declare-prefix-for-mode
     'coq-mode
     (car prefix) (cdr prefix)))
  (spacemacs/set-leader-keys-for-major-mode 'coq-mode
    "]" 'proof-assert-next-command-interactive
    "[" 'proof-undo-last-successful-command
    "." 'proof-goto-point
    ;; Layout
    "ll" 'proof-layout-windows
    "lc" 'pg-response-clear-displays
    "lp" 'proof-prf
    ;; Prover Interaction
    "px" 'proof-shell-exit
    "pc" 'proof-interrupt-process
    "pr" 'proof-retract-buffer
    "af" 'proof-find-theorems
    "ap" 'coq-Print
    "ac" 'coq-Check
    "ab" 'coq-About
    "aip" 'coq-Print-with-implicits
    "aic" 'coq-Check-show-implicits
    "aib" 'coq-About-with-implicits
    "anp" 'coq-Print-with-all
    "anc" 'coq-Check-show-all
    "anb" 'coq-About-with-all
    "g." 'proof-goto-end-of-locked
    "ga" 'proof-goto-command-start
    "ge" 'proof-goto-command-end)
  (add-hook 'coq-mode-hook
            (lambda ()
              (setq-local
               evil-insert-state-cursor
               (cons
                (car evil-normal-state-cursor)
                (cdr evil-insert-state-cursor))))))
