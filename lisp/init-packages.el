;;; init-packages.el --- Declare, install and update Emacs packages.
;;; Commentary:
;;; Code:

;; Bootstrap straight.el - next-generation, purely functional package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package via straight and integrate
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Compatibility shims for legacy Flymake integrations like scss-mode
(defvar flymake-allowed-file-name-masks nil)
(defvar flymake-err-line-patterns nil)

;;
;; A list of packages to ensure are installed at launch
;;

;; General
(use-package s)
(use-package smartscan)
(use-package helm)
(use-package projectile)
(use-package google-this)
(use-package paredit)
(use-package restclient)
(use-package expand-region)
(use-package company)
(use-package flycheck)

;; Undo tree (with config)
(use-package undo-tree
  :init
  (global-undo-tree-mode 1))

;; Ruby
(use-package rbenv)
(use-package inf-ruby)
(use-package rspec-mode)
(use-package bundler)

;; Go-lang
(use-package go-mode)
(use-package gotest)

;; Search
(use-package ag)
(use-package anzu)
(use-package swiper)

;; YAML
(use-package yaml-mode)

;; Infrastructure
(use-package terraform-mode
  :hook ((terraform-mode . terraform-format-on-save-mode)
         (terraform-mode . lsp-deferred)))
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init (setq lsp-keymap-prefix "C-c l"))

;; UI
(use-package popup)
(use-package gruvbox-theme)
(use-package solarized-theme)
(use-package atom-one-dark-theme)
(use-package atom-dark-theme)

;; SSH
(use-package ssh-config-mode)

;; HTML
(use-package rhtml-mode)
(use-package web-mode)

;; HAML
(use-package haml-mode)

;; Javascript
(use-package js2-mode)
(use-package js2-refactor)

;; TypeScript
(use-package typescript-mode)
(use-package tide)

;; Git tools
(use-package magit)
(use-package git-timemachine)
(use-package git-gutter)
(use-package diff-hl)

;; Markdown
(use-package markdown-mode)

;; Org
(use-package org-bullets)

;; Elixir
(use-package elixir-mode)

;; Frontend
(use-package scss-mode)
(use-package sass-mode)

;; Containers
(use-package dockerfile-mode)
(use-package docker)
(use-package kubernetes)
(use-package k8s-mode)

;; PHP
(use-package php-mode)

;; Java/Android
(use-package android-mode)
(use-package kotlin-mode)

;; Swift
(use-package swift-mode)

;; Rust
(use-package rust-mode)
(use-package cargo)

;; Protobuffer
(use-package protobuf-mode)

;; Python
(use-package python-pytest)
(use-package lsp-pyright
  :custom (lsp-pyright-langserver-command "pyright")
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))

(use-package python-black
  :demand t
  :after python
  :hook (python-mode . python-black-on-save-mode-enable-dwim))

;; Dart/Flutter
(use-package dart-mode)
(use-package flutter)

;; Groovy
(use-package groovy-mode)

;; Fountain
(use-package fountain-mode)

;; Clojure
(use-package cider)
(use-package clojure-mode)
(use-package clojure-mode-extra-font-locking)

;; Claude/Copilot/AI
(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c C-'" . claude-code-ide-menu)
  :config
  (claude-code-ide-emacs-tools-setup))

(straight-use-package '(copilot :type git :host github :repo "copilot-emacs/copilot.el" :files ("*.el" "dist")))
(straight-use-package 'gptel)

;; Transient (required by magit and others)
(require 'transient)

;; Built-in libraries to require
(defvar libs-to-require
  '(cl-lib
    uniquify
    whitespace
    recentf
    saveplace
    ansi-color
    dired-x
    sh-script
    sgml-mode
    nxml-mode))

;; Load built-in libraries
(dolist (lib libs-to-require)
  (require lib))

(provide 'init-packages)
;;; init-packages.el ends here
