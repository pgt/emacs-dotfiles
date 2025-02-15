;;; init-packages.el --- Declare, install and update Emacs packages.
;;; Commentary:
;;; Code:
(require 'package)
(package-initialize)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)

;; upload straight: next-generation, purely functional package manager
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

(setq package-enable-at-startup nil)

;; use-package initialization
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (require 'use-package))

; (use-package use-package-ensure-system-package
;   :ensure t)

; (setq use-package-always-ensure t)
; (setq use-package-compute-statistics t)

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
(use-package undo-tree)
(use-package restclient)
(use-package expand-region)

;; Ruby
(use-package rbenv)
(use-package inf-ruby)
(use-package rspec-mode)
(use-package bundler)

;; Go-lang
(use-package go-mode)
(use-package company-go)
(use-package gotest)

;; Search
(use-package ag)
(use-package anzu)
(use-package swiper)

;; YAML
(use-package yaml-mode)

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
(use-package alchemist)

;; Frontend
(use-package scss-mode)
(use-package sass-mode)

;; PHP
(use-package php-mode)

;; Java/Android
(use-package android-mode)
(use-package kotlin-mode)

;; Swift
(use-package swift-mode)

;; Rust
(use-package rust-mode)
(use-package racer)
(use-package cargo)

;; Protobuffer
(use-package protobuf-mode)

;; Dart/Flutter
(use-package dart-mode)
(use-package flutter)

;; Groovy
(use-package groovy-mode)

;; Fountain
(use-package fountain-mode)

; (use-package diminish)

;; Clojure
(use-package cider)
(use-package clojure-mode)
(use-package clojure-mode-extra-font-locking)

;; Undo tree
(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode 1))

(straight-use-package 'gptel)

;; To require
(defvar libs-to-require
  '(cl
    uniquify
    linum
    whitespace
    recentf
    saveplace
    ansi-color
    dired-x
    s
    sh-script
    sgml-mode
    nxml-mode
    yaml-mode
    ))

;; vendor loading
(dolist (lib libs-to-require)
  (require lib))

(provide 'init-packages)
;;; init-packages.el ends here
