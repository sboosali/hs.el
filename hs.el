;;; hs.el --- Haskell IDE -*- lexical-binding: t -*-

;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;;
;;; Metadata

;; Copyright (c) 2019 Spiros Boosalis

;; Author: Spiros Boosalis <samboosalis@gmail.com>
;; Maintainer: Spiros Boosalis <samboosalis@gmail.com>
;; URL: https://github.com/sboosali/hs.el
;; Created: January 2019
;; Keywords: haskell, tools
;; Package-Requires: ((emacs "26.1") (dash "2.12.0") (s "1.11.0") (f "0.19.0"))
;; Version: 0.0

;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;;
;;; License

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;;
;;; Commentary:

;; « hs.el »: HaSkell mode.

;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;;
;;; Code:

;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;;
;; Builtin Features:

(require 'cl-lib)
(require 'flymake)
(require 'xref)

;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;;
;; Extermal Features:

(require 'dash)
(require 's)
(require 'f)

;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;;



;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;; ;;