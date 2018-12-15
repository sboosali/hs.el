;;; hs.el --- Haskell IDE -*- lexical-binding: t -*-


;; Copyright © 2019 Spiros Boosalis
;; Copyright © 2016 Jean-Philippe Bernardy
;; Copyright © 2016 Chris Done
;; Copyright © 2015 Athur Fayzrakhmanov
;; Copyright © 2013 Herbert Valerio Riedel
;; Copyright © 2007 Stefan Monnier

;; Version: 0.0
;; URL: https://github.com/sboosali/hs.el
;; Author: Spiros Boosalis <samboosalis@gmail.com>
;; Maintainer: Spiros Boosalis <samboosalis@gmail.com>
;; Created: January 2019
;; Keywords: haskell, tools
;; Package-Requires: ((emacs "26.1") (dash "2.12.0") (s "1.11.0") (f "0.19.0"))


;; This file is NOT part of GNU Emacs.

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


;;; Commentary:

;; « hs.el »: a {H}a{S}kell IDE.
;;
;; Develop Cabal projects, supporting « cabal new-* » and « cabal.project ».
;;
;; Switch easily between components (e.g. « lib », « test », « exe », « bench », « test:_:unit », « test:_:doc », etc) and between compiler versions (e.g. « ghc-8.6.3 », « ghc-7.10.3 », etc)
;; and flavors (e.g. « ghc » and « ghcjs »). The target is inferred by parsing the project file
;; or cabal file (in particular, for « hs-source-dirs »),
;; and by heurists of ancestor directories (e.g. « src/**/*.hs » and « sources/**/*.hs » default to « lib », while « test/**/*.hs » and « tests/**/*.hs » default to « test »).
;;
;; Uses mostly Emacs-builtin features (i.e. `compilation-mode', `flymake', `eldoc', `ibuffer', etc)
;; and functions (`completing-read', M-x `compile', etc).That is, instead of external packages
;; like FlyCheck or Company (those may be integrated with in a separate package).
;;
;; 
;;


;;; Code:


;;----------------------------------------------;;
;;; Imports
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Builtin Features:

(require 'cl-lib)
(require 'compile)
;; (require 'thingatpt)
;; (require 'flymake)
;; (require 'ibuffer)
;; (require 'xref)

;;----------------------------------------------;;
;; External Features:

(require 'dash)
(require 's)
(require 'f)


;;----------------------------------------------;;
;;; Customization
;;----------------------------------------------;;

(defgroup hs nil

  "Develop Haskell projects."

  :group 'haskell

  :link '(url-link :tag "GitHub" "https://github.com/sboosali/hs.el")
  ;;TODO :link '(url-link :tag "Online Manual" "https://docs.hs.io/")
  :link '(emacs-commentary-link :tag "Commentary" "hs"))


;;----------------------------------------------;;

(defcustom hs-default-project-file-basename-regex nil

  "Regular expression which matches the basename of a « .project » file.

For example:

    (setq hs-default-project-file-basename-regex (rx \"cabal\" ?- (eval (user-login-name))))

    ;; ^ Matches a contributor-specific project file, like « cabal-sboo.project ».
"

  :group 'hs
  :type '(choice (const nil) string))

(put 'hs-default-project-file-basename-regex 'safe-local-variable #'stringp)

;TODO (let ((USER (user-login-name))) (rx "cabal" ?- (eval USER)))

;;(rx "cabal" ?- (eval (user-login-name)))


;;----------------------------------------------;;
;;; Utilities
;;----------------------------------------------;;

(defun hs--project-file-filename-regex (&optional BASENAME)

  "Regular expression which matches the entirety of the filename of a « .project » file.

For example:

    M-: (hs--project-file-filename-regex \"cabal-ghcjs\")
    \"cabal-ghcjs.project\"

    M-: (hs--project-file-filename-regex (rx \"cabal-\" (one-or-more word)))
    \"\\`cabal-[[:word:]]+.project\\'\"

Equals either:

* « `BASENAME'.project »
* « `hs-default-project-file-basename-regex'.project »
* « cabal.project »

depending on whether the relevant variables are provided / customized."

  (let ((BASENAME (or BASENAME
                      (bound-and-true-p hs-default-project-file-basename-regex)
                      "cabal")))

    (format "\\`%s.project\\'" BASENAME)))

;; ^ NOTE « (bound-and-true-p FOO) » returns « FOO », not « t ».

;TODO (format "%s" BASENAME)
;TODO (rx-to-string (rx string-start) BASENAME (rx string-end))))

;;----------------------------------------------;;

(cl-defun hs--find-dominating-project-file (&key directory basename)

  "

Arguments:

* DIRECTORY defaults to `default-directory'
* BASENAME is given to `hs--project-file-filename-regex' 

See:

* `findProjectRoot' in URL `http://hackage.haskell.org/package/cabal-install-2.4.1.0/src/Distribution/Client/ProjectConfig.hs', and
* `findPackageDesc' in URL `http://hackage.haskell.org/package/Cabal-2.4.1.0/docs/Distribution-Simple-Utils.html'.

For example:

    ;; (Assuming « ~/haskell/project/src/Example.hs » and « ~/haskell/project/cabal-ghcjs.project »):

    M-: (hs--find-dominating-project-file :basename \"cabal-ghcjs\")
    \"/home/sboo/haskell/project/cabal-ghcjs.project\"

"

  (let* ((DIRECTORY (f-canonical (or directory
                                     default-directory)))
         (BASENAME (hs--project-file-regex basename))
         (FILEPATH (f-join DIRECTORY BASENAME)))

    (let* ((isAbsolute    (f-absolute? FILEPATH))
           (doesFileExist (f-file?     FILEPATH))
           )

      (if doesFileExist
          (f-canonical FILEPATH)

        (let ((startingDirectory DIRECTORY)
              (homeDirectory     (expand-file-name "~"))
              )

          (hs--probe-dominating-file DIRECTORY FILEPATH)

          )))))

;;----------------------------------------------;;

(cl-defun hs--probe-dominating-file (&key filepath directory basename)

  "Search upwards; If we reach the user's home dir or the filesystem root, then return the current dir.

"

    (let* ((isHome  (f-equal? FILEPATH))
           (isDrive ( FILEPATH))
           )

  ())

; isDrive dir || dir == homedir

;;----------------------------------------------;;

(cl-defun hs--find-dominating-package-description (&key directory basename)

  "Find a package description file in the directory DIR.
Returns nil if none or multiple \".cabal\" files were found.  If
ALLOW-MULTIPLE is non nil, in case of multiple \".cabal\" files,
a list is returned instead of failing with a nil result."
  ;; This is basically a port of Cabal's
  ;; Distribution.Simple.Utils.findPackageDesc function
  ;;  http://hackage.haskell.org/packages/archive/Cabal/1.16.0.3/doc/html/Distribution-Simple-Utils.html
  ;; but without the exception throwing.


  (let* ((cabal-files
          (cl-remove-if 'file-directory-p
                        (cl-remove-if-not 'file-exists-p
                                          (directory-files dir t ".\\.cabal\\'")))))
    (cond
     ((= (length cabal-files) 1) (car cabal-files)) ;; exactly one candidate found
     (allow-multiple cabal-files) ;; pass-thru multiple candidates
     (t nil))))


;;----------------------------------------------;;

(provide 'hs)

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;; findProjectRoot :: Maybe FilePath   -- ^ starting directory, or current directory
;;                 -> Maybe FilePath   -- ^ @cabal.project@ file name override
;;                 -> IO (Either BadProjectRoot ProjectRoot)



;;----------------------------------------------;;
;;; dante.el ends here