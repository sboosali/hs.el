;;; hs.el --- Haskell IDE -*- lexical-binding: t -*-


;; Copyright © 2019 Spiros Boosalis
;; Copyright © 2016 Jean-Philippe Bernardy
;; Copyright © 2016 Chris Done
;; Copyright © 2015 Athur Fayzrakhmanov
;; Copyright © 2013 Herbert Valerio Riedel
;; Copyright © 2007 Stefan Monnier

;; Version: 0.0
;; Homepage: https://github.com/sboosali/hs.el
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
;; and flavors (e.g. « ghc » and « ghcjs »).  The target is inferred by parsing the project file
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
;;; Imports ------------------------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;
;; Builtin Features:

(require 'cl-lib)
;;TODO (require 'version)
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
;;; Constants ----------------------------------;;
;;----------------------------------------------;;

(defvar hs-versioned-compiler-project-file-regex

  (rx "cabal"
      "-"
      "ghc"
      (? "js")
      (? "-"
         (1+ digit)
         (repeat 0 2
                 "."
                 (1+ digit))))

  "Regular expression which (optionally) matches a compiler flavor and/or compiler version.

Matches: \"cabal-ghcjs\", \"cabal-ghcjs-7\", \"cabal-ghc-7.10\", \"cabal-ghc-8.6.3\".")

;;----------------------------------------------;;

(defconst hs--minimal-ghc-version "7.10")

;;----------------------------------------------;;

(defconst hs--minimal-cabal-version "2.0.0.0")

;;----------------------------------------------;;

(defconst hs--minimal-emacs-version "26.1")

;;----------------------------------------------;;







;;----------------------------------------------;;
;;; Types --------------------------------------;;
;;----------------------------------------------;;

(cl-defstruct hs-pvp-version
  (a 0   :read-only t)                  ; « a :: Int »
  (b 0   :read-only t)                  ; « b :: Int »
  (c 0   :read-only t)                  ; « c :: Int »
  (d nil :read-only t)                  ; « d :: [Int] »
  )

;;----------------------------------------------;;


;;----------------------------------------------;;







;;----------------------------------------------;;
;;; Variables ----------------------------------;;
;;----------------------------------------------;;


;;----------------------------------------------;;


;;----------------------------------------------;;


;;----------------------------------------------;;


;;----------------------------------------------;;








;;----------------------------------------------;;
;;; Customization ------------------------------;;
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

    ;; ^ Matches a contributor-specific project file, like « cabal-sboo.project »."

  :group 'hs
  :type '(choice (const nil) string))

(put 'hs-default-project-file-basename-regex 'safe-local-variable #'stringp)

;TODO (let ((USER (user-login-name))) (rx "cabal" ?- (eval USER)))

;;(rx "cabal" ?- (eval (user-login-name)))

;;----------------------------------------------;;

(defcustom hs-project-file-basename-list

  (list "cabal-ghc-7.10.3"
        "cabal-ghc-8.0.2"
        "cabal-ghc-8.2.2"
        "cabal-ghc-8.4.4"
        "cabal-ghc-8.6.3"
        ;; "cabal-"
        ;; "cabal-"
        ;; "cabal-"
        ;; "cabal-"
        ;; "cabal-"
        ;; "cabal-"
        "cabal-ghcjs"
        "cabal")

  "Regular expression which matches the basename of a « .project » file.

For example:

    (setq hs-default-project-file-basename-regex (rx \"cabal\" ?- (eval (user-login-name))))

    ;; ^ Matches a contributor-specific project file, like « cabal-sboo.project »."

  :group 'hs
  :type '(list string))

;;TODO list-of-string (put 'hs-project-file-basename-list 'safe-local-variable #'stringp)

;;----------------------------------------------;;

(defcustom hs-ghc-program "ghc"

  "Which « ghc » program should `hs' invoke?

Can be a relative filepath (like the default,
which is just the command name),
or an absolute filepath
\(useful to pin a development version,
or if Emacs's PATH environment variables is corrupt).

MUST be visible to `executable-find'."

  :group 'hs
  :type 'string)

(put 'hs-ghc-program 'safe-local-variable #'stringp)

;;----------------------------------------------;;

(defcustom hs-cabal-program "cabal"

  "Which « cabal » program should `hs' invoke?

Can be a relative filepath (like the default,
which is just the command name),
or an absolute filepath
\(useful to pin a development version,
or if Emacs's PATH environment variables is corrupt).

MUST be visible to `executable-find'."

  :group 'hs
  :type 'string)

(put 'hs-cabal-program 'safe-local-variable #'stringp)

;;----------------------------------------------;;








;;----------------------------------------------;;


;;----------------------------------------------;;
;;; Utilities (Private API) --------------------;;
;;----------------------------------------------;;

;;----------------------------------------------;;

(defun hs--run-ghc (&rest arguments)

  "Shell out to ‹ghc›.

Optional argument ARGUMENTS: other ‘command-line’ options and/or arguments (will be passed through verbatim)."

  ())

;; ^

;;----------------------------------------------;;

(defun hs--run-cabal (&rest arguments)

  "Shell out to ‹cabal›.

Optional argument ARGUMENTS: other ‘command-line’ options and/or arguments (will be passed through verbatim)."

  ())

;; ^

;;----------------------------------------------;;

(defun hs--pvp-version-to-plist (VERSION-STRING)

  "Parse a `VERSION-STRING' into the PVP version format.

Arguments:

- `VERSION-STRING' must be parseable by `version-to-list'.

Returns:

- A `hs-pvp-version' upon success.
- `nil' upon failure.

Examples:

    M-: (hs--pvp-version-to-plist \"2.4.1.0\")
     ⤷ (:a 2 :b 4 :c 4 :d (0))

NOTE the Package Versioning Policy (PVP) states:

- A.B is known as the major version number, and C the minor version number.
- A package version number SHOULD have the form A.B.C, and MAY optionally have any number of additional components, for example 2.1.0.4 (in this case, A=2, B=1, C=0).

See URL `https://pvp.haskell.org/'."

    (pcase (version-to-list VERSION-STRING)

      (`(,a ,b ,c)

       (make-hs-pvp-version :a a
                            :b b
                            :c c))

      (`(,a ,b ,c ,d)

         (make-hs-pvp-version
          :a a
          :b b
          :c c
          :d `(,d)))

      (`(,a ,b ,c ,d ,e)

         (make-hs-pvp-version
          :a a
          :b b
          :c c
          :d `(,d ,e)))

      (_ nil)

      ))

;; e.g.
;;
;;    M-: (version-to-list "2.4.1.0")
;;     ⤷ (2 4 1 0)
;;
;;    M-: (hs--pvp-version-to-plist "2.4.1.0")
;;     ⤷ (:major (2 4) :minor 4 :patch 0)
;;

;;----------------------------------------------;;

(defun hs--ghc-version ()

  "Get the version of the (available) ‹ghc› program."

  (hs--run-ghc "--numeric-version"))

;; ^ e.g.
;;
;;    $ ghc --numeric-version
;;    8.6.3

;;
;;    $ ghc --version
;;    The Glorious Glasgow Haskell Compilation System, version 8.6.3

;;----------------------------------------------;;

(defun hs--cabal-version ()

  "Get the version of the (available) ‹cabal› program."

  (hs--run-cabal "--numeric-version")

)

;; ^ e.g.
;;
;;    $ cabal --numeric-version
;;    2.4.1.0
;;
;;    $ cabal --version
;;    cabal-install version 2.4.1.0
;;    compiled using version 2.4.1.0 of the Cabal library

;;----------------------------------------------;;

(defun hs--project-file-filename-regex (&optional BASENAME)

  "Regular expression which matches the entirety of the filename of a “.project” file.

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

  "Find the nearest « `BASENAME'.project » file that is an ancestor of `DIRECTORY'.

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

(cl-defun hs--probe-project-file (&key directory basename)

  "Probe DIRECTORY for “BASENAME.project”."

    (let* (()
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
;;; Commands (and Public API) ------------------;;
;;----------------------------------------------;;

(cl-defun hs-find-project-file (&key directory basename)

  "Find a Cabal project file.

Find a file:

* with a “.project” extension,
* whose basename matches BASENAME,
* which is an ancestor of DIRECTORY.

We try `hs--find-dominating-project-file':

* returning it (i.e. `hs--find-dominating-project-file') if successful (i.e. non-nil);
* returning « `directory'/`basename'.project » if « `directory'/`basename'.project » exists
* failing otherwise (i.e. returning nil).

Examples:

    M-: (hs-find-project-file :directory `default-directory' :basename (rx \"cabal\" ?- \"ghc\" (zero-or-one \"js\") ?- (one-or-more digit) (repeat 0 3 ?. (one-or-more digit))))
   (\"cabal-ghc-8.4\" \"cabal-ghc-8.6.3\" \"cabal-ghcjs\")

Also see URL `http://hackage.haskell.org/package/filepath-1.4.2.1/src/System/FilePath/Internal.hs'."

  (interactive (list
                (read-directory-name "Directory to start the search from (default 'default-directory): ")
                (completing-read "Basename of the “.project” file (default “cabal”): " hs-project-file-basename-list)))

  (or (hs--find-dominating-project-file directory basename)
      ()
      nil))

;TODO \"cabal-ghcjs-[[:digit:]]+\\\\.[[:digit:]]+\\\\.[[:digit:]]+\")

;;----------------------------------------------;;

;;----------------------------------------------;;
;;; Functions (Public API)
;;----------------------------------------------;;


;; (when (version< emacs-version hs--minimal-emacs-version)
;;   ())

;; (when (version< (hs--ghc-version) hs--minimal-ghc-version)
;;   ())

;; (when (version< (hs--cabal-version) hs--minimal-cabal-version)
;;   ())



;;----------------------------------------------;;
;;; Filesystem ---------------------------------;;
;;----------------------------------------------;;




;;----------------------------------------------;;
;;; Loading Libraries --------------------------;;
;;----------------------------------------------;;

(provide 'hs)

;; (eval-after-load 'bookmark
;;   '(require 'hs-bookmark))

;; (add-hook 'after-init-hook #'hs-startup-asserts t)

;;----------------------------------------------;;
;; Notes ---------------------------------------;;
;;----------------------------------------------;;

;; findProjectRoot :: Maybe FilePath   -- ^ starting directory, or current directory
;;                 -> Maybe FilePath   -- ^ @cabal.project@ file name override
;;                 -> IO (Either BadProjectRoot ProjectRoot)

;;----------------------------------------------;;

;; e.g. `version-to-list'
;;
;; M-: (version-to-list "8.6.3")
;;     (8 6 3)
;;
;; M-: (version-to-list "2.4.1.0")
;;     (2 4 1 0)
;;
;; M-: (version-to-list "2.4.1.0-pre")
;;     (2 4 1 0 -1)
;;

;;----------------------------------------------;;

;; PVP...

;; > A package version number SHOULD have the form A.B.C, and MAY optionally have any number of additional components, for example 2.1.0.4 (in this case, A=2, B=1, C=0). This policy defines the meaning of the first three components A-C, the other components can be used in any way the package maintainer sees fit.
;; >
;; > Version number ordering is already defined by Cabal as the lexicographic ordering of the components. For example, 2.0.1 > 1.3.2, and 2.0.1.0 > 2.0.1.
;; >
;; > A.B is known as the major version number, and C the minor version number.

;;----------------------------------------------;;




;;----------------------------------------------;;
;;; dante.el ends here
(provide 'hs)

;;; hs.el ends here
