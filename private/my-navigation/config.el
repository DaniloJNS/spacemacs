;;; config.el --- My Navigation Layer config File for Spacemacs -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2012-2024 Danilo Nascimento
;;
;; Author: DaniloJNS <dan.silva.13.zx@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(defvar spacemacs/lookup-provider-url-alist
  (append '(("Doom issues"       "https://github.com/orgs/doomemacs/projects/2/views/30?filterQuery=%s")
            ("Doom discourse"    "https://discourse.doomemacs.org/search?q=%s")
            ("Google"            +lookup--online-backend-google "https://google.com/search?q=%s")
            ("Google images"     "https://www.google.com/images?q=%s")
            ("Google maps"       "https://maps.google.com/maps?q=%s")
            ("Kagi"              "https://kagi.com/search?q=%s")
            ("Project Gutenberg" "http://www.gutenberg.org/ebooks/search/?query=%s")
            ("DuckDuckGo"        +lookup--online-backend-duckduckgo "https://duckduckgo.com/?q=%s")
            ("DevDocs.io"        "https://devdocs.io/#q=%s")
            ("StackOverflow"     "https://stackoverflow.com/search?q=%s")
            ("StackExchange"     "https://stackexchange.com/search?q=%s")
            ("Github"            "https://github.com/search?ref=simplesearch&q=%s")
            ("Youtube"           "https://youtube.com/results?aq=f&oq=&search_query=%s")
            ("Wolfram alpha"     "https://wolframalpha.com/input/?i=%s")
            ("Wikipedia"         "https://wikipedia.org/search-redirect.php?language=en&go=Go&search=%s")
            ("MDN"               "https://developer.mozilla.org/en-US/search?q=%s")
            ("Internet archive"  "https://web.archive.org/web/*/%s")
            ("Sourcegraph"       "https://sourcegraph.com/search?q=context:global+%s&patternType=literal")))
  "An alist that maps online resources to either:

  1. A search url (needs on '%s' to substitute with an url encoded query),
  2. A non-interactive function that returns the search url in #1,
  3. An interactive command that does its own search for that provider.

Used by `spacemacs/lookup-online'.")

(defvar spacemacs/lookup-definition-functions
  '(spacemacs/dictionary-definition-backend-fn
    spacemacs/xref-definitions-backend-fn
    spacemacs/dumb-jump-backend-fn
    spacemacs/lookup-project-search-backend-fn
    spacemacs/evil-goto-definition-backend-fn))

(defvar spacemacs/lookup-implementations-functions '()
  "Function for `spacemacs/lookup-implementations-functions' to try. Stops at the first function to
return non-nil or change the current window/point.

If the argument is interactive (satisfies `commandp'), it is called with
`call-interactively' (with no arguments). Otherwise, it is called with one
argument: the identifier at point. See `spacemacs/set-lookup-handlers' about adding to
this list.")

(defvar spacemacs/lookup-type-definition-functions '())

(defvar spacemacs/lookup-references-functions
  '(spacemacs/lookup-thesaurus-definition-backend-fn
    spacemacs/lookup-xref-references-backend-fn
    spacemacs/lookup-project-search-backend-fn)
  "Functions for `+lookup/references' to try, before resorting to `dumb-jump'.
Stops at the first function to return non-nil or change the current
window/point.

If the argument is interactive (satisfies `commandp'), it is called with
`call-interactively' (with no arguments). Otherwise, it is called with one
argument: the identifier at point. See `set-lookup-handlers!' about adding to
this list.")

(defvar spacemacs/lookup-documentation-functions
  '(spacemacs/lookup-online-backend-fn
    spacemacs/evil-smart-doc-lookup-backend-fn)
  "Functions for `spacemacs/lookup-documentation' to try, before resorting to
`dumb-jump'. Stops at the first function to return non-nil or change the current
window/point.

If the argument is interactive (satisfies `commandp'), it is called with
`call-interactively' (with no arguments). Otherwise, it is called with one
argument: the identifier at point. See `spacemacs/set-lookup-handlers' about adding to
this list.")

(defvar spacemacs/lookup-file-functions
  '(+lookup-bug-reference-backend-fn
    +lookup-ffap-backend-fn)
  "Function for `spacemacs/lookup-file' to try, before restoring to `find-file-at-point'.
Stops at the first function to return non-nil or change the current
window/point.

If the argument is interactive (satisfies `commandp'), it is called with
`call-interactively' (with no arguments). Otherwise, it is called with one
argument: the identifier at point. See `set-lookup-handlers!' about adding to
this list.")

(defvar spacemacs/lookup-open-url-fn #'browse-url
  "Function to use to open search urls.")

(defvar spacemacs/lookup-dictionary-prefer-offline nil)
