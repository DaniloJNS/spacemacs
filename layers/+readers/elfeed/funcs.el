;;; funcs.el --- Elfeed Layer functions File for Spacemacs
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


(defun spacemacs//elfeed-show-all ()
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-all"))

(defun spacemacs//elfeed-show-emacs ()
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-emacs"))

(defun spacemacs//elfeed-show-daily ()
  (interactive)
  (bookmark-maybe-load-default-file)
  (bookmark-jump "elfeed-daily"))

(defun spacemacs//elfeed-star ()
  "Apply starred to all selected entries"
  (interactive)
  (let* ((entries (elfeed-search-selected))
         (tag (intern "starred")))

    (cl-loop for entry in entries do (elfeed-tag entry tag))
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))

(defun spacemacs//elfeed-unstar ()
  "Remove starred tag from all selected entries."
  (interactive)
  (let* ((entries (elfeed-search-selected))
         (tag (intern "starred")))

    (cl-loop for entry in entries do (elfeed-untag entry tag))
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))


(defun spacemacs//concatenate-authors (authors-list)
  "Given AUTHORS-LIST, list of plists; return string of all authors
concatenated."
  (mapconcat
   (lambda (author) (plist-get author :name))
   authors-list ", "))

(defun spacemacs//elfeed-search-print-entry (entry)
  "Print ENTRY to the buffer."
  (let* ((date (elfeed-search-format-date (elfeed-entry-date entry)))
         (title (or (elfeed-meta entry :title)
                    (elfeed-entry-title entry) ""))
         (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
         (feed (elfeed-entry-feed entry))
         (feed-title
          (when feed
            (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
         (entry-authors (spacemacs//concatenate-authors
                         (elfeed-meta entry :authors)))
         (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
         (tags-str (mapconcat
                    (lambda (s) (propertize s 'face
                                            'elfeed-search-tag-face))
                    tags ","))
         (title-width (- (window-width) 10
                         elfeed-search-trailing-width))
         (title-column (elfeed-format-column
                        title (elfeed-clamp
                               elfeed-search-title-min-width
                               title-width
                               elfeed-search-title-max-width)
                        :left))
         (authors-width 135)
         (authors-column (elfeed-format-column
                          entry-authors (elfeed-clamp
                                         elfeed-search-title-min-width
                                         authors-width
                                         131)
                          :left))
         (score
          (elfeed-score-format-score
           (elfeed-score-scoring-get-score-from-entry entry))))



    (when feed-title
      (insert (propertize feed-title
                          'face 'elfeed-search-feed-face) " "))
    (when tags
      (insert (propertize (concat "(" tags-str ")") 'face 'elfeed-search-tag-face) " "))

    (when score
      (insert (propertize score
                          'face 'elfeed-search-feed-face) " "))

    (put-text-property (- (point) 1) (point) 'display "\n")

    (insert (propertize date 'face 'elfeed-search-date-face) " ")

    (insert (propertize title-column
                        'face title-faces 'kbd-help title) " ")

    (insert (propertize authors-column
                        'face 'elfeed-search-date-face))))

(setq elfeed-search-print-entry-function #'spacemacs//elfeed-search-print-entry)
;; (setq elfeed-search-print-entry-function #'elfeed-search-print-entry--default)
