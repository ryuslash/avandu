;;; avandu.el --- Gateway to Tiny Tiny RSS

;; Copyright (C) 2012 Tom Willemsen <tom@ryuslash.org>

;; Author: Tom Willemsen <tom@ryuslash.org>
;; Created: Jul 22, 2012
;; Version: 0
;; Keywords: net

;; Permission to use, copy, modify, and distribute this software for any
;; purpose with or without fee is hereby granted, provided that the
;; above copyright notice and this permission notice appear in all
;; copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
;; WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
;; AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR
;; CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
;; OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
;; NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
;; CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

;;; Commentary:

;; Still coming...

;;; Code:
(require 'json)

(defgroup avandu nil
  "Tiny Tiny RSS for emacs cusomizations."
  :group 'applications)

(defface avandu-overview-feed
  '((((class color)
      (background dark))
     (:foreground "white" :height 1.2 :bold t :family "sans"))
    (((class color)
      (background light))
     (:foreground "black" :height 1.2 :bold t :family "sans")))
  "Face for feed titles in avandu overview."
  :group 'avandu)

(defface avandu-overview-unread-article
  '((((class color)
      (background dark))
     (:foreground "orange3" :weight bold :family "sans"))
    (((class color)
      (background light))
     (:foregroung "red4" :weight bold :family "sans")))
  "Face for unread article titles in avandu overview."
  :group 'avandu)

(defface avandu-overview-read-article
  '((((class color)
      (background dark))
     (:foreground "white" :weight normal :family "sans"))
    (((class color)
      (background light))
     (:foreground "black" :weight normal :family "sans")))
  "Face for read article titles in avandu overview."
  :group 'avandu)

(defvar avandu--session-id nil
  "*internal* Session id for avandu.")

(defvar avandu-tt-rss-api-url nil
  "URL of your Tiny Tiny RSS instance.")

(defvar avandu-user nil
  "Username of your Tiny Tiny RSS account.")

(defvar avandu-password nil
  "Password for your Tiny Tiny RSS account.")

(defvar avandu-feed-button-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map button-map)
    (define-key map "c" 'avandu-feed-catchup)
    map)
  "Keymap for buttons of articles in `avandu-overview-mode', uses
  `button-map' as its parent.")

(defvar avandu-article-button-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map button-map)
    (define-key map "o" 'avandu-browse-article)
    (define-key map "r" 'avandu-mark-article-read)
    map)
  "Keymap for buttons of articles in `avandu-overview-mode', uses
  `button-map' as its parent.")

(defvar avandu-overview-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "n" 'avandu-next-article)
    (define-key map "N" 'avandu-next-feed)
    (define-key map "p" 'avandu-previous-article)
    (define-key map "P" 'avandu-previous-feed)
    map)
  "Keymap for `avandu-overview-mode', uses `special-mode-map' as
  its parent.")

(define-derived-mode avandu-overview-mode special-mode "Avandu:Overview"
  "Major mode fo the avandu overview screen.

\\{avandu-overview-map}
\\<avandu-overview-map>"
  (use-local-map avandu-overview-map)
  (set (make-local-variable 'revert-buffer-function)
       #'(lambda (ignore-auto noconfirm) (avandu-list))))

(defun avandu--get-status-id (results)
  "Get the status id from RESULTS."
  (cdr (assq 'status results)))

(defun avandu--get-session-id (results)
  "Get the session id from RESULTS."
  (cdr (assq 'session_id (assq 'content results))))

(defun avandu--check-login ()
  (unless (or (and avandu--session-id (avandu-logged-in-p))
              (avandu-login))
    (error "Could not log in to tt-rss")))

(defmacro avandu-getset (var prompt &optional passwdp)
  `(or ,var (setq ,var (,(if passwdp 'read-passwd 'read-string)
                        ,prompt))))

(defun avandu-send-command (data)
  (let* ((url-request-data
          (json-encode
           (if avandu--session-id
               (append `((sid . ,avandu--session-id))
                       data)
             data)))
         (url-request-method "POST")
         (buffer (url-retrieve-synchronously avandu-tt-rss-api-url))
         result)
    (with-current-buffer buffer
      (goto-char (point-min))
      (search-forward "\n\n")
      (setq result (json-read)))
    (kill-buffer buffer)
    result))

(defun avandu-feed-catchup ()
  (interactive)
  (let* ((button (button-at (point)))
         (id (button-get button 'feed-id)))
    (avandu-send-command `((op . "catchupFeed")
                           (feed_id . ,id))))
  (revert-buffer))

(defun avandu-mark-article-read (&optional button)
  (interactive)
  (let* ((button (or button (button-at (point))))
         (id (button-get button 'article-id)))
    (avandu-send-command `((op . "updateArticle")
                           (article_ids . ,id)
                           (mode . 0)
                           (field . 2)))
    (button-put button 'face 'avandu-overview-read-article))
  (avandu-next-article))

(defun avandu-logged-in-p ()
  (let* ((response (avandu-send-command '((op . "isLoggedIn"))))
         (result (cdr (assq 'status (assq 'content response)))))
    (if (eq result :json-false)
        nil
      result)))

(defun avandu-login ()
  (interactive)
  (let ((result (avandu-send-command
                 `((op . "login")
                   (user . ,(avandu-getset avandu-user "Username: "))
                   (password
                    . ,(avandu-getset avandu-password "Password: " t))))))
    (if (eq (avandu--get-status-id result) 0)
        (progn
          (setq avandu--session-id (avandu--get-session-id result))
          t)
      nil)))

(defun avandu-new-articles-count ()
  (interactive)
  (avandu--check-login)
  (let ((result (avandu-send-command '((op . "getUnread")))))
    (message (cdr (assq 'unread (assq 'content result))))))

(defmacro avandu--next-button-of-type (direction type)
  (let ((prop (case type
                (feed 'feed-id)
                (article 'article-id)
                (t (error "Invalid type"))))
        (next-point-function (case direction
                               (forward 'point-min)
                               (backward 'point-max)
                               (t (error "Invalid direction"))))
        (next-button-function (case direction
                                (forward 'next-button)
                                (backward 'previous-button)
                                (t (error "Invalid direction")))))
    `(let ((pos (point))
           found-value)
       (while (not found-value)
         (let ((button (,next-button-function pos)))
           (unless button
             (setq pos (,next-point-function)
                   button (or (button-at pos)
                              (,next-button-function pos))))
           (setq found-value (button-get button ',prop)
                 pos (overlay-start button))))
       (goto-char pos))))

(defun avandu-next-article ()
  (interactive)
  (avandu--next-button-of-type forward article))

(defun avandu-previous-article ()
  (interactive)
  (avandu--next-button-of-type backward article))

(defun avandu-next-feed ()
  (interactive)
  (avandu--next-button-of-type forward feed))

(defun avandu-previous-feed ()
  (interactive)
  (avandu--next-button-of-type backward feed))

(defun avandu--insert-feed-title (id title)
  (unless (eq (point) (point-min)) (newline))
  (insert-button
   (replace-regexp-in-string "^[ \n\t]*\\|[ \n\t]*$" "" title)
   'face 'avandu-overview-feed
   'feed-id id
   'keymap avandu-feed-button-map
   'action #'(lambda (button)
               (message "%s" (button-label button))))
  (newline)
  (newline))

(defun avandu-browse-article ()
  "Browse the current button's article url."
  (interactive)
  (let ((button (button-at (point))))
    (browse-url (button-get button 'link))
    (avandu-mark-article-read button)))

(defun avandu--insert-article-title (id link title)
  (insert-button
   (replace-regexp-in-string "^[ \n\t]*\\|[ \n\t]*$" "" title)
   'face 'avandu-overview-unread-article
   'article-id id
   'link link
   'keymap avandu-article-button-map
   'action #'(lambda (button)
               (message "%s" (button-get button 'link))))
  (newline))

;;;###autoload
(defun avandu-list ()
  (interactive)
  (avandu--check-login)
  (let ((buffer (get-buffer-create "*avandu-overview*"))
        (result (avandu-send-command '((op . "getHeadlines")
                                       (feed_id . -4)
                                       (view_mode . "unread"))))
        feed-id)
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (goto-char (point-min))
      (mapc #'(lambda (elt)
                (unless (equal feed-id (assq 'feed_id elt))
                  (avandu--insert-feed-title
                   (cdr (assq 'feed_id elt))
                   (cdr (assq 'feed_title elt))))
                (setq feed-id (assq 'feed_id elt))
                (avandu--insert-article-title
                 (cdr (assq 'id elt))
                 (cdr (assq 'link elt))
                 (cdr (assq 'title elt))))
            (cdr (assq 'content result)))
      (setq buffer-read-only t)
      (goto-char (point-min))
      (avandu-overview-mode))
    (switch-to-buffer buffer)))

(provide 'avandu)

;;; avandu.el ends here
;; ((feed_title . "Identity at Mozilla")
;;  (labels . [])
;;  (tags . [""])
;;  (feed_id . "6")
;;  (link . "http://identity.mozilla.com/post/13619011637")
;;  (title . "BrowserID this week: better, faster, more secure.")
;;  (is_updated . :json-false)
;;  (updated . 1322795401)
;;  (published . :json-false)
;;  (marked . :json-false)
;;  (unread . t)
;;  (id . 109))
