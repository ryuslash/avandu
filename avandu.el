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

;; Avandu is an emacs mode that connects to a Tiny Tiny RSS instance
;; and allows you to read the feeds it has gathered locally.

;; The simplest way to install it is to use package.el:

;;     (package-install-file "/path/to/avandu.el")

;; For further information I would like to refer you to the avandu
;; info file.

;; Once installation is out of the way, it should get a value for
;; `avandu-tt-rss-api-url' (for example: http://tt-rss.org/demo/api/)
;; and then run `avandu-overview'.

;; Once in avandu:overview mode some key bindings will be:

;; - `r' :: Mark article at point as read.
;; - `o' :: Open article at point in a browser.  Uses `browse-url'.
;; - `n' :: Next article.
;; - `p' :: Previous article.

;;; Code:
(require 'auth-source)
(require 'json)
(require 'simple)
(require 'url)

(defconst avandu-entity-replacement-alist
  '(("hellip" . 8230)
    ("qout" . 34)
    ("amp" . 38)
    ("nbsp" . 32))
  "What to replace the part between & and ; of HTML entities with
  names.")

(defconst avandu-overview-mode-name "Avandu:Overview"
  "The default name for `avandu-overview-mode'.")

(defconst avandu-version 0
  "The current version of avandu.")

;; Customization
(defgroup avandu nil
  "Tiny Tiny RSS interface for emacs."
  :group 'applications)

;; Faces
(defface avandu-overview-excerpt
  '((t (:inherit shadow :slant italic)))
  "Face for article excerpts in avandu overview."
  :group 'avandu)

(defface avandu-overview-feed
  '((((class color)
      (background dark))
     (:foreground "white" :height 1.2 :bold t :family "sans"))
    (((class color)
      (background light))
     (:foreground "black" :height 1.2 :bold t :family "sans")))
  "Face for feed titles in avandu overview."
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

(defface avandu-overview-unread-article
  '((((class color)
      (background dark))
     (:foreground "orange3" :weight bold :family "sans"))
    (((class color)
      (background light))
     (:foregroung "red4" :weight bold :family "sans")))
  "Face for unread article titles in avandu overview."
  :group 'avandu)

;; User options
(defcustom avandu-tt-rss-api-url nil
  "URL of your Tiny Tiny RSS instance. For example:
  http://tt-rss.org/demo/api/"
  :group 'avandu
  :type 'string)

(defcustom avandu-user nil
  "Username of your Tiny Tiny RSS account."
  :group 'avandu
  :type 'string)

;; Variables
(defvar avandu--session-id nil
  "*internal* Session id for avandu.")

(defvar avandu-article-button-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map button-map)
    (define-key map "o" 'avandu-browse-article)
    (define-key map "r" 'avandu-mark-article-read)
    map)
  "Keymap for articles in `avandu-overview-mode'.")

(defvar avandu-feed-button-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map button-map)
    (define-key map "c" 'avandu-feed-catchup)
    map)
  "Keymap for feeds in `avandu-overview-mode'.")

(defvar avandu-overview-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "n" 'avandu-next-article)
    (define-key map "N" 'avandu-next-feed)
    (define-key map "p" 'avandu-previous-article)
    (define-key map "P" 'avandu-previous-feed)
    map)
  "Keymap for `avandu-overview-mode'.")

(defvar avandu-password nil
  "Password for your Tiny Tiny RSS account.")

;; Macros
(defmacro avandu--next-button-of-type (direction type)
  "Go DIRECTION and find the next button of a TYPE."
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

(defmacro avandu-getset (var prompt &optional passwdp)
  "Ask the user for, and then save, VAR with PROMPT. Use
`read-passwd' if PASSWDP and `read-string' otherwise."
  `(or ,var (setq ,var (,(if passwdp 'read-passwd 'read-string)
                        ,prompt))))

;; Internal
(defun avandu--check-login ()
  "Check to see if we're (still) logged in, try to login
otherwise. Signals an error if we're not logged in *and* login
was unsuccesful."
  (unless (or (and avandu--session-id (avandu-logged-in-p))
              (avandu-login))
    (avandu--clear-data)
    (error "Could not log in to tt-rss")))

(defun avandu--clean-text (text)
  "Go through TEXT and remove any trailing and leading whitespace
from it, then look for any HTML entities and either replace them
with their char value or with the value in
`avandu-entity-replacement-alist'."
  (with-temp-buffer
    (insert text)
    (while (re-search-forward
            "\\`[[:space:][:cntrl:]]+\\|[[:space:][:cntrl:]]+\\'" nil t)
      (replace-match ""))

    (goto-char (point-min))
    (while (search-forward "&" nil t)
      (let ((pos (point)))
        (save-excursion
          (when (search-forward ";" nil t)
            (let* ((sstring (buffer-substring pos (1- (point))))
                   (char-code
                    (if (= (char-after pos) ?#)
                        (unless (string-match-p "[^[:digit:]]"
                                                (substring sstring 1))
                          (string-to-number (substring sstring 1)))
                      (assoc sstring avandu-entity-replacement-alist))))
              (when char-code
                (delete-region (1- pos) (point))
                (insert-char (if (consp char-code)
                                 (cdr char-code)
                               char-code) 1)))))))

    (setq text (buffer-string)))
  text)

(defun avandu--clear-data ()
  "Clean up login data. This makes for a clean slate next time."
  (setq avandu-user nil
        avandu--session-id nil)

  (if (stringp avandu-password)
      (clear-string avandu-password)
    (setq avandu-password nil)))

(defun avandu--get-credentials ()
  "Get a username and password for Tiny Tiny RSS.  Try it first
with `auth-source-search' and then by asking the user."
  (let ((credentials (auth-source-search :max 1
                                         :host avandu-tt-rss-api-url
                                         :type 'netrc
                                         :require '(:user :secret)
                                         :user avandu-user)))
    (if credentials
        (setq avandu-user (plist-get (car credentials) :user)
              avandu-password (plist-get (car credentials) :secret))
      (avandu-getset avandu-user "Username: ")
      (avandu-getset avandu-password "Password: " t))))

(defun avandu--get-session-id (results)
  "Get the session id from RESULTS."
  (cdr (assq 'session_id (assq 'content results))))

(defun avandu--get-status-id (results)
  "Get the status id from RESULTS."
  (cdr (assq 'status results)))

(defun avandu--insert-article-excerpt (excerpt)
  "Insert the excerpt of an article."
  (let ((start-pos (point))
        end-pos
        (text (replace-regexp-in-string
               "[ \t\n]*$" "" (avandu--clean-text excerpt))))
    (unless (or (not text) (string= text ""))
      (insert
       (propertize
        text
        'face 'avandu-overview-excerpt))
      (indent-region start-pos (point) tab-width)
      (fill-region start-pos (point))
      (insert-char ?\n 1))))

(defun avandu--insert-article-title (id link title)
  "Insert a button with the label TITLE and store ID and LINK in
the article-id and link properties, respectively."
  (insert-button
   (replace-regexp-in-string "^[ \n\t]*\\|[ \n\t]*$" "" title)
   'face 'avandu-overview-unread-article
   'article-id id
   'link link
   'keymap avandu-article-button-map
   'action #'(lambda (button)
               (message "%s" (button-get button 'link))))
  (insert-char ?\n 1))

(defun avandu--insert-feed-title (id title)
  "Insert a button with the label TITLE and store ID in the
feed-id property."
  (unless (eq (point) (point-min)) (insert-char ?\n 1))
  (insert-button
   (replace-regexp-in-string "^[ \n\t]*\\|[ \n\t]*$" "" title)
   'face 'avandu-overview-feed
   'feed-id id
   'keymap avandu-feed-button-map
   'action #'(lambda (button)
               (message "%s" (button-label button))))
  (insert-char ?\n 2))

(defun avandu--password ()
  "Get the password.  This means either return `avandu-password'
as-is, or if it's a function return the result of that function."
  (if (functionp avandu-password)
      (funcall avandu-password)
    avandu-password))

(defun avandu--send-command (data)
  "Send a command with parameters DATA to tt-rss. The current
session-id is added to the request and then DATA is passed on to
`json-encode'.

DATA should be an association list with at least an OP value.
For example:

    (avandu--send-command '((op . \"isLoggedIn\")))

This function returns the result of `json-read' passed over the
returned json."
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

(defun avandu-categories (&optional unread)
  "Get the created categories.  If UNREAD is non-nil only get
categories with feeds with unread articles in them."
  (cdr (assq 'content
             (avandu--send-command
              `((op . "getCategories")
                ,@(when unread `((unread_only . ,unread))))))))

(defun avandu-feeds (&optional category unread limit offset)
  "Get the subscribed feeds.  If CATEGORY has been specified show
only the feeds in CATEGORY.  If UNREAD has been specified only
show feeds with unread articles in them.  Only fets LIMIT
number of feeds, starting from OFFSET.

There are a number of special category IDs:
  0 -- Uncategorized feeds
  -1 -- Special (e.g. Starred, Published, Archived, etc.) feeds
  -2 -- Labels
  -3 -- All feeds, excluding virtual feeds (e.g. Labels and such)
  -4 -- All feeds, including virtual feeds"
  (cdr (assq 'content
             (avandu--send-command
              `((op . "getFeeds")
                ,@(when category `((cat_id . ,category)))
                ,@(when unread `((unread_only . ,unread)))
                ,@(when limit `((limit . ,limit)))
                ,@(when offset `((offset . ,offset))))))))

(defun avandu-headlines (feed-id &rest plist)
  "Get a list of headlines from Tiny Tiny RSS from the feed
identified by FEED-ID.  Options about what to get can be
specified in the form of a property list PLIST.

If `:limit' is specified only get that many headlines, and if
`:skip' has been specified skip that many headlines first.

If `:is-cat' is non-nil, that means FEED-ID is actually the ID of
a category.

When `:show-excerpt' is non-nil, send back an excerpt along with
the headline and if `:show-content' is non-nil send along the
entire article.

`:view-mode' determines what type of headlines are sent back:

  all_articles -- All articles found are sent back.
  unread -- Only unread articles are sent back.
  adaptive -- ?
  marked -- ?
  updated -- ?

If `:include-attatchments' is non-nil, send along any files
enclosed in the articles.

If `:since-id' is specified, send only articles with a FEED-ID
greater than this.

There are some special feed IDs:
  -1 -- Starred feeds
  -2 -- Published feeds
  -3 -- Fresh feeds (less than X hours old)
  -4 -- All articles
  0 -- Archived articles
  IDs < -10 -- Labels"
  (let ((limit (plist-get plist :limit))
        (skip (plist-get plist :skip))
        (is-cat (plist-get plist :is-cat))
        (show-excerpt (plist-get plist :show-excerpt))
        (show-content (plist-get plist :show-content))
        (view-mode (plist-get plist :view-mode))
        (include-attachments (plist-get plist :include-attachments))
        (since-id (plist-get plist :since-id)))
    (cdr (assq 'content
               (avandu--send-command
                `((op . "getHeadlines")
                  (feed_id . ,feed-id)
                  ,@(when limit `((limit . ,limit)))
                  ,@(when skip `((skip . ,skip)))
                  ,@(when is-cat `((is_cat . ,is-cat)))
                  ,@(when show-excerpt `((show_excerpt . ,show-excerpt)))
                  ,@(when show-content `((show_content . ,show-content)))
                  ,@(when view-mode `((view_mode . ,view-mode)))
                  ,@(when include-attachments `((include_attachments . ,include-attachments)))
                  ,@(when since-id `((since_id . ,since-id)))))))))

(defun avandu-update-article (article-ids mode field &optional data)
  "Update the status of FIELD to MODE for the articles identified
by ARTICLE-IDS.

ARTICLE-IDS should either be a single integer or a
comma-separated list of integers.

MODE should be one of:
  0 -- Set to false
  1 -- Set to true
  2 -- Toggle

FIELD should be one of:
  0 -- Starred
  1 -- Published
  2 -- Unread
  3 -- Article Note

When updating FIELD 3 DATA functions as the note's contents."
  (avandu--send-command `((op . "updateArticle")
                          (article_ids . ,article-ids)
                          (mode . ,mode)
                          (field . ,field)
                          ,@(when data `((data . ,data))))))

;; Commands
(defun avandu-browse-article ()
  "Browse the current button's article url."
  (interactive)
  (let ((button (button-at (point))))
    (browse-url (button-get button 'link))
    (avandu-mark-article-read button)))

(defun avandu-feed-catchup ()
  "Send a request to tt-rss to \"Catch up\" with a feed.  This
  means that all the (unread) articles in a feed will be marked
  as read. After having completed this request the overview is
  reloaded."
  (interactive)
  (let* ((button (button-at (point)))
         (id (button-get button 'feed-id)))
    (avandu--send-command `((op . "catchupFeed")
                           (feed_id . ,id))))
  (revert-buffer))

(defun avandu-logged-in-p ()
  "Send a request to tt-rss to see if we're (still) logged
in. This function returns t if we are, or nil if we're not."
  (let* ((response (avandu--send-command '((op . "isLoggedIn"))))
         (result (cdr (assq 'status (assq 'content response)))))
    (if (eq result :json-false)
        nil
      result)))

(defun avandu-login ()
  "Send a request to log in to tt-rss. If `avandu-user' or
`avandu-password' have not been specified they will be asked for
and saved in memory. This function returns t on succes, nil
otherwise."
  (interactive)
  (unless (and avandu-user avandu-password)
    (avandu--get-credentials))

  (let ((result (avandu--send-command
                 `((op . "login")
                   (user . ,avandu-user)
                   (password . ,(avandu--password))))))
    (if (eq (avandu--get-status-id result) 0)
        (progn
          (setq avandu--session-id (avandu--get-session-id result))
          t)
      nil)))

(defun avandu-logout ()
  "Logout from Tiny Tiny RSS."
  (interactive)
  (avandu--send-command '((op . "logout")))
  (avandu--clear-data))

(defun avandu-mark-article-read (&optional button)
  "Send a request to tt-rss to mark an article as read.

BUTTON, if given, should be a button widget, as created by
`button-insert' and such, which contains FEED-ID. If BUTTON is
nil, it will be assumed that `point' is currently within the
bounds of a button."
  (interactive)
  (let* ((button (or button (button-at (point))))
         (id (button-get button 'article-id)))
    (avandu-update-article id 0 2)
    (button-put button 'face 'avandu-overview-read-article))
  (avandu-next-article))

(defun avandu-new-articles-count ()
  "Send a request to tt-rss for the total number of unread
feeds."
  (interactive)
  (avandu--check-login)
  (let* ((result (avandu--send-command '((op . "getUnread"))))
         (count (cdr (assq 'unread (assq 'content result)))))

    (when (called-interactively-p 'any)
      (message "There are %s unread articles" count))

    count))

(defun avandu-next-article ()
  "Search forward for the next article."
  (interactive)
  (avandu--next-button-of-type forward article))

(defun avandu-next-feed ()
  "Go forward and find the next feed."
  (interactive)
  (avandu--next-button-of-type forward feed))

(defun avandu-previous-article ()
  "Go backward and find the next article."
  (interactive)
  (avandu--next-button-of-type backward article))

(defun avandu-previous-feed ()
  "Go backward and find the next feed."
  (interactive)
  (avandu--next-button-of-type backward feed))

(defun avandu-tt-rss-api-level ()
  "Get the API level of your Tiny Tiny RSS instance."
  (interactive)
  (let ((level (cdr (assq 'level
                          (assq 'content
                                (avandu--send-command
                                 '((op . "getApiLevel"))))))))
    (when (called-interactively-p 'any)
      (message "API Level: %d" level))

    level))

(defun avandu-tt-rss-version ()
  "Get the version of your Tiny Tiny RSS instance."
  (interactive)
  (let ((version (cdr (assq 'version
                            (assq 'content
                                  (avandu--send-command
                                   '((op . "getVersion"))))))))
    (when (called-interactively-p 'any)
      (message "Tiny Tiny RSS Version: %s" version))

    version))

;; Overview
(define-derived-mode avandu-overview-mode special-mode
  avandu-overview-mode-name
  "Major mode fo the avandu overview screen.

This screen shows the articles categorized by feed as a list. It
doesn't sort the list, so you'll have to set that up in tt-rss.

\\{avandu-overview-map}
\\<avandu-overview-map>"
  (use-local-map avandu-overview-map)
  (set (make-local-variable 'revert-buffer-function)
       #'(lambda (ignore-auto noconfirm) (avandu-overview)))
  (setq mode-name (format "%s[%s]"
                          avandu-overview-mode-name
                          (avandu-new-articles-count))))

;;;###autoload
(defun avandu-overview ()
  "Request the headlines of unread articles and list them grouped
by feed."
  (interactive)
  (avandu--check-login)
  (let ((buffer (get-buffer-create "*avandu-overview*"))
        (result (avandu-headlines -4 :show-excerpt t :view-mode "unread"))
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
                 (cdr (assq 'title elt)))
                (avandu--insert-article-excerpt
                 (cdr (assq 'excerpt elt))))
            result)
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

;; (login user password)
;; (get-article article-id)
;; (get-config icons-dir icons-url daemon-is-running num-feeds)
;; (update-feed feed-id)
;; (get-pref pref-name)
;; (catchup-feed feed-id categoryp)
;; (get-counters output-mode)
;; (get-labels article-id)
;; (set-article-label article-ids label-id assingp)
