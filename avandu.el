;;; avandu.el --- Gateway to Tiny Tiny RSS

;; Copyright (C) 2012 Tom Willemse <tom@ryuslash.org>

;; Author: Tom Willemse <tom@ryuslash.org>
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

;; Avandu is an Emacs mode that connects to a Tiny Tiny RSS
;; (http://tt-rss.org) instance and allows you to read the feeds it
;; has gathered.

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
(require 'view)

(declare-function w3m-region "w3m")
(declare-function w3m-minor-mode "w3m")

(defconst avandu-entity-replacement-alist
  '(("hellip" . 8230)
    ("qout" . 34)
    ("amp" . 38)
    ("nbsp" . 32))
  "What to replace certain HTML entities with.")

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

(defface avandu-article-title
  '((((class color)
      (background dark))
     (:foreground "orange3" :weight bold :family "sans"))
    (((class color)
      (background light))
     (:foreground "red4" :weight bold :family "sans")))
  "Face for titles in avandu article view."
  :group 'avandu)

(defface avandu-article-author
  '((t (:inherit shadow :slant italic :height 0.9)))
  "Face for the author's name in avandu article view."
  :group 'avandu)

;; User options
(defcustom avandu-article-render-function #'shr-render-region
  "A function to call that will render the content of an article."
  :group 'avandu
  :type 'function)

(defcustom avandu-tt-rss-api-url nil
  "URL of your Tiny Tiny RSS instance.

For example: http://tt-rss.org/demo/api/"
  :group 'avandu
  :type 'string)

(defcustom avandu-html2text-command nil
  "Shell command to call to change HTML to plain text."
  :group 'avandu
  :type 'string)

(defcustom avandu-user nil
  "Username of your Tiny Tiny RSS account."
  :group 'avandu
  :type 'string)

(defcustom avandu-next-button-fallback-function #'shr-next-link
  "The fallback function to use when `avandu-next-button' fails.

Your HTML renderer might not create links the same way avandu
does, as is the case with `shr'.  This will try the specified
function, if avandu can't find a next link, before giving up.

The specified value should be a function that can take 0
arguments."
  :group 'avandu
  :type 'function)

(defcustom avandu-previous-button-fallback-function #'shr-previous-link
  "The fallback function to use when `avandu-previous-button' fails.

Your HTML renderer might not create links the same way avandu
does, as is the case with `shr'.  This will try the specified
function, if avandu can't find a next link, before giving up.

The specified value should be a function that can take 0
arguments."
  :group 'avandu
  :type 'function)

;; Variables
(defvar avandu--session-id nil
  "*Internal* Session id for avandu.")

(defvar avandu-article-button-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map button-map)
    (define-key map "o" 'avandu-browse-article)
    (define-key map "r" #'(lambda ()
                            (interactive)
                            (let ((button (button-at (point))))
                              (avandu-mark-article-read
                               (button-get button 'article-id))
                              (avandu-ui-mark-article-read button))))
    (define-key map "u" #'(lambda ()
                            (interactive)
                            (let ((button (button-at (point))))
                              (avandu-mark-article-unread
                               (button-get button 'article-id))
                              (avandu-ui-mark-article-unread button))))
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

(defvar avandu-article-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "TAB") #'avandu-next-button)
    (define-key map (kbd "<backtab>") #'avandu-previous-button)
    map)
  "Keymap for `avandu-atricle-mode'.")

(defvar avandu-password nil
  "Password for your Tiny Tiny RSS account.")

;; Macros
(defmacro avandu--next-button-of-type (direction type)
  "Go DIRECTION and find the next button of a TYPE."
  (let ((prop (cl-case type
                (feed 'feed-id)
                (article 'article-id)
                (t (error "Invalid type"))))
        (next-point-function (cl-case direction
                               (forward 'point-min)
                               (backward 'point-max)
                               (t (error "Invalid direction"))))
        (next-button-function (cl-case direction
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
  "Ask the user for VAR with PROMPT.

After getting the answer from the user save the value.  Use
`read-passwd' if PASSWDP and `read-string' otherwise."
  `(or ,var (setq ,var (,(if passwdp 'read-passwd 'read-string)
                        ,prompt))))

(defmacro avu-prop (element property)
  "Look in ELEMENT for PROPERTY."
  `(cdr (assq (quote ,property) ,element)))

;; Internal
(defun avandu--check ()
  "Check to see if everything is ready to go."
  (avandu--check-setup)
  (avandu--check-login))

(defun avandu--check-login ()
  "Check to see if we're (still) logged in.

Try to login otherwise.  Signals an error if we're not logged in
*and* login was unsuccesful."
  (unless (or (and avandu--session-id (avandu-logged-in-p))
              (avandu-login))
    (avandu--clear-data)
    (error "Could not log in to tt-rss")))

(defun avandu--check-setup ()
  "Check to see if everything's been set-up correctly."
  (unless avandu-tt-rss-api-url
    (user-error "No URL has been specified in `avandu-tt-rss-api-url', please do so")))

(defun avandu--clean-text (text)
  "Go through TEXT and remove any trailing and leading whitespace.

Then look for any HTML entities and either replace them with
their char value or with the value in
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
  "Clean up login data.  Clean the slate for next time."
  (setq avandu-user nil
        avandu--session-id nil)

  (if (stringp avandu-password)
      (clear-string avandu-password)
    (setq avandu-password nil)))

(defun avandu--get-credentials ()
  "Get a username and password for Tiny Tiny RSS.

Try it first with `auth-source-search' and then by asking the
user."
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
  (avu-prop (assq 'content results) session_id))

(defun avandu--get-status-id (results)
  "Get the status id from RESULTS."
  (avu-prop results status))

(defun avandu--insert-article-excerpt (excerpt)
  "Insert EXCERPT of an article."
  (let ((start-pos (point))
        end-pos
        (text (avandu--oneline excerpt)))
    (unless (or (not text) (string= text ""))
      (insert
       (propertize
        text
        'face 'avandu-overview-excerpt))
      (indent-region start-pos (point) tab-width)
      (fill-region start-pos (point))
      (insert-char ?\n 1))))

(defun avandu--insert-article-title (id link title)
  "Insert a button.

Store ID and LINK in the `article-id' and `link' properties
respectively, use TITLE as the text for the button."
  (let ((pos (point)))
    (insert-button
     (avandu--oneline title)
     'face 'avandu-overview-unread-article
     'article-id id
     'link link
     'keymap avandu-article-button-map
     'action #'(lambda (button)
                 (avandu-view-article (button-get button 'article-id))))

    (fill-region pos (point))
    (insert-char ?\n 1)))

(defun avandu--insert-feed-title (id title)
  "Insert a button.

Store ID in the `feed-id' property and use TITLE as the text of
the button."
  (unless (eq (point) (point-min)) (insert-char ?\n 1))

  (let ((pos (point)))
    (insert-button
     (avandu--oneline title)
     'face 'avandu-overview-feed
     'feed-id id
     'keymap avandu-feed-button-map
     'action #'(lambda (button)
                 (message "%s" (button-label button))))
    (fill-region pos (point))
    (insert-char ?\n 2)))

(defun avandu--oneline (text)
  "Make a single line out of and clean up TEXT."
  (replace-regexp-in-string "[ \n\t]*$" "" (avandu--clean-text text)))

(defun avandu--password ()
  "Get the password.

This means either return `avandu-password' as-is, or if it's a
function return the result of that function."
  (if (functionp avandu-password)
      (funcall avandu-password)
    avandu-password))

(defun avandu--prep-params (data)
  "Prepare DATA to be sent to Tiny Tiny RSS."
  (json-encode (if avandu--session-id
                   (append `((sid . ,avandu--session-id))
                           data)
                 data)))

(defun avandu--send-command-async (data func)
  "Send a command with parameters DATA to tt-rss asynchronously.

The current session-id is added to the request and then DATA is
passed on to `json-encode'.

DATA should be an association list with at least an OP value.

FUNC should be a callback function as defined by
`url-retrieve'."
  (let* ((url-request-data (avandu--prep-params data))
         (url-request-method "POST"))
    (unless (url-retrieve avandu-tt-rss-api-url func)
      (message "Complete."))))

(defun avandu--send-command-sync (data &optional raw)
  "Send a command with parameters DATA to tt-rss.

The current session-id is added to the request and then DATA is
passed on to `json-encode'.

DATA should be an association list with at least an OP value.
For example:

    (avandu--send-command-sync '((op . \"isLoggedIn\")))

This function returns the content part of the result of
`json-read' passed over the returned json, unless RAW is non-nil,
in which case the result is returned as-is."
  (let* ((url-request-data (avandu--prep-params data))
         (url-request-method "POST")
         (buffer (url-retrieve-synchronously avandu-tt-rss-api-url))
         result)
    (with-current-buffer buffer
      (goto-char (point-min))
      (search-forward "\n\n")
      (setq result (json-read)))
    (kill-buffer buffer)
    (if (not raw)
        (avu-prop result content)
      result)))

(defun avandu-categories (&optional unread)
  "Get the created categories.

If UNREAD is non-nil only get categories with feeds with unread
articles in them."
  (let ((hash (make-hash-table :test 'equal)))
    (mapc (lambda (category)
            (setf (gethash (cdr (assq 'title category)) hash)
                  (cdr (assq 'id category))))
          (avandu--send-command-sync
           `((op . "getCategories")
             ,@(when unread `((unread_only . ,unread))))))
    hash))

(defun avandu-feeds (&optional category unread limit offset)
  "Get the subscribed feeds.

If CATEGORY has been specified show only the feeds in CATEGORY.
If UNREAD has been specified only show feeds with unread articles
in them.  Only fets LIMIT number of feeds, starting from OFFSET.

There are a number of special category IDs:
  0 -- Uncategorized feeds
  -1 -- Special (e.g. Starred, Published, Archived, etc.) feeds
  -2 -- Labels
  -3 -- All feeds, excluding virtual feeds (e.g. Labels and such)
  -4 -- All feeds, including virtual feeds"
  (avandu--send-command-sync
   `((op . "getFeeds")
     ,@(when category `((cat_id . ,category)))
     ,@(when unread `((unread_only . ,unread)))
     ,@(when limit `((limit . ,limit)))
     ,@(when offset `((offset . ,offset))))))

(defun avandu--all-feeds-hash ()
  "Get a hashtable of all feeds subsrcibed to in tt-rss."
  (let ((hash (make-hash-table :test 'equal)))
    (mapc (lambda (feed)
            (setf (gethash (cdr (assq 'title feed)) hash)
                  (cdr (assq 'id feed))))
          (avandu-feeds -3))
    hash))

(defun avandu-headlines (feed-id &rest plist)
  "Get a list of headlines from Tiny Tiny RSS.

Get the ones for the feed identified by FEED-ID.  Options about
what to get can be specified in the form of a property list
PLIST.

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
    (avandu--send-command-sync
     `((op . "getHeadlines")
       (feed_id . ,feed-id)
       ,@(when limit `((limit . ,limit)))
       ,@(when skip `((skip . ,skip)))
       ,@(when is-cat `((is_cat . ,is-cat)))
       ,@(when show-excerpt `((show_excerpt . ,show-excerpt)))
       ,@(when show-content `((show_content . ,show-content)))
       ,@(when view-mode `((view_mode . ,view-mode)))
       ,@(when include-attachments `((include_attachments
                                      . ,include-attachments)))
       ,@(when since-id `((since_id . ,since-id)))))))

(defun avandu-update-article (article-ids mode field &optional data)
  "Update the status of a field.

For each id in ARTICLE-IDS put MODE in FIELD.

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
  (avandu--send-command-async `((op . "updateArticle")
                                (article_ids . ,article-ids)
                                (mode . ,mode)
                                (field . ,field)
                                ,@(when data `((data . ,data))))
                              (lambda (status)
                                (message "Update done."))))

(defun avandu-get-article (article-ids)
  "Get one or more articles from Tiny Tiny RSS.

Filter the articles by ARTICLE-IDS, if you're using version 1.5.0
or higher this can also be a comma-separated list of ids."
  (avandu--send-command-sync `((op . "getArticle")
                               (article_id . ,article-ids))))

;; Commands
(defun avandu-browse-article ()
  "Browse the current button's article url."
  (interactive)
  (let ((button (button-at (point)))
        (message-truncate-lines t))
    (browse-url (button-get button 'link))
    (avandu-mark-article-read (button-get button 'article-id))
    (avandu-ui-mark-article-read button)
    (message "Opened: %s" (button-label button))))

(defun avandu-feed-catchup ()
  "Send a request to tt-rss to \"Catch up\" with a feed.

This means that all the (unread) articles in a feed will be
marked as read. After having completed this request the overview
is reloaded."
  (interactive)
  (let* ((button (button-at (point)))
         (id (button-get button 'feed-id)))
    (avandu--send-command-async `((op . "catchupFeed")
                                  (feed_id . ,id))
                                (lambda (status)
                                  (message "Catch-up complete."))))
  (revert-buffer))

(defun avandu-logged-in-p ()
  "Send a request to tt-rss to see if we're (still) logged in.

This function returns t if we are, or nil if we're not."
  (let* ((response (avandu--send-command-sync '((op . "isLoggedIn"))))
         (result (avu-prop response status)))
    (if (eq result :json-false)
        nil
      result)))

(defun avandu-login ()
  "Send a request to log in to tt-rss.

If `avandu-user' or `avandu-password' have not been specified
they will be asked for and saved in memory.  This function
returns t on succes, nil otherwise."
  (interactive)
  (unless (and avandu-user avandu-password)
    (avandu--get-credentials))

  (let ((result (avandu--send-command-sync
                 `((op . "login")
                   (user . ,avandu-user)
                   (password . ,(avandu--password))) t)))
    (if (eq (avandu--get-status-id result) 0)
        (progn
          (setq avandu--session-id (avandu--get-session-id result))
          t)
      nil)))

(defun avandu-logout ()
  "Logout from Tiny Tiny RSS."
  (interactive)
  (avandu--send-command-async '((op . "logout"))
                              (lambda (status)
                                (message "Logged out.")))
  (avandu--clear-data))

(defun avandu-mark-article-read (id)
  "Send a request to tt-rss to mark an article as read.

Update the article identified by ID."
  (interactive)
  (let* ((message-truncate-lines t))
    (avandu-update-article id 0 2)))

(defun avandu-mark-article-unread (id)
  "Send a request to tt-rss to mark an article as unread.

Update the article identified by ID."
  (interactive)
  (let* ((message-truncate-lines t))
    (avandu-update-article id 1 2)))

(defun avandu-ui-mark-article-read (&optional button)
  "Try to change the state of BUTTON to a read article button.

If BUTTON is nil, try to use a button at `point'."
  (let ((button (or button (button-at (point)))))
    (if button
        (progn
          (button-put button 'face 'avandu-overview-read-article)
          (avandu-next-article))
      (error "No button found"))))

(defun avandu-ui-mark-article-unread (&optional button)
  "Try to change the state of BUTTON to an unread article button.

If BUTTON is nil, try to use a button at `point'."
  (let ((button (or button (button-at (point)))))
    (if button
        (progn
          (button-put button 'face 'avandu-overview-unread-article)
          (avandu-next-article))
      (error "No button found"))))

(defun avandu-new-articles-count ()
  "Get the total number of unread feeds."
  (interactive)
  (avandu--check-login)
  (let* ((result (avandu--send-command-sync '((op . "getUnread"))))
         (count (avu-prop result unread)))

    (when (called-interactively-p 'any)
      (message "There are %s unread articles" count))

    count))

(defun avandu-next-article ()
  "Search forward for the next article."
  (interactive)
  (avandu--next-button-of-type forward article))

(defun avandu-next-button ()
  "Search forward for the next button."
  (interactive)
  (let ((button-overlay (next-button (point))))
    (if button-overlay
        (goto-char (overlay-start button-overlay))
      (funcall avandu-next-button-fallback-function))))

(defun avandu-next-feed ()
  "Go forward and find the next feed."
  (interactive)
  (avandu--next-button-of-type forward feed))

(defun avandu-previous-article ()
  "Go backward and find the next article."
  (interactive)
  (avandu--next-button-of-type backward article))

(defun avandu-previous-button ()
  "Go backward and find the next button."
  (interactive)
  (let ((button-overlay (previous-button (point))))
    (if button-overlay
        (goto-char (overlay-start button-overlay))
      (funcall avandu-previous-button-fallback-function))))

(defun avandu-previous-feed ()
  "Go backward and find the next feed."
  (interactive)
  (avandu--next-button-of-type backward feed))

(defun avandu-tt-rss-api-level ()
  "Get the API level of your Tiny Tiny RSS instance."
  (interactive)
  (let ((level (avu-prop (avandu--send-command-sync
                          '((op . "getApiLevel")))
                         level)))
    (when (called-interactively-p 'any)
      (message "API Level: %d" level))

    level))

(defun avandu-tt-rss-version ()
  "Get the version of your Tiny Tiny RSS instance."
  (interactive)
  (let ((version (avu-prop (avandu--send-command-sync
                            '((op . "getVersion")))
                           version)))
    (when (called-interactively-p 'any)
      (message "Tiny Tiny RSS Version: %s" version))

    version))

;;;###autoload
(defun avandu-subscribe-to-feed (url category)
  "Subscribe to the feed at URL optionally putting it in CATEGORY."
  (interactive (let ((categories (avandu-categories)))
                 (list (read-from-minibuffer "URL: ")
                       (gethash (completing-read "Category: "
                                                 categories nil t)
                                categories))))
  (let ((status (avu-prop (avu-prop (avandu--send-command-sync
                                     `((op . "subscribeToFeed")
                                       (feed_url . ,url)
                                       (category_id . ,category)))
                                    status)
                          code)))
    (if (= status 1)
        (message "Succesfully subscribed to feed")
      (message "Could not subscribe to feed"))))

;;;###autoload
(defun avandu-unsubscribe-from-feed (id)
  "Unsubscribe from ID."
  (interactive (let ((feeds (avandu--all-feeds-hash)))
                 (list (gethash (completing-read "Feed: " feeds nil t)
                                feeds))))
  (let ((status (avu-prop (avandu--send-command-sync
                           `((op . "unsubscribeFeed")
                             (feed_id . ,id))) status)))
    (if (string= status "OK")
        (message "Succesfully unsubscribed from feed")
      (message "Could not unsubscribe from feed"))))

(defun avandu-view-possibly-external (start end)
  "Maybe execute a command on the region between START and END.

If `avandu-html2text-command' has been specified use that on the
given region, otherwise just leave it alone."
  (when avandu-html2text-command
    (shell-command-on-region
     start end avandu-html2text-command t t)))

(defun avandu-view-w3m (start end)
  "Render the region between START and END with w3m."
  (when (require 'w3m nil t)
    (w3m-region start end)
    (w3m-minor-mode)))

;; Overview
(define-derived-mode avandu-overview-mode special-mode
  avandu-overview-mode-name
  "Major mode for the avandu overview screen.

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

(define-derived-mode avandu-article-mode special-mode "Avandu:Article"
  "Major mode for the avandu article screen.

This screen shows the contents of an article.

\\{avandu-article-mode-map}
\\<avandu-article-mode-map>")

(defsubst avandu--feed-id (alist)
  "Get a feed_id from ALIST."
  (cdr (assoc 'feed_id alist)))

(defun avandu--order-feed (feed1 feed2)
  "Return t if FEED1 should be sorted before FEED2."
  (string< (avandu--feed-id feed1) (avandu--feed-id feed2)))

;;;###autoload
(defun avandu-overview ()
  "Request the headlines of unread articles and list them.

The list is grouped and sorted by feed ID.  Sorting by feed ID is
meaningless, but it's easy."
  (interactive)
  (avandu--check)
  (let ((buffer (get-buffer-create "*avandu-overview*"))
        (result (sort (cl-coerce (avandu-headlines -4 :show-excerpt t
                                                   :view-mode "unread")
                                 'list)
                      #'avandu--order-feed))
        feed-id)
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (goto-char (point-min))
      (mapc #'(lambda (elt)
                (unless (equal feed-id (assq 'feed_id elt))
                  (avandu--insert-feed-title (avu-prop elt feed_id)
                                             (avu-prop elt feed_title)))
                (setq feed-id (assq 'feed_id elt))
                (avandu--insert-article-title (avu-prop elt id)
                                              (avu-prop elt link)
                                              (avu-prop elt title))
                (avandu--insert-article-excerpt (avu-prop elt excerpt)))
            result)
      (setq buffer-read-only t)
      (goto-char (point-min))
      (avandu-overview-mode))
    (switch-to-buffer buffer)))

(defun avandu-view-article (id)
  "Show a single article identified by ID in a new buffer."
  (interactive "nArticle id: ")
  (let* ((data (avandu-get-article id))
         (buffer (get-buffer-create "*avandu-article*"))
         (inhibit-read-only t)
         content-start
         content-end)
    (with-current-buffer buffer
      (erase-buffer)
      (mapc #'(lambda (item)
                (insert
                 (propertize (avu-prop item title)
                             'face 'avandu-article-title))
                (newline)
                (insert
                 (propertize (concat "by: " (avu-prop item author))
                             'face 'avandu-article-author))
                (insert " (")
                (insert-button
                 "Browse original"
                 'url (avu-prop item link)
                 'action #'(lambda (button)
                             (browse-url (button-get button 'url))))
                (insert ")")
                (newline)(newline)
                (setq content-start (point))
                (insert (avu-prop item content))
                (setq content-end (point))
                (newline)(newline))
            data)
      (setq buffer-read-only t)
      (goto-char (point-min))
      (avandu-article-mode))
    (avandu-mark-article-read id)
    (avandu-ui-mark-article-read)
    (switch-to-buffer buffer)
    (when avandu-article-render-function
      (funcall
       avandu-article-render-function content-start
       (min content-end (point-max))))
    (goto-char (point-min))))

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
