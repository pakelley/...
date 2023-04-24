(use-package org-contacts
  :ensure nil
  :after (org doct)
  :custom
  (org-contacts-files '("~/.local/share/notes/contacts.org"))
  (org-capture-templates
   (append org-capture-templates
           (doct `(("Contact"
                    :keys "c"
                    :file "~/.local/share/notes/contacts.org"
                    ;; NOTE: Not using my property hook bc I haven't gotten it
                    ;;       to handle %^-style variable setting yet.
                    :template ("* %(org-contacts-template-name)"
                               ":PROPERTIES:"
                               ":EMAIL: %^{EMAIL}p"
                               ;; hey.com style email groups, used to categorize emails in notmuch
                               ":EMAIL-GROUP: %^{EMAIL-GROUP|screener|imbox|feed|paper-trail}"
                               ":BIRTHDAY: %^{BIRTHDAY|yyyy-mm-dd}p"
                               ":NOTE: %^{NOTE}p"
                               ":END:")
                    :kill-buffer t)))))
  :commands (+patch/get-email-group-for-email-sender +patch/set-email-group-for-email-sender)
  :defines (+patch/get-email-group-for-email-sender +patch/set-email-group-for-email-sender)
  :config
  (defun +patch/get-org-contact-by-email (email)
    ;; notably, this just returns the first match
    (car (org-contacts-filter nil nil `("EMAIL" . ,email))))

  (defun +patch/get-email-group-for-email-sender (email)
    (let* ((contact (+patch/get-org-contact-by-email email))
           (contact-info (nth 2 contact)))
      (cdr (assoc-string "EMAIL-GROUP" contact-info))))

  (defmacro +patch--from-contact-location (email &rest body)
    `(let* ((contact (+patch/get-org-contact-by-email email))
            (contact-marker (nth 1 contact)))
       (with-current-buffer (marker-buffer contact-marker)
         (save-excursion
           (goto-char contact-marker)
           ,@body))))

  (defmacro +patch--from-contact-file (email &rest body)
    `(let* ((contact (+patch/get-org-contact-by-email email))
            (contact-marker (nth 1 contact)))
       (with-current-buffer (find-file-noselect (car org-contacts-files))
         (save-excursion
           ,@body))))

  (defun +patch/add-org-contact (name email &optional email-group note bday)
    (+patch--from-contact-file email
     (end-of-buffer)
     (org-insert-heading nil nil t)  ;; force a top-level heading
     (insert name)
     (org-entry-put (point) "EMAIL" email)
     (org-entry-put (point) "EMAIL-GROUP" email-group)
     (org-entry-put (point) "NOTE" note)
     (org-entry-put (point) "BIRTHDAY" bday)))

  (defun +patch/set-email-group-for-email-sender (name email email-group)
    (if (+patch/get-org-contact-by-email email)
        (+patch--from-contact-location email
                                       (org-set-property "EMAIL-GROUP" email-group))
      (+patch/add-org-contact name email email-group))))

;; this seems to have been removed from notmuch, but I'll keep a copy here
(after! notmuch

  (general-define-key
   :keymaps 'notmuch-search-mode-map
   :states '(normal)
   "f" (cmd! (+patch-notmuch/move-thread-to-group "feed"))
   "F" (cmd! (+patch-notmuch/add-sender-to-group "feed"))
   "i" (cmd! (+patch-notmuch/move-thread-to-group "imbox"))
   "I" (cmd! (+patch-notmuch/add-sender-to-group "imbox"))
   "p" (cmd! (+patch-notmuch/move-thread-to-group "paper-trail"))
   "P" (cmd! (+patch-notmuch/add-sender-to-group "paper-trail"))
   "t" #'+patch-notmuch/move-thread-to-group
   "T" #'+patch-notmuch/add-sender-to-group
   "-" nil)
  (general-define-key
   :keymaps 'notmuch-search-mode-map
   :states '(normal)
   :prefix "-"
   "t" #'notmuch-search-filter-by-tag)

  (defgroup patch-notmuch nil
    "My personal notmuch config group"
    :group 'convenience
    :prefix "+patch-notmuch")
  ;; making this custom while I'm thinking about it, in case I ever get around
  ;; to breaking this into it's own package.
  (defcustom +patch-notmuch/tag-retroactively 'prompt
    "When adding a sender to a group, always set all existing emails"
    :group 'patch-notmuch
    :type '(choice (const :tag "Always" always)
            (const :tag "Never" never)
            (const :tag "Prompt" prompt)))
  (setq +patch-notmuch/tag-retroactively 'always)

  (defun +patch-notmuch--query-get-threads (search-terms)
    "Return a list of threads of messages matching SEARCH-TERMS.

A thread is a forest or list of trees. A tree is a two element
list where the first element is a message, and the second element
is a possibly empty forest of replies."
    (let ((args '("show" "--format=sexp" "--format-version=5")))
      (when notmuch-show-process-crypto
        (setq args (append args '("--decrypt=true"))))
      (setq args (append args search-terms))
      (apply #'notmuch-call-notmuch-sexp args)))

  (defun +patch-notmuch/get-thread (&optional thread-id)
    (let* ((thread-id (or thread-id (notmuch-search-find-thread-id)))
           (threads (+patch-notmuch--query-get-threads (list thread-id))))
      (caaar threads)))

  (defun +patch-notmuch/get-email-from-notmuch-search (&optional thread-id)
    (let* ((thread-id (or thread-id (notmuch-search-find-thread-id)))
           (thread (+patch-notmuch/get-thread thread-id))
           (headers (plist-get thread :headers)))
      (or
       (plist-get headers :Reply-To)
       (plist-get headers :From))))


  (defun +patch-notmuch/add-sender-to-group (group &optional thread-id tag-retroactively)
    "Add sender of thread from THREAD-ID to GROUP. From now on, all emails from this
sender will be tagged with GROUP.

If TAG-RETROACTIVELY is specified, all emails from this sender will be tagged
with GROUP.

Group seting is done by setting the EMAIL-GROUP property of the sender's contact
in org-contacts. If the contact does not exist, it will be created.
Tagging of future messages is done by the HeyFilter afew filter."
    (interactive (list (completing-read "Email group: " '("feed" "paper-trail" "imbox"))))
    ;; TODO prompt whether to set tags for existing messages from sender
    ;;      - could have a config value to determine this behavior
    ;;      - could (maybe additionally) determine via prefix args and/or function args
    (let* ((email (+patch-notmuch/get-email-from-notmuch-search thread-id))
           (name (notmuch-search-find-authors))  ;; used to set name of contact, if we have to make a new contact
           (tag-retroactively (or tag-retroactively
                                  +patch-notmuch/tag-retroactively
                                  (yes-or-no-p (format "Retroactively update tags for messages from %s?" email))))
           (tag-changes (cond ((equal group "feed") '("+feed" "-screener" "-paper-trail" "-imbox" "-unread"))
                              ((equal group "paper-trail") '("+paper-trail" "-screener" "-feed" "-imbox"))
                              ((equal group "imbox") '("+imbox" "-screener" "-paper-trail" "-feed"))
                              (t `(,(format "+%s" group) "-screener" "-paper-trail" "-feed" "-imbox")))))
      (+patch/set-email-group-for-email-sender name email group)
      (notmuch-search-tag tag-changes)
      (when tag-retroactively
        (notmuch-tag (format "from:%s" email) tag-changes))))

  (defun +patch-notmuch/move-thread-to-group (group &optional thread-id)
    "Tag thread from THREAD-ID with GROUP"
    (interactive (list (completing-read "Email group: " '("feed" "paper-trail" "imbox"))))
    (let* ((email (+patch-notmuch/get-email-from-notmuch-search thread-id))
           (tag-changes (cond ((equal group "feed") '("+feed" "-screener" "-paper-trail" "-imbox" "-unread"))
                              ((equal group "paper-trail") '("+paper-trail" "-screener" "-feed" "-imbox"))
                              ((equal group "imbox") '("+imbox" "-screener" "-paper-trail" "-feed"))
                              (t `(,(format "+%s" group) "-screener" "-paper-trail" "-feed" "-imbox")))))
      (notmuch-search-tag tag-changes))))

(after! notmuch
  ;; for some reason, the python notmuch client that gmi uses can't find my XDG notmuch config without this
  (setenv "NOTMUCH_CONFIG" (expand-file-name "~/.config/notmuch/default/config"))

  (setq send-mail-function #'smtpmail-send-it
        message-sendmail-f-is-evil t
        message-send-mail-function #'message-send-mail-with-sendmail)

  (defun +patch--get-my-email-address-in-message ()
    (car (mail-header-parse-address (message-field-value "From"))))

  (defun +patch--set-lieer-as-smtp-client ()
    (setq sendmail-program (executable-find "gmi")
          message-sendmail-extra-arguments '("send" "--quiet" "--read-recipients" "--path" "~/.local/share/mail/account.kelleys-gmail")))

  (defun +patch--set-msmtp-as-smtp-client ()
    (setq sendmail-program (executable-find "msmtp")
          message-sendmail-extra-arguments '("--read-envelope-from")))

  ;; TODO find elegant way to use account-specific settings
  (defun +patch--set-smtp-client ()
    (let ((email-address (+patch--get-my-email-address-in-message)))
      (cond ((string-match-p email-address "patrick@the-kelleys.com")
             (+patch--set-lieer-as-smtp-client)
             (setq org-msg-signature plain-org-msg-signature))
            ((string-match-p email-address "pakelley@pm.me")
             (+patch--set-msmtp-as-smtp-client)
             (setq org-msg-signature plain-org-msg-signature))
            ((string-match-p email-address "patrick@heartex.com")
             (+patch--set-lieer-as-smtp-client)
             (setq org-msg-signature heartex-org-msg-signature))
            ((t) (message (format "Could not find smtp client for email address: %s" email-address))))))

  (add-hook 'notmuch-mua-send-hook #'+patch--set-smtp-client))

(use-package! org-msg
  :after notmuch
  :custom
  (org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil tex:dvipng")
  (org-msg-startup "hidestars indent inlineimages")
  (org-msg-greeting-name-limit 3)
  (org-msg-default-alternatives '((new . (utf-8 html))
                                  (reply-to-text . (utf-8))
                                  (reply-to-html . (utf-8 html))))
  (org-msg-convert-citation t)
  ;; The default attachment matcher gives too many false positives,
  ;; it's better to be more conservative. See https://regex101.com/r/EtaiSP/4.
  (org-msg-attached-file-reference
   "see[ \t\n]\\(?:the[ \t\n]\\)?\\(?:\\w+[ \t\n]\\)\\{0,3\\}\\(?:attached\\|enclosed\\)\\|\
(\\(?:attached\\|enclosed\\))\\|\
\\(?:attached\\|enclosed\\)[ \t\n]\\(?:for\\|is\\)[ \t\n]")
  (heartex-org-msg-signature (let* ((fname "~/.config/doom/modules/email/notmuch-patch/signature.html")
                            (fpath (expand-file-name fname))
                            (signature-html (with-current-buffer (find-file-noselect fpath t)
                                              (buffer-string))))
                       (format "\n\n#+BEGIN_EXPORT html\n%s\n#+END_EXPORT" signature-html)))
  (plain-org-msg-signature "Best,\nPatrick")
  :config
  (org-msg-mode)
  (org-msg-mode-notmuch)
  ;; (defun my-org-msg-composition-parameters (orig-fun &rest args)
  ;;   "Tweak my greeting message and my signature when replying as
  ;;  plain/text only."
  ;;   (let* ((res (apply orig-fun args))
  ;;          ())
  ;;     (when (equal (cadr args) '(text))
  ;;   (setf (alist-get 'greeting-fmt res) "\n")
  ;;   (setf (alist-get 'signature res)
  ;;         (replace-regexp-in-string "\\([\*/]\\|\nRegards,\n\n\\)" ""
  ;;                   org-msg-signature)))
  ;;     res))
  ;; (advice-add 'org-msg-composition-parameters
  ;;         :around #'my-org-msg-composition-parameters)
  )

(after! notmuch
  (defun notmuch-agenda-event-time (event zone-map property)
    "Given an EVENT and a ZONE-MAP, turn the icalendar timestamp
  for PROPERTY into an emacs internal time representation"
    (let* ((timestamp (icalendar--get-event-property event property))
           (zone (icalendar--find-time-zone (icalendar--get-event-property-attributes event property)
                                            zone-map)))
      (icalendar--decode-isodatetime timestamp nil zone)))

  (defun +patch/relative-date-plain-english-description (now comparison-ts)
    (let* ((now-encoded (apply 'encode-time now))
           (comparison-ts-encoded (apply 'encode-time comparison-ts))
           (today (nth 3 now))
           (comparison-day (nth 3 comparison-ts))
           (seconds-until-event (float-time (time-subtract comparison-ts-encoded now-encoded)))
           (days-until-event (/ seconds-until-event 86400))
           (event-in-past (< days-until-event 0))
           (days-away-from-event (abs days-until-event))
           (rem-days (% (ceiling days-away-from-event) 7))
           (weeks-away-from-event (floor (/ days-away-from-event 7))))
      (cond
       ((and (< days-away-from-event 2)
             (= today comparison-day))
        "today")
       ((and (< days-away-from-event 2)
             (= (abs (- today comparison-day)) 1 ))
        (if event-in-past "yesterday" "tomorrow"))
       ((< days-away-from-event 8)
        (format "%d days%s" (ceiling days-away-from-event) (if event-in-past " ago" "")))
       (t (format "%d week%s%s%s"
                  weeks-away-from-event
                  (if (= 1 weeks-away-from-event) "" "s")
                  (if (zerop rem-days) ""
                    (format " %d days" rem-days))
                  (if event-in-past " ago" ""))))))

  ;; TODO rewrite this (and function above) using ts library
  (defun +patch/notmuch-agenda-friendly-date (dtstart)
    (let* ((now (decode-time (current-time)))
           (start-time (format-time-string "%a, %d %b %H:%M" (apply 'encode-time dtstart)))
           (rel-date (+patch/relative-date-plain-english-description now dtstart)))
      (concat start-time " (" rel-date ")")))

  (defun notmuch-agenda-insert-agenda (event zone-map)
    (require 'org)
    (let* ((dtstart (notmuch-agenda-event-time event zone-map 'DTSTART))
           (wins (current-window-configuration))
           (org-agenda-sticky nil)
           (inhibit-redisplay t)
           (year (nth 5 dtstart))
           (month (nth 4 dtstart))
           (day (nth 3 dtstart))

           (org-agenda-custom-commands '(("q" "Mail agenda" ((agenda ""))))))
      (cl-progv
          ;; `,@(-unzip (org-make-parameter-alist
          ;;          `(org-agenda-span 'day
          ;;            org-agenda-start-day ,(format "%04d-%02d-%02d" 2022 04 20)
          ;;            org-agenda-use-time-grid nil
          ;;            org-agenda-remove-tags t
          ;;            org-agenda-window-setup 'nope)))
          '(org-agenda-span
            org-agenda-start-day
            org-agenda-use-time-grid
            org-agenda-remove-tags
            org-agenda-window-setup)
          (list 'day
                (format "%04d-%02d-%02d" year month day)
                nil
                t
                'nope)
        (progn
          (save-excursion
            (org-agenda nil "q")
            (org-agenda-redo)
            (setq org-agenda-mail-buffer (current-buffer)))
          (set-window-configuration wins)
          (let ((p (point))
                pa)
            ;; copy text
            (insert-buffer-substring org-agenda-mail-buffer)

            ;; copy markers
            (save-restriction
              (narrow-to-region p (point))
              (let ((org-marker-regions
                     (with-current-buffer
                         org-agenda-mail-buffer
                       (setq pa (point-min))
                       (gnus-find-text-property-region (point-min) (point-max) 'org-marker))))
                (cl-loop for marker in org-marker-regions
                         do
                         (add-text-properties
                          (+ p (- (car marker) pa)) (+ p (- (cadr marker) pa))
                          `(org-marker
                            ,(copy-marker (get-text-property (car marker) 'org-marker org-agenda-mail-buffer))))

                         (set-marker (car marker) nil)
                         (set-marker (cadr marker) nil))))

            ;; copy faces via font-lock-face
            (save-restriction
              (narrow-to-region p (point))
              (let ((face-regions (gnus-find-text-property-region (point-min) (point-max) 'face)))
                (cl-loop for range in face-regions
                         do
                         (let ((face (get-text-property (car range) 'face)))
                           (add-text-properties
                            (car range) (cadr range)
                            `(font-lock-face ,face)))


                         (set-marker (car range) nil)
                         (set-marker (cadr range) nil))))

            (kill-buffer org-agenda-mail-buffer)
            (put-text-property p (point) 'keymap
                               org-agenda-keymap)))
        )))

  (defun notmuch-agenda-insert-summary (event zone-map)
    (let* ((summary (icalendar--get-event-property event 'SUMMARY))
           (comment (icalendar--get-event-property event 'COMMENT))
           (location (icalendar--get-event-property event 'LOCATION))
           (organizer (icalendar--get-event-property event 'ORGANIZER))
           (attendees (icalendar--get-event-properties event 'ATTENDEE))
           (summary (when summary (icalendar--convert-string-for-import summary)))
           (comment (when comment (icalendar--convert-string-for-import comment)))

           (dtstart (notmuch-agenda-event-time event zone-map 'DTSTART))
           (dtend (notmuch-agenda-event-time event zone-map 'DTEND))
           (rrule (icalendar--get-event-property event 'RRULE))
           (rdate (icalendar--get-event-property event 'RDATE))
           (duration (icalendar--get-event-property event 'DURATION))
           (description (icalendar--get-event-property event 'DESCRIPTION))

           (friendly-start (+patch/notmuch-agenda-friendly-date dtstart)))

      (when summary (insert (propertize summary 'face '(:underline t :height 1.5)) "\n"))

      (when (or rrule rdate) (insert (format "RRULE: %s %s\n" rrule rdate)))

      (when friendly-start
        (insert (propertize "Start: " 'face 'bold))
        (insert friendly-start "\n"))

      (when comment (insert (propertize "Comment: " 'face 'bold)
                            comment"\n"))

      (when location (insert (propertize "Location: " 'face 'bold)
                             location"\n"))
      (when organizer (insert (propertize "Organizer: " 'face 'bold)
                              (replace-regexp-in-string
                               "^mailto: *" ""
                               organizer)"\n"))
      (when attendees (insert (propertize "Attending: " 'face 'bold))
            (while attendees
              (insert (replace-regexp-in-string
                       "^mailto: *" ""
                       (car attendees)))
              (when (cdr attendees) (insert ", "))
              (setq attendees (cdr attendees)))
            (insert "\n"))

      ;; (when description
      ;;   (insert (read (format "\"%s\"" description))))

      (insert "\n")
      ))

  (defun notmuch-agenda-insert-part (msg part content-type nth depth button)
    (let (icalendar-element)
      (with-temp-buffer
        ;; Get the icalendar text and stick it in a temp buffer
        (insert (notmuch-get-bodypart-text msg part notmuch-show-process-crypto))
        ;; Transform CRLF into LF
        (goto-char (point-min))
        (while (re-search-forward "\r\n" nil t) (replace-match "\n" nil nil))
        ;; Unfold the icalendar text so it can be parsed
        (set-buffer (icalendar--get-unfolded-buffer (current-buffer)))
        ;; Go to the first VCALENDAR object in the result
        (goto-char (point-min))
        (when (re-search-forward "^BEGIN:VCALENDAR\\s-*$")
          (beginning-of-line)
          (setq icalendar-element (icalendar--read-element nil nil)))
        ;; Dispose of the junk buffer produced by icalendar--get-unfolded-buffer
        (kill-buffer (current-buffer)))

      (when icalendar-element
        (let* ((events (icalendar--all-events icalendar-element))
               (zone-map (icalendar--convert-all-timezones icalendar-element)))
          (insert "#+BEGIN_EXAMPLE\n")
          (dolist (event events)
            ;; insert event description string
            (notmuch-agenda-insert-summary event zone-map)
            (notmuch-agenda-insert-agenda event zone-map)
            (insert-button "[ Update agenda ]"
                           :type 'notmuch-show-part-button-type
                           'action 'notmuch-agenda-do-capture
                           'calendar-event event))
          (insert "\n#+END_EXAMPLE\n")
          t))))

  (defun +patch/notmuch-agenda-get-start ()
    "Return the point of the beginning of the message body."
    (save-excursion
      (message-goto-body)
      (search-forward "#+BEGIN_EXAMPLE" nil t)
      (line-beginning-position)))

  (defun +patch/notmuch-agenda-get-end ()
    "Return the point of the beginning of the message body."
    (save-excursion
      (message-goto-body)
      (search-forward "#+END_EXAMPLE" nil t)
      (line-end-position)))

  (fset 'notmuch-show-insert-part-text/calendar #'notmuch-agenda-insert-part))

(after! notmuch
  (defun notmuch-agenda-datetime-as-iso (datetime)
    "Convert a date retrieved via `icalendar--get-event-property' to ISO format."
    (if datetime
        (format "%04d-%02d-%02d"
                (nth 5 datetime)                  ; Year
                (nth 4 datetime)                  ; Month
                (nth 3 datetime))))



  (defun +patch/notmuch-agenda-org-repeater (rrule)
    (if rrule
        (let* ((rrule-parts (split-string rrule ";" t "\\s-"))
               (parts (mapcar (lambda (p)
                                (let ((parts (split-string p "=")))
                                  (cons (intern (car parts))
                                        (cadr parts))))
                              rrule-parts))
               (freq (alist-get 'FREQ parts))
               (interval (string-to-number (alist-get 'INTERVAL parts "1"))))
          (and freq interval
               (cond
                ((string= freq "DAILY") (format " +%dd" interval))
                ((string= freq "WEEKLY") (format " +%dw" interval))
                ((string= freq "MONTHLY") (format " +%dm" interval))
                ((string= freq "YEARLY") (format " +%dy" interval)))))
      ""))

  (defun notmuch-agenda-org-date (dtstart-dec dtend-dec rrule rdate duration)
    (let* ((start-d (notmuch-agenda-datetime-as-iso dtstart-dec))
           (start-t (icalendar--datetime-to-colontime dtstart-dec))

           end-d end-t

           (repeater (+patch/notmuch-agenda-org-repeater rrule)))

      (setq end-d (if dtend-dec
                      (notmuch-agenda-datetime-as-iso dtend-dec)
                    start-d))

      (setq end-t (if dtend-dec
                      (icalendar--datetime-to-colontime dtend-dec)
                    start-t))

      (if (equal start-d end-d)
          (format "<%s %s-%s%s>" start-d start-t end-t repeater)
        (format "<%s %s>--<%s %s>" start-d start-t end-d end-t))))

  (defvar notmuch-agenda-capture-targets
    `(( ,(rx "tom.hinton@cse.org.uk")
        file "~/notes/agenda/work.org")
      ( ""
        file "~/notes/agenda/calendar.org")))

  (defvar notmuch-agenda-capture-template
    ;; TODO insert also link to email
    "* %:event-summary
:PROPERTIES:
:LOCATION: %:event-location
:SEQUENCE: %:event-sequence
:ORGANIZER: [[%:event-organizer]]
:ID: %:event-uid
:END:
%:event-timestamp
%:event-comment
%:event-description
%a
%?")

  (defvar notmuch-agenda-capturing-event nil)
  (defvar notmuch-agenda-capturing-subject-line nil)
  (defvar notmuch-agenda-capturing-message-id nil)

  (defun notmuch-agenda-store-link ()
    (when notmuch-agenda-capturing-event
      (let ((event notmuch-agenda-capturing-event)
            (zone-map (icalendar--convert-all-timezones (list event)))
            (props (mapcan
                    (lambda (prop)
                      (let* ((val (icalendar--get-event-property event prop))
                             (val (and val (icalendar--convert-string-for-import val))))
                        (list
                         (intern (concat ":event-" (downcase (symbol-name prop))))
                         (or val ""))))

                    (list 'LOCATION 'SEQUENCE 'UID 'SUMMARY 'COMMENT 'ORGANIZER 'DESCRIPTION))))
        (apply 'org-store-link-props
               :type "event"
               :link (format "nm:%s" notmuch-agenda-capturing-message-id)
               :description (format "✉ %s" notmuch-agenda-capturing-subject-line)
               :event-timestamp (notmuch-agenda-org-date
                                 (notmuch-agenda-event-time event zone-map 'DTSTART)
                                 (notmuch-agenda-event-time event zone-map 'DTEND)
                                 (icalendar--get-event-property event 'RRULE)
                                 (icalendar--get-event-property event 'RDATE)
                                 (icalendar--get-event-property event 'DURATION))
               props))
      t))

  (defun notmuch-agenda-org-capture-or-update (event)
    (require 'org-id)
    (require 'org-capture)

    (let ((existing-event (org-id-find (icalendar--get-event-property event 'UID) t)))
      (if existing-event
          (let ((use-dialog-box nil)
                (existing-sequence
                 (org-entry-get existing-event "SEQUENCE")))
            (with-current-buffer
                (pop-to-buffer (marker-buffer existing-event))
              (goto-char existing-event)
              (outline-hide-sublevels 1)
              (outline-show-entry)
              (org-reveal)
              (if (>= (string-to-number existing-sequence)
                      (string-to-number (icalendar--get-event-property event 'SEQUENCE)))
                  (message "Event is already in calendar")
                (when (y-or-n-p "Update event?")
                  (org-entry-put nil "ID" nil)
                  (org-id-update-id-locations (list buffer-file-name))
                  (org-archive-subtree)
                  (notmuch-agenda-org-capture-or-update event))))

            (set-marker existing-event nil nil))

        (let* ((notmuch-agenda-capturing-subject-line
                (notmuch-show-get-subject))

               (notmuch-agenda-capturing-message-id
                (notmuch-show-get-message-id))

               (notmuch-agenda-capturing-event event)

               (org-link-parameters
                '(("nope" :store notmuch-agenda-store-link)))

               (org-overriding-default-time
                (apply 'encode-time
                       (notmuch-agenda-event-time event
                                                  (icalendar--convert-all-timezones (list event))
                                                  'DTSTART)))

               (org-capture-templates
                `(("l" "Capture an event from email invitation"
                   entry
                   ,notmuch-agenda-capture-target
                   ,notmuch-agenda-capture-template))))
          (org-capture t "l")))))

  (defun notmuch-agenda-do-capture (event)
    (let ((calendar-event (plist-get (overlay-properties event) 'calendar-event))
          (notmuch-agenda-capture-target
           (let ((addr (notmuch-show-get-to)))
             (cl-loop
              for tgt in notmuch-agenda-capture-targets
              when (string-match-p (car tgt) addr)
              return (cdr tgt)
              )))
          )
      (notmuch-agenda-org-capture-or-update calendar-event)))

  (defun notmuch-agenda-reply-advice (o &rest args)
    ;; look for any text/calendar parts
    (require 'cl)
    (let* ((responded (cl-intersection (notmuch-show-get-tags)
                                       '("accepted" "declined" "tentative")
                                       :test 'string=
                                       ))

           requires-response

           response

           (query (car args))
           (original (unless responded
                       (notmuch-call-notmuch-sexp
                        "reply" "--format=sexp" "--format-version=4" query)))
           (body (unless responded
                   (plist-get (plist-get original :original)
                              :body))))
      (while body
        (let ((head (car body)))
          (setq body (cdr body))
          (let ((content-type (plist-get head :content-type)))
            (cond
             ((or (string= content-type "multipart/alternative")
                  (string= content-type "multipart/mixed"))
              (setq body (append body (plist-get head :content))))
             ((and (string= content-type "text/calendar")
                   (string-match-p "^METHOD:REQUEST$" (plist-get head :content)))
              (setq requires-response (plist-get head :content)
                    body nil))))))

      (when requires-response
        (setq response (completing-read "Event invitation: "
                                        '("Accepted"
                                          "Declined"
                                          "Tentative"
                                          "Ignore")
                                        nil t)))


      (when (and response (not (string= "Ignore" response)))

        (notmuch-show-tag-message (concat "+" (downcase response))))

      (apply o args)

      (when (and requires-response
                 response
                 (not (string= response "Ignore")))
        (require 'ox-icalendar)
        ;; (require 'imip)

        (make-variable-buffer-local 'message-syntax-checks)
        (push '(illegible-text . disabled) message-syntax-checks)
        (delete-region (+patch/notmuch-agenda-get-start) (+patch/notmuch-agenda-get-end))

        (save-excursion
          (goto-char (point-max))
          (save-excursion
            (mml-insert-part "text/calendar; method=REPLY")
            (insert
             (org-icalendar-fold-string
              (with-temp-buffer
                (insert requires-response)
                (goto-char (point-min))
                (with-current-buffer
                    (icalendar--get-unfolded-buffer (current-buffer))
                  (goto-char (point-min))
                  (setq requires-response (icalendar--read-element nil nil))
                  (kill-buffer))
                (erase-buffer)

                (imip-write-element
                 (imip-respond (car requires-response)
                               '("patrick@the-kelleys.com" "the-kelleys.com")
                               (upcase response)))

                (buffer-string)
                ;; (replace-string "" "")
                )))))
        ;; NOTE have to do this manually, bc message-change-subject appends "was: old-subject" unconditionally
        (let ((new-subject (format "%s: %s"
                                   response
                                   (message-strip-subject-re (message-fetch-field "Subject")))))
          (save-excursion
            (message-goto-subject)
            (message-delete-line)
            (insert (concat "Subject: " new-subject "\n")))))))

  (advice-add 'notmuch-mua-reply :around 'notmuch-agenda-reply-advice))

(after! notmuch
  ;; provides icalendar message-based interoperability protocol
  ;; like rfc6047 but probably full of bugs

  (require 'icalendar)

  (defun imip-respond (invitation addresses response-string)
    "Given the icalendar object for invitation, produce a new one which responds appropriately."
    ;; According to RFC5546 p25, we can send a reply that contains just:
    ;; - method (vevent attendee dtstamp organizer uid sequence)+
    ;; however, outlook doesn't support this because it's stupid
    (let* ((invitation-contents (nth 3 invitation))
           (address-re (rx-to-string `(| ,@addresses)))
           (dtstamp (format-time-string "%Y%m%dT%H%M%SZ" nil t))
           events)

      (dolist (item invitation-contents)
        (cl-case (car item)
          (VTIMEZONE
           (push item events))

          (VEVENT
           (let* ((event-things (nth 2 item))
                  (organizer (assq 'ORGANIZER event-things))
                  (sequence (assq 'SEQUENCE event-things))
                  (uid (assq 'UID event-things))
                  attendees
                  misc)
             (dolist (thing event-things)
               (cl-case (car thing)
                 (ATTENDEE
                  (when (string-match-p address-re (nth 2 thing))
                    (push `(ATTENDEE (PARTSTAT ,response-string) ,(nth 2 thing)) attendees)))
                 ((DTSTART DTEND)
                  (push thing misc))))

             (push `(VEVENT nil (,organizer
                                 ,sequence
                                 ,uid
                                 (DTSTAMP nil ,dtstamp)
                                 ,@attendees
                                 ,@misc))
                   events))
           )))

      ;; (message (format "%s" events))
      `(VCALENDAR nil
        ((METHOD nil "REPLY")
         (PRODID nil "Emacs")
         ;; (CALSCALE nil "GREGORIAN")
         (VERSION nil "2.0"))
        ,events)))


  (defun imip-write-element (icalendar)
    "This is the inverse of icalendar--read-element from icalendar.el.
  It doesn't do stupid icalendar wrappning, nor does it put in CRLFs"

    (cond
     ((symbolp (car icalendar))
      (let ((element-name (nth 0 icalendar))
            (element-attrs (nth 1 icalendar))
            (element-properties (nth 2 icalendar))
            (element-children (nth 3 icalendar)))

        (insert (format "BEGIN:%s" element-name))
        (while element-attrs
          (insert ";")
          (insert (format "%s=%s" (car element-attrs) (cadr element-attrs)))
          (setq element-attrs (cddr element-attrs)))
        (insert "\n")
        (dolist (prop element-properties)
          (let ((prop-name (nth 0 prop))
                (prop-attrs (nth 1 prop)) ;; WHY?
                (prop-val (nth 2 prop)))
            (insert (format "%s" prop-name))
            (while prop-attrs
              (insert ";")
              (insert (format "%s=%s" (car prop-attrs) (cadr prop-attrs)))
              (setq prop-attrs (cddr prop-attrs)))
            (insert (format ":%s\n" prop-val))))
        (dolist (child element-children)
          (imip-write-element child))
        (insert (format "END:%s\n" element-name))))
     ((listp (car icalendar))
      (dolist (sub-element icalendar)
        (imip-write-element sub-element))))))

(after! notmuch
  (defun +patch-notmuch/remove-tag-filter (tag-name query-string)
    (or
     (string-replace (format " and tag:%s" tag-name) "" query-string)
     (string-replace (format "tag:%s and " tag-name) "" query-string)
     (string-replace (format "tag:%s" tag-name) "*" query-string)
     query-string))


  (defun +patch-notmuch/toggle-unread ()
    (interactive)
    (let ((query-string (if (string-match "tag:unread" notmuch-search-query-string)
                            (+patch-notmuch/remove-tag-filter "unread" notmuch-search-query-string)
                          (concat notmuch-search-query-string " and tag:unread"))))
      (notmuch-search query-string notmuch-search-oldest-first))))
