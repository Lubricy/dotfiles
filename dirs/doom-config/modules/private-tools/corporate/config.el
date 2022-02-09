(use-package! excorporate
  :defer t
  :commands excorporate
  :config
;;; hack
  (defun exco-diary-insert-meeting (finalize
                                    subject start _end _location
                                    _main-invitees _optional-invitees
                                    icalendar-text)
    "Insert a retrieved meeting into the diary.
     see also the documentation for `exco-calendar-item-iterate'.  the
     arguments are subject, a string, the subject of the meeting,
     start, the start date and time in emacs internal representation,
     and icalendar-text, icalendar text representing the meeting.
     _end, _location, _main-invitees, and _optional-invitees are
     unused.
     call finalize after the meeting has been inserted."
    (when (not (string-match "^Cancel[l]?ed: " subject))
      ;; FIXME: Sometimes meetings are duplicated if they have
      ;; overlapping (and (diary-cyclic ...) (diary-block ...)) ranges,
      ;; e.g., one in the today file and one in the transient file.
      ;; Maybe we should de-duplicate them in the final display.  If the
      ;; meeting start time is sometime today then put it in today's
      ;; diary file, otherwise put it in the transient one.
      (let* ((time (decode-time (current-time)))
             (now (list (elt time 3) (elt time 4) (elt time 5)))
             (dawn (apply #'encode-time 0 0 0 now))
             (dusk (time-add dawn (seconds-to-time 86400)))
             (file (if (and (time-less-p dawn start) (time-less-p start dusk))
                       excorporate-diary-today-file
                     excorporate-diary-transient-file)))
        (with-temp-buffer
          (insert icalendar-text)

          ;; FIXME: Maybe some users of multiple calendars will want to
          ;; know the source calendar's name for each diary entry.
          ;; There is no great way to achieve that right now, but one
          ;; idea is to add X-WR-CALNAME support to
          ;; icalendar-import-buffer, replace the
          ;; exco-diary-insert-meeting argument to
          ;; exco-calendar-item-with-details-iterate with:
          ;;
          ;; (lambda (&rest arguments)
          ;;  (apply #'exco-diary-insert-meeting identifier arguments))
          ;;
          ;; and uncomment the following code.
          ;;
          ;; (goto-char (point-min))
          ;; (while (re-search-forward
          ;;	"^SUMMARY\\([^:]*\\):\\(.*\\(\n[ 	].*\\)*\\)" nil t)
          ;;   (insert (format "\nX-WR-CALNAME: (%s)" identifier)))

          ;; Escape literal percent signs (%).  Use less-than sign (<)
          ;; and greater-than sign (>) which are forbidden URL
          ;; characters, so that in the plain text diary file,
          ;; percent-encoded URLs become completely invalid rather than
          ;; slightly wrong.
          (goto-char (point-min))
          (while (re-search-forward "%" nil t)
            (replace-match "<EXCO_PERCENT_SIGN>"))
          (while
              (re-search-forward
               "\\(25[-5]\\|2[0-4][0-9]\\|1[0-9][0-9]\\|[1-9]?[0-9]\\)\\.\\(25[0-5]\\|2[0-4][0-9]\\|1[0-9][0-9]\\|[1-9]?[0-9]\\)\\.\\(25[0-5]\\|2[0-4][0-9]\\|1[0-9][0-9]\\|[1-9]?[0-9]\\)\\.\\(25[0-5]\\|2[0-4][0-9]\\|1[0-9][0-9]\\|[1-9]?[0-9]\\)"
               nil t)                   ;
            (replace-match "<EXCO_IPV4_ADDR \\1 \\2 \\3 \\4>"))
          (icalendar-import-buffer file t))))
    (funcall finalize))
  (defun lubricy/agenda-update-diary ()
    "call excorporate to update the diary for today"
    (exco-diary-diary-advice (calendar-current-date) (calendar-current-date) #'message "diary updated")))
