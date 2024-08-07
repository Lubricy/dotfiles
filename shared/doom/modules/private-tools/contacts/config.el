;; -*- lexical-binding: t; -*-
(map!
 :leader
 (:prefix ("i" . "insert")
          (:prefix ("l" . "link")
           :desc "People" "p" #'contact/insert))
 (:prefix ("l" . "link")
  :desc "People" "p" #'contact/insert))


(cl-defstruct (identity-provider
               (:constructor identity-provider--create)
               (:copier nil))
  name fetcher org formatter formats)

(cl-defstruct (contact
               (:constructor contact--create)
               (:copier nil))
  uid props)

(defcustom contact-identity-providers-alist
  '()
  "list of identity providers"
  :type '(alist
          :key-type (symbol :tag "Key")
          :value-type (identity-provider :tag "Value"))
  :group 'contacts)

(defcustom contact-default-identity-provider
  'github
  "default identity provider to use"
  :type 'symbol
  :group 'contacts)

(defvar contact-repo
  (make-hash-table :test #'equal)
  "the contacts repository")

(after! savehist
  (add-to-list 'savehist-additional-variables 'contact-repo))

(after! org
  (defun +org/update-contact-link (&optional link)
    "Replace LINK with proper data at point"
    (save-excursion
      (let* ((link (or link (org-element-context)))
             (type (org-element-property :type link))
             (path (org-element-property :path link)))
        (goto-char (org-element-property :begin link))
        (dolist (tag (contact/org-tags))
          (when (string-equal type tag)
            (let* ((choice (contact-which (contact/find path)))
                   (pos (org-in-regexp org-link-any-re 1)))
              (when pos
                (let ((start (car pos))
                      (end (cdr pos)))
                  (delete-region start end)
                  (goto-char start)
                  (insert (contact/format 'link choice))
                  't))))))))

  (add-hook! org-ctrl-c-ctrl-c #'+org/update-contact-link))

(after! marginalia
  (defun contact--annotator (cand)
    (let ((entries (-filter
                    (lambda (contact)
                      (--any? (string-match (format "^%s$" cand) it)
                              (contact-all-formats contact)))
                    (hash-table-values contact-repo)))
          (center (propertize " " 'display '(space :align-to center))))
      (pcase entries
        ('nil "")
        (`(,single) (concat
                     (propertize
                      (format " (%s)" (if (string= (contact/format 'uid single) cand)
                                          (contact/format 'name single)
                                        (contact/format 'uid single)))
                      'face 'marginalia-key)
                     center
                     (propertize (contact/format '--display single) 'face 'marginalia-documentation)))
        (multiple (concat
                   center
                   (propertize (format "%s candidates: %s" (length multiple)
                                       (s-join ", " (mapcar (lambda (s) (contact/format 'uid s))
                                                            multiple)))
                               'face 'marginalia-documentation))))))

  (add-to-list 'marginalia-prompt-categories '("\\<contact\\>" . contact))
  (add-to-list 'marginalia-annotator-registry
               '(contact contact--annotator builtin none)))

(after! embark
  (defvar-keymap embark-contact-actions-map
    :doc "Keymap for actions the current people."
    :parent embark-general-map
    "<return>" #'contact/im
    "m"    #'contact/mailto
    "h"  #'contact/homepage)
  (defun contact/im (key)
    (browse-url (format "sip:%s" (contact/format 'mail (contact-which key)))))
  (defun contact/mailto (key)
    (browse-url (format "mailto:%s" (contact/format 'mail (contact-which key)))))
  (defun contact/homepage (key)
    (browse-url (contact/format '--homepage (contact-which key))))
  (add-to-list 'embark-keymap-alist '(contact . embark-contact-actions-map)))

(defmacro identity-provider-add! (name &rest body)
  (declare (indent 1))
  (let ((aliases `(quote ,(mapcar #'symbol-name (list name (plist-get body :alias))))))
    `(progn
       (after! org
         (dolist (alias ,aliases)
           (org-link-set-parameters alias :follow #'contact/im)))
       (setf (alist-get (quote ,name) contact-identity-providers-alist)
             (identity-provider--create
              :name ,(symbol-name name)
              :fetcher ,(plist-get body :fetcher)
              :org ,aliases
              :formats '(,@(cl-loop for (s c) on (plist-get body :formats) by #'cddr
                                    collect `(,s . ,(if (characterp c)
                                                        (format "%%%c" c)
                                                      c)))
                         (--display . "%u")
                         (--representation . ,(string-join
                                               (cl-loop for (_ c) on (plist-get body :formats) by #'cddr
                                                        when (characterp c)
                                                        collect (format "%%%c" c))
                                               " ")))
              :formatter (lambda (props)
                           (format-spec-make
                            ,@(cl-loop for (s c) on (plist-get body :formats) by #'cddr
                                       when (characterp c)
                                       nconc `(,c (alist-get (quote ,s) props "<unknown>"))))))))))

(after! request
  (defun github-fetch-user (query)
    "Search GitHub users with QUERY."
    (when query
      (let ((url (format "https://api.github.com/search/users?q=%s&per_page=2" query)))
        (mapcan
         (lambda (item)
           (let ((data  (request-response-data
                         (request
                           (alist-get 'url item)
                           :parser 'json-read
                           :sync t))))
             (if (alist-get 'message data) ;; error
                 'nil
               `((("uid" . ,(alist-get 'login data "<unknown>"))
                  ("name" . ,(alist-get 'name data "<unknown>"))
                  ("html_url" . ,(alist-get 'html_url data "<unknown>"))
                  ("blog" . ,(alist-get 'blog data "<unknown>"))
                  ("email" . ,(alist-get 'email data "<unknown>")))))))
         (alist-get
          'items
          (request-response-data
           (request
             url
             :parser 'json-read
             :sync t)))))))

  (identity-provider-add! github
    :fetcher #'github-fetch-user
    :alias gh
    :formats (uid      ?u
                       name       ?c
                       email      ?m
                       html_url   ?h
                       link       "[[gh:%u][%c <%m>]]"
                       --display  "%u\t%c\t%m"
                       --homepage "https://github.com/%u")))
