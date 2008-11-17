;;; aurquery.el -- Interface to the Arch User Repository.

;; Copyright (C) 2008 Christopher M. Brannon.

;; Author: Christopher M. Brannon <cmbrannon@cox.net>
;; Version: 0.1
;; Keywords: AUR

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;;; Commentary:

;; This program provides an interface to the Arch User Repository (AUR).
;; Thus, it is primarily of interest to users of Archlinux.  For more
;; information about ArchLinux, please see http://archlinux.org/

;; Currently, this library provides two entry-points: aur-search and
;; aur-retrieve-info.

(require 'json)

(defconst aur--url-base "http://aur.archlinux.org"
  "Prefix of URLs used to access the AUR.")
(defconst aur--rpc-path "/rpc.php?"
  "Path of the RPC handler page.")

;; Accessor functions.
(defun aur-alist-val (key alist)
  "Return the value associated with KEY in ALIST."
  (cdr (assq key alist)))

(defun aur-response-type (response)
  "Return the type of RESPONSE.
The return value should be one of info, search, or error."
  (intern (aur-alist-val 'type response)))

(defun aur-get-result-name (result)
  "Return the value associated with key Name in alist RESULT."
  (aur-alist-val 'Name result))

(defun aur-url-from-request (request-type first-arg &rest rest-args)
  "Produce an URL representing REQUEST."
  (if (not (memq request-type '(info search)))
    (error "Query type is not one of info or search")
    (concat aur--url-base aur--rpc-path "type=" (symbol-name request-type) "&"
      (mapconcat
	(lambda (arg) (concat "arg=" (url-hexify-string arg)))
	(cons first-arg rest-args) "&"))))

(defun aur-parse-json-response ()
  "Parse the JSON object received from the webserver.
Right now, the current buffer contains the HTTP response.
The body of the response is a JSON object."
  (goto-char url-http-end-of-headers)
  (json-read))

(defun aur-info-result-buffer (response)
  "Make a buffer containing detailed information about a package."
  (let ((result (aur-alist-val 'results response))
	 (buffer (generate-new-buffer "*AUR Info*")))
    (save-excursion
      (set-buffer buffer)
      (insert (format "Name: %s\n" (aur-get-result-name result)))
      (insert (format "Description: %s\n" (aur-alist-val 'Description result)))
      (insert (format "Version: %s\n" (aur-alist-val 'Version result)))
      (insert (format "License: %s\n" (aur-alist-val 'License result)))
      (insert (format "Homepage: %s\n" (aur-alist-val 'URL result)))
      (insert (format "Votes: %s\n" (aur-alist-val 'NumVotes result)))
      (insert (format "Link to PKGBUILD: %s%s\n"
		aur--url-base (aur-alist-val 'URLPath result)))
      (when (not (zerop (string-to-int (aur-alist-val 'OutOfDate result))))
	(insert "This package is out of date.\n"))
      buffer)))

(defun aur-search-result-buffer (response)
  "Make a buffer containing a list of search results."
  (let ((results (aur-alist-val 'results response))
	 (buffer (generate-new-buffer "*AUR Search*")))
    (save-excursion
      (set-buffer buffer)
      (mapc
	#'(lambda (result) 
	    (insert (aur-get-result-name result))
	    (terpri buffer))
	results))
    buffer))

(defun aur-switch-to-response (status)
  "Build a buffer from server's response, and switch to it."
  (let ((response-buffer (current-buffer)))
    (unwind-protect
      (let* ((response (aur-parse-json-response))
	      (response-type (aur-response-type response))
	      (new-buffer
		(cond
		  ((eq response-type 'info) (aur-info-result-buffer response))
		  ((eq response-type 'search)
		    (aur-search-result-buffer response))
		  ((eq response-type 'error) (aur-json-error response))
		  (t (error "Unknown response of type %s" response-type)))))
	(switch-to-buffer new-buffer))
      (kill-buffer response-buffer))))

(defun aur-send-request (request-url)
  (url-retrieve request-url #'aur-switch-to-response))

(defun aur-search (keywords)
  (interactive "sKeywords: ")
  (let ((keyword-list (split-string keywords)))
    (if (null keyword-list)
      (error "You must specify at least one keyword.")
      (aur-send-request
	(apply #'aur-url-from-request 'search
	  (car keyword-list) (cdr keyword-list))))))

(defun aur-info (name-or-id)
  "Obtain detailed information about the package NAME-OR-ID."
  (interactive "sPackage name or ID: ")
  (aur-send-request
    (aur-url-from-request 'info name-or-id)))
