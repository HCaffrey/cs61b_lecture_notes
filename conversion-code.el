;;code to help convert diagrams to proper ditaa format
;; and wrap code in org blocks

(defun fix-ditaa ()
  (interactive)
  (save-excursion
    (goto-char 1)
    ;; (while (re-search-forward "\\.\\+--" nil t)
    ;;   (replace-match " +--"))
    ;; (goto-char 1)
    ;; (while (re-search-forward "--\\+\\." nil t)
    ;;   (replace-match "--+ "))
    ;; (goto-char 1)
    (while (re-search-forward "+\\||" nil t)
      (let ((p (1- (point)))
	    above below)
	(goto-char p)
	(previous-line)
	(if (and (looking-at-p "-") ;;check if we are at a corner
		 (or (or (bolp)
			 (save-excursion
			   (backward-char)
			   (looking-at-p "[^-]")))
		     (or (eolp)
			 (save-excursion
			   (forward-char)
			   (looking-at-p "[^-]")))))
	    
	    
	    (setq above (point)))
	(goto-char p)
	(next-line)
	(if (and (looking-at-p "-") ;;check if we are at a corner
		 (or (or (bolp)
			 (save-excursion
			   (backward-char)
			   (looking-at-p "[^-]")))
		     (or (eolp)
			 (save-excursion
			   (forward-char)
			   (looking-at-p "[^-]")))))
	    (setq below (point)))
	
	(when (and above below)
	  (goto-char above)
	  (delete-char 1)
	  (insert "+")
	  (goto-char below)
	  (delete-char 1)
	  (insert "+"))
	(goto-char (1+ p))
	))))

(setq ditaa-block-counter 0)
(defun ditaa-wrap-block (&optional image-dir file)
  (interactive)
  (setq image-dir (or image-dir "ditaa-images"))
  (unless (file-directory-p image-dir)
    (make-directory image-dir))
  (setq file (or file (format "img%s" (setq ditaa-block-counter
					    (1+ ditaa-block-counter)))))
  (mbs-wrap-region (format "#+begin_ditaa %s" (file-path-concat image-dir
								file))
		   "#+end_ditaa"))
(defun java-wrap-block ()
  (interactive)
  (mbs-wrap-region "#+Begin_SRC java" "#+END_SRC"))

(defun mbs-wrap-region (begin end)
  (interactive)
  (save-excursion 
    (goto-char (region-end))
    (unless (bolp)
      (insert "\n"))
    (insert end)
    (unless (eolp)
      (insert "\n"))
    
    (goto-char (region-beginning))
    (unless (bolp)
      (insert "\n"))
    (insert begin)
    (unless (eolp)
      (insert "\n"))))


;; manual conversions:
;;
;; this kind of list:
;; (1) ...
;; (2) ...

;; should to be changed to:
;; 1. ...
;; 2. ...

;; underlines like this
;; _empty_string_
;; need to be changed to
;; _empty\_string_ or _empty string_


(defun kill-buffer-if-exists (name)
  (let ((buff (get-buffer name)))
    (if buff
	(kill-buffer buff))))

(defconst cs61b-export-html-style-default
  "<style type=\"text/css\">
 <!--/*--><![CDATA[/*><!--*/
  html { font-family: Times, serif; font-size: 12pt; }
  .title  { text-align: center; }
  .todo   { color: red; }
  .done   { color: green; }
  .tag    { background-color: #add8e6; font-weight:normal }
  .target { }
  .timestamp { color: #bebebe; }
  .timestamp-kwd { color: #5f9ea0; }
  .right  {margin-left:auto; margin-right:0px;  text-align:right;}
  .left   {margin-left:0px;  margin-right:auto; text-align:left;}
  .center {margin-left:auto; margin-right:auto; text-align:center;}
  p.verse { margin-left: 3% }
  pre {
	padding: 5pt;
	font-family: courier, monospace;
        font-size: 90%;
        overflow:auto;
  }
  table { border-collapse: collapse; }
  td, th { vertical-align: top;  }
  th.right  { text-align:center;  }
  th.left   { text-align:center;   }
  th.center { text-align:center; }
  td.right  { text-align:right;  }
  td.left   { text-align:left;   }
  td.center { text-align:center; }
  dt { font-weight: bold; }
  div.figure { padding: 0.5em; }
  div.figure p { text-align: center; }
  div.inlinetask {
    padding:10px;
    border:2px solid gray;
    margin:10px;
    background: #ffffcc;
  }
  textarea { overflow-x: auto; }
  .linenr { font-size:smaller }
  .code-highlighted {background-color:#ffff00;}
  .org-info-js_info-navigation { border-style:none; }
  #org-info-js_console-label { font-size:10px; font-weight:bold;
                               white-space:nowrap; }
  .org-info-js_search-highlight {background-color:#ffff00; color:#000000;
                                 font-weight:bold; }
  /*]]>*/-->
</style>"
  "The default style specification for exported HTML files.
Please use the variables `org-export-html-style' and
`org-export-html-style-extra' to add to this style.  If you wish to not
have the default style included, customize the variable
`org-export-html-style-include-default'.")

(defun cs61b-export-to-html ()
  (interactive)
  (if (string= (buffer-name) "notes.org")
      (let ((org-export-with-section-numbers nil)
	    (org-export-html-style-default cs61b-export-html-style-default))
	(progn
	  (org-map-entries (lambda ()
			     (if (looking-at "^\*[ \t]+[Ll]ecture[ \t]+\\([0-9]+\\)")
				 (let* ((name (format "notes%s"
						      (match-string 1)))
					(file (format "single-notes/%s.org" name))
					(html (format "%s.html" name)))
				   (org-copy-subtree)
				   (with-temp-buffer
				     (org-paste-subtree)
				     (write-file file)
				     (org-export-as-html 3))
				   (kill-buffer-if-exists html))))
			   nil 'file)
	  (org-export-as-html 3)
	  (kill-buffer-if-exists "notes.html")))
    (message "Incorrect buffer: Run in notes.org")))
