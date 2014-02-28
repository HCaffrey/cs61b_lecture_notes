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


(require 'org)
(require 'org-html)

(setq org-ditaa-jar-path "~/configs/lisp/org/contrib/scripts/ditaa.jar")

(find-file "notes.org")

(let ((org-export-with-section-numbers nil)
      (org-export-html-style-default cs61b-export-html-style-default))
  (progn
    (org-map-entries (lambda ()
		       (if (looking-at "^\*[ \t]+[Ll]ecture[ \t]+\\([0-9]+\\)")
			   (let* ((name (format "notes%s"
						(match-string 1)))
				  (file (format "%s.org" name))
				  (html (format "%s.html" name)))
			     (org-copy-subtree)
			     (with-temp-buffer
			       (org-paste-subtree)
			       (write-file file)
			       (org-export-as-html 3)))))
		     nil 'file)
    (org-export-as-html 3)))
