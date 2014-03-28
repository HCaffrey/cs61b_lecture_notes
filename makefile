all:
	cd ditaa-images; rm *; cd ..; emacs -q --eval "(progn (load-file \"conversion-code.el\")(find-file \"notes.org\")(cs61b-export-to-html)(kill-emacs))"; ./clean.sh
