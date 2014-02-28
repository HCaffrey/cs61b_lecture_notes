all:
	cd ditaa-images; rm *; cd ..; emacs --script make.el; ./clean.sh

