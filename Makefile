compile:
	mkdir core modules screencasts lisp
	@emacs -Q --batch -l 'compile.el'

update: 
	@emacs -Q --batch -l "update.el"
