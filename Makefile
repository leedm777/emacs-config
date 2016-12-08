install:
	@if test -e $(HOME)/.emacs; then \
		echo "You'll have to get rid of $(HOME)/.emacs for this to work." >&2; \
		echo "Probably want to back it up first, though." >&2; \
		exit 1; \
	fi
	ln -s $(shell pwd) $(HOME)/.emacs.d
