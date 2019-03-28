include ./default.mk

clean:
    @printf "Cleaning lisp/*...\n"
    @$(RM) *.elc $(ELGS)

.PHONY: clean

