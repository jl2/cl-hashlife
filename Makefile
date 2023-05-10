cl-hashlife: manifest.txt *.lisp *.asd
	buildapp --output hashlife --manifest-file manifest.txt --load-system asdf --load-system alexandria  --load-system cl-hashlife --entry 'hl:main'


manifest.txt: *.asd
	sbcl --no-userinit --no-sysinit --non-interactive --load ~/quicklisp/setup.lisp --eval '(ql:quickload :alexandria)'  --eval '(ql:write-asdf-manifest-file "~/src/lisp/cl-hashlife/manifest.txt")'


clean:
	rm -Rf manifest.txt cl-hashlife *.fasl

.PHONY: clean
