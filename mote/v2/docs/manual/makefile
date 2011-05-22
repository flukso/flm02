BOOK=manual

pdf:
	pdflatex -halt-on-error $(BOOK).tex
	pdflatex -halt-on-error $(BOOK).tex

view: pdf
	evince $(BOOK).pdf	

clean:
	rm *.aux *.out *.toc *.log *.pdf
