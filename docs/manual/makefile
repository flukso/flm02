BOOK=manual
PREFACE=preface
DIR=out

DATE=$(shell date '+%B %Y')
COMMIT=$(shell git rev-parse HEAD | awk '{print substr($$0,1,16)}')
URL=https://github.com/flukso/flm02/commit/$(shell git rev-parse HEAD)

SITE=$(USER)@flukso.net:~/www/public/files/flm02

pdf:
	mkdir -p $(DIR)
	cp *.tex *.sty $(DIR)
	sed -i 's,\\DATE,$(DATE),g' $(DIR)/$(BOOK).tex
	sed -i 's,\\COMMIT,$(COMMIT),g' $(DIR)/$(PREFACE).tex
	sed -i 's,\\URL,$(URL),g' $(DIR)/$(PREFACE).tex
	pdflatex -halt-on-error -output-directory=$(DIR) $(DIR)/$(BOOK).tex
	pdflatex -halt-on-error -output-directory=$(DIR) $(DIR)/$(BOOK).tex

view: pdf
	evince $(DIR)/$(BOOK).pdf	

publish: pdf
	scp $(DIR)/$(BOOK).pdf $(SITE)

clean:
	rm $(DIR)/*.aux $(DIR)/*.out $(DIR)/*.toc $(DIR)/*.log $(DIR)/*.pdf

pristine:
	rm -rf $(DIR)
