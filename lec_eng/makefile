# makefile: Rmd -> md -> tex -> pdf
# v 2.0
# .Rnw extension is automatically added

all: lec_01_eng.pdf lec_02_eng.pdf



%.pdf: %.tex
	# protection against biber error
	# http://tex.stackexchange.com/questions/140814/
	rm -rf `biber --cache`

	# create pdf
	# will automatically run pdflatex/biber if necessary
	latexmk -pdf $<

	# clean auxillary files
	latexmk -c $<

%.tex: %.md
	pandoc $< --to beamer --self-contained  --from markdown+autolink_bare_uris+ascii_identifiers+tex_math_single_backslash-implicit_figures --output $@ --variable theme=Madrid --variable colortheme=whale --highlight-style tango

%.md: %.Rmd
	Rscript -e "library(knitr); knit('$<')"


clean:
	# rm *.md
	# rm *.pdf
	# rm *.tex
