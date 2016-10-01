# makefile: Rmd -> md -> tex -> pdf
# v 2.0
# .Rnw extension is automatically added
file_name = lec_10

$(file_name).pdf: $(file_name).tex
	# protection against biber error
	# http://tex.stackexchange.com/questions/140814/
	rm -rf `biber --cache`

	# create pdf
	# will automatically run pdflatex/biber if necessary
	latexmk -pdf $(file_name).tex

	# clean auxillary files
	latexmk -c $(file_name).tex

$(file_name).tex : $(file_name).md
	pandoc $(file_name).md --to beamer --self-contained  --from markdown+autolink_bare_uris+ascii_identifiers+tex_math_single_backslash-implicit_figures --output $(file_name).tex --variable theme=Madrid --variable colortheme=whale --highlight-style tango

$(file_name).md : $(file_name).Rmd
	Rscript -e "library(knitr); knit('$(file_name).Rmd')"

clean:
	rm $(file_name).md
	rm $(file_name).pdf
	rm $(file_name).tex
