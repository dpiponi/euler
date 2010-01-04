euler.pdf: euler.tex
	pdflatex euler.tex

euler.tex: euler.lhs
	lhs2TeX euler.lhs > euler.tex
