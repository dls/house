NE2000-driver.dvi: NE2000-driver.tex references.bib
	latex NE2000-driver
	bibtex NE2000-driver
	latex NE2000-driver
	latex NE2000-driver

clean: 
	-rm *.log *.aux

veryclean: clean
	-rm NE2000-driver.dvi
	
