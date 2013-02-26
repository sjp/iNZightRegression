build-and-check:
	make build
	R CMD check iNZightRegression_0.0.2*.tar.gz
	-rm -rf iNZightRegression.Rcheck

install-and-run:
	make gen-version
	R CMD INSTALL ./
	make undo-version
	R

win-build:
	make build
	mkdir tmp
	R CMD INSTALL -l tmp iNZightRegression_0.0.2*.tar.gz
	cd tmp ; zip -r iNZightRegression_0.0.2-`date +%Y%m%d`.zip iNZightRegression ; mv iNZightRegression_0.0.2*.zip ../
	rm -rf tmp

build:
	make clean
	make gen-version
	R CMD build .
	make undo-version

release:
	make win-build
	mv iNZightRegression_0.0.2*.tar.gz iNZightRegression.tar.gz
	mv iNZightRegression_0.0.2*.zip iNZightRegression.zip

clean:
	-rm iNZightRegression_0.0.2*.tar.gz
	-rm -rf iNZightRegression.Rcheck/
	-rm iNZightRegression_0.0.2*.zip

gen-version:
	sed -i "s/0\.0\.2/\\0-`date +%Y%m%d`/" DESCRIPTION

undo-version:
	sed -i "s/0\.0\.2.*/0\.0\.2/" DESCRIPTION

