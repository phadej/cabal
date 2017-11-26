.PHONY : all lexer sdpx lib exe doctest gen-extra-source-files

LEXER_HS:=Cabal/Distribution/Parsec/Lexer.hs
SPDX_LICENSE_HS:=Cabal/Distribution/SPDX/LicenseId.hs
SPDX_EXCEPTION_HS:=Cabal/Distribution/SPDX/LicenseExceptionId.hs

all : exe lib

lexer : $(LEXER_HS)

spdx : $(SPDX_LICENSE_HS) $(SPDX_EXCEPTION_HS)

$(LEXER_HS) : boot/Lexer.x
	alex --latin1 --ghc -o $@ $^

$(SPDX_LICENSE_HS) : SPDX.LicenseId.template.hs cabal-dev-scripts/src/GenSPDX.hs license-list-data/licenses.json
	cabal new-run --builddir=dist-newstyle-meta --project-file=cabal.project.meta gen-spdx -- SPDX.LicenseId.template.hs license-list-data/licenses.json $(SPDX_LICENSE_HS)

$(SPDX_EXCEPTION_HS) : SPDX.LicenseExceptionId.template.hs cabal-dev-scripts/src/GenSPDXExc.hs license-list-data/licenses.json
	cabal new-run --builddir=dist-newstyle-meta --project-file=cabal.project.meta gen-spdx-exc -- SPDX.LicenseExceptionId.template.hs license-list-data/exceptions.json $(SPDX_EXCEPTION_HS)

lib : $(LEXER_HS)
	cabal new-build --enable-tests Cabal

exe : $(LEXER_HS)
	cabal new-build --enable-tests cabal-install

doctest :
	doctest --fast Cabal/Distribution Cabal/Language

gen-extra-source-files:
	cabal new-run --builddir=dist-newstyle-meta --project-file=cabal.project.meta gen-extra-source-files -- Cabal/Cabal.cabal
	cabal new-run --builddir=dist-newstyle=meta --project-file=cabal.project.meta gen-extra-source-files -- cabal-install/cabal-install.cabal
