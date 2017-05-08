OUTFILE=haskell-intro.pdf
INFILE=haskell-intro.md

all:
	pandoc -t beamer -o ${OUTFILE} ${INFILE}

.PHONY: all
