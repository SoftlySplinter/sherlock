OUT=sherlock
SRC=src/
TEST=test/
ASK=ask.pl
INPTR=prolog

all: ${OUT}

${OUT}: 
	${INPTR} -o ${OUT} -c ${SRC}${ASK}

clean:
	rm -rf ${OUT}

test:
	${INPTR} -s ${TEST}/test_rdf.pl
