OUT=sherlock
SRC=src/
ASK=ask.pl
COMPILER=prolog

all: ${OUT}

${OUT}: 
	${COMPILER} -o ${OUT} -c ${SRC}${ASK}

clean:
	rm -rf ${OUT}
