OUT=sherlock
SRC=src/
ASK=ask.pl

all: ${OUT}

${OUT}:
	prolog -o ${OUT} -c ${SRC}${ASK}

clean:
	rm -rf ${OUT}
