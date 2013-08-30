parse(Input):-
   parse(Input, Answer, Total),
   Total > 0,
   Confidence is (Answer/Total)*100,
   format('Confidence: ~w%~n', Confidence).

parse(Input, Answer, Total):-
    parse(Input, Input, 0, 0, Answer, Total).

parse([], _, IAnswer, ITotal, OAnswer, OTotal):-
    OAnswer is IAnswer,
    OTotal is ITotal.

parse([Subject|Tail], Input, IAnswer, ITotal, OAnswer, OTotal):-
    parseBySubject(Subject, Input, IAnswer, ITotal, NOAnswer, NOTotal),
    parse(Tail, Input, NOAnswer, NOTotal, OAnswer, OTotal).


parseBySubject(_, [], IAnswer, ITotal, OAnswer, OTotal):-
    OAnswer is IAnswer,
    OTotal is ITotal.

%parseBySubject(Subject, [Input|Tail], IAnswer, ITotal, OAnswer, OTotal):-
%    Subject = Input,
%    parseBySubject(Subject, Tail, IAnswer, ITotal, OAnswer, OTotal).

parseBySubject(Subject, [Input|Tail], IAnswer, ITotal, OAnswer, OTotal):-
    Subject \= Input,
    hasRDF(Subject),
    are([Input], IAnswer, NOAnswer),
    parseBySubject(Subject, Tail, NOAnswer, ITotal+1, OAnswer, OTotal).

parseBySubject(Subject, [_|Tail], IAnswer, ITotal, OAnswer, OTotal):-
    parseBySubject(Subject, Tail, IAnswer, ITotal, OAnswer, OTotal).



hasRDF('a').

are([], IAnswer, OAnswer):-
    OAnswer is IAnswer.

are(['b'|T], IAnswer, OAnswer):-
    are(T, IAnswer + 1, OAnswer).

are([_|T], IAnswer, OAnswer):-
    are(T, IAnswer, OAnswer).
