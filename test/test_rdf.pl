:- begin_tests(rdf).
:- use_module(['../src/rdf.pl']).

test(get_file):-
    get_file('tadpole',File),
    File = './rdf/tadpole.pl'.

:- end_tests(rdf).
