:- library(rdf).

are(File, Thing, Value):-
	load_rdf(File, RDF),
	are_rdf(RDF, Thing, Value).

are_rdf([],_,_):-
	fail.
are_rdf([rdf(Thing, Predicate, Object)|T], Thing, Value):-
	are_rdf([Object], Value, Thing, T).

are_rdf([], Value, Thing, T):-
	are_rdf(T, Thing, Value).
are_rdf([literal(Value)|_], Value, _, _).
are_rdf([_|T], Value, Thing, OtherT):-
	are_rdf(T, Value, Thing, OtherT).
