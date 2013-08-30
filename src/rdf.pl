:- module(rdf_ask, [has_rdf/1, get_rdf/2, rdf_references/3]).
:- use_module(library(rdf)).

% dir('./rdf/') - The location of the RDF database.
dir('./rdf/').

% extension('.rdf') - The RDF file extension.
extension('.rdf').

% get_file(+Subject, -File) - Convert a subject to an RDF data file.
%     The formate is: dir(Dir) + Subject + extension(Ext).
%     Example: get_file('tadpole', File) sets File to './rdf/tadpole.rdf'.
get_file(Subject, File):-
    dir(Dir),
    extension(Ext),
    concat(Dir, Subject, Temp),
    concat(Temp, Ext, File).

% has_rdf(+Subject) - Checks if a subject has an RDF file.
has_rdf(Subject):-
    get_file(Subject, File),
    exists_file(File),
    load_rdf(File, _).

% get_rdf(+Subject, -Data) - Gets the RDF data for the subject.
get_rdf(Subject, Data):-
    has_rdf(Subject),
    get_file(Subject, File),
    load_rdf(File, Data).

rdf_references(Subject, RDFData, Refs):-
    rdf_references(Subject, RDFData, 0, Refs).

rdf_references(_, [], IRefs, ORefs):-
    ORefs is IRefs, !.

rdf_references(Lookup, [rdf(_, _, Value)|Tail], IRefs, ORefs):-
    value(Value, Lookup),
    rdf_references(Lookup, Tail, IRefs+1, ORefs), !.

rdf_references(Lookup, [_|Tail], IRefs, ORefs):-
    rdf_references(Lookup, Tail, IRefs, ORefs).

value(Lookup, Lookup).
value(literal(Lookup), Lookup).
value(literal(Value), Lookup):-
    downcase_atom(Value, LValue),
    downcase_atom(Lookup, LLookup),
    value(LValue, LLookup).

