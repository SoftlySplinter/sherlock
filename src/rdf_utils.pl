% Utilities for RDF Documents.
:- module(rdf_utils, [has_rdf/1, get_rdf/2, rdf_references/3, get_value/2]).

% Use the main swipl RDF library.
:- use_module([library(rdf), './synonym.pl']).

% dir('./rdf/') - The location of the RDF database.
dir('./rdf/').

% extension('.rdf') - The RDF file extension.
extension('.rdf').

% get_file(+Subject, -File) - Convert a subject to an RDF data file.
%     Subject - The subject title to search for.
%     File    - The returned filename.
%
%     The formate is: dir(Dir) + Subject + extension(Ext).
%
%     Example: get_file('tadpole', File) sets File to './rdf/tadpole.rdf'.
get_file(Subject, File):-
    dir(Dir),
    extension(Ext),
    concat(Dir, Subject, Temp),
    concat(Temp, Ext, File).

% has_rdf(+Subject) - Checks if a subject has an RDF file.
%     Subject - The subject to check for an RDF document.
has_rdf(Subject):-
    get_file(Subject, File),
    exists_file(File),
    load_rdf(File, _).

% get_rdf(+Subject, -Data) - Gets the RDF data for the subject.
%     Subject - The subject to get the RDF document of.
%     Data    - The RDF data, produced by the RDF library.
get_rdf(Subject, Data):-
    has_rdf(Subject),
    get_file(Subject, File),
    load_rdf(File, Data).

% rdf_references(+Subject, +RDFData, -Refs) - Search for RDF references.
%     Subject - The subject to look for references in the RDF data.
%     RDFData - The RDF data to search through.
%     Refs    - The reference count.
%
%     Call rdf_references/4 with 0 as the input reference count.
rdf_references(Subject, RDFData, Refs):-
    rdf_references(Subject, RDFData, 0, Refs).

% rdf_references(+Lookup, +Data, +IRefs, -ORefs) - End point for the RDF
%                                         refereces search.
%     Lookup - The lookup to search for references in.
%     Data   - The RDF data to search through.
%     IRefs  - The input reference count.
%     ORefs  - The output reference count.
rdf_references(_, [], IRefs, ORefs):-
    ORefs is IRefs, 
    !.

% rdf_references(+Lookup, +Data,  +IRefs, -ORefs) - Search through RDF
%                                         references search.
%     Lookup - The lookup to search for references in.
%     Data   - The RDF data to search through.
%     IRefs  - The input reference count.
%     ORefs  - The output reference count.
rdf_references(Lookup, [rdf(_, _, Value)|Tail], IRefs, ORefs):-
    value(Value, Lookup), 
    !,
    rdf_references(Lookup, Tail, IRefs+1, ORefs).

% rdf_references(+Lookup, +Data,  +IRefs, -ORefs) - Search through RDF
%                                         references search.
%     Lookup - The lookup to search for references in.
%     Data   - The RDF data to search through.
%     IRefs  - The input reference count.
%     ORefs  - The output reference count.
%
%     Checks through synonyms, as full references haven't been found.
%
%     This is weighted slightly lower to allow confidences to work fully.
rdf_references(Lookup, [rdf(_,_,Value)|Tail], IRefs, ORefs):-
    get_value(Value, RealValue),
    synonym(RealValue, Synonym),
    value(Lookup, Synonym), 
    !,
    rdf_references(Lookup, Tail, IRefs + 0.9, ORefs).

% rdf_references(+Lookup, +Data,  +IRefs, -ORefs) - Continuation.
%     Lookup - The lookup to search for references in.
%     Data   - The RDF data to search through.
%     IRefs  - The input reference count.
%     ORefs  - The output reference count.
rdf_references(Lookup, [_|Tail], IRefs, ORefs):-
    rdf_references(Lookup, Tail, IRefs, ORefs).

% value(+Value, +Lookup) - Fail if no references were found.
%     Lookup - The word to search for.
%     Value  - The values from the RDF data.
value([],_):- fail.

% value(+Value, +Lookup) - Succede if the reference is a match.
%     Lookup - The word to search for.
%     Value  - The value from the RDF data. Equal to Lookup.
value(Lookup, Lookup).

% value(+Value, +Lookup) - Succede if the reference is a match to
%                          the RDF literal.
%     Lookup - The word to search for.
%     Value  - The value from the RDF data. Equal to literal(Lookup).
value(literal(Lookup), Lookup).

% value(+Value, +Lookup) - Downcase both sides as it shouldn't be
%                          case sensative.
%     Lookup - The word to search for.
%     Value  - The value from the RDF data.
value(literal(Value), Lookup):-
    downcase_atom(Value, LValue),
    downcase_atom(Lookup, LLookup),
    value(LValue, LLookup).

% value(+Values, +Lookup) - Search through the RDF values for referencess.
%     Lookup - The word to search for.
%     Values  - The values from the RDF data.
value([Value|_], Lookup):-
    value(Value, Lookup).

% value(+Values, +Lookup) - Search through the RDF values for referencess.
%     Lookup - The word to search for.
%     Values  - The values from the RDF data.
value([_|Tail], Lookup):-
    value(Tail, Lookup).

% get_value(+Values, -FullValue) - Get the value of RDF element.
%     Value      - The value from the RDF data.
%     FullValue  - The string value.
get_value(literal(Value), FullValue):-
    downcase_atom(Value, LValue),
    get_value(LValue, FullValue).

% get_value(+Values, -FullValue) - Get the value of RDF element.
%     Value      - The value from the RDF data.
%     FullValue  - The string value.
get_value(Value, FullValue):-
    append([], Value, FullValue).
