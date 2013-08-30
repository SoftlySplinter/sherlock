% Search a special file for synonyms and plurals (related words).
:- module(synonym, [synonym/2, has_synonym/1]).
:- use_module(['./rdf_utils.pl', library(rdf)]).

% The subject of the synonym file.
subject('synonym').

% not(+X) - Logical not of X.
%     X - The call to negate.
not(X):-
    call(X), 
    !, 
    fail.

% has_synonym(+Word) - Ensures a Word has a synonym.
%     Word - The word to check has a synonym.
has_synonym(Word):- 
    synonym(Word, List), 
    !,
    not_empty(List).

% not_empty(+List) - Checks if a list is empty.
%     List - The list to check is empty.
not_empty([]):- fail.

% not_empty(+List) - Checks if a list is empty.
%     List - The list to check is empty.
not_empty(_).

% synonym(+Word, -Synonyms) - Get synonyms of a word.
%     Word     - The word to get synonyms of.
%     Synonyms - The synonyms of that word.
%
%     Ensure the synonym file can be loaded correctly.
synonym(_, _):-
    subject(X),
    not(has_rdf(X)).

% synonym(+Word, -Synonyms) - Get synonyms of a word.
%     Word     - The word to get synonyms of.
%     Synonyms - The synonyms of that word.
synonym(Word, Synonyms):-
    subject(X),
    get_rdf(X, RDFData),
    get_synonyms(Word, RDFData, Synonyms).

% get_synonym(+Word, -Synonyms) - Get synonyms of a word using RDF data.
%     Word     - The word to get synonyms of.
%     RDFData  - The RDF data.
%     Synonyms - The synonyms of that word.
%
%     Calls into get_synonyms/4 with an empty input.
get_synonyms(Word, RDFData, Synonyms):-
    get_synonyms(Word, RDFData, [], Synonyms).

% get_synonyms(+Word, +RDFData, -Input, +Synonyms) - End point for getting
%                                         Synonyms.
%     Word     - The word to get synonyms of.
%     RDFData  - The RDF data of synonyms.
%     Input    - The loaded synonyms
%     Synonyms - The synonyms to return.
get_synonyms(_, [], Input, Synonyms):-
    append([], Synonyms, Input).

% get_synonyms(+Word, +RDFData, -Input, +Synonyms) - Load the synonyms.
%     Word     - The word to get synonyms of.
%     RDFData  - The RDF data of synonyms.
%     Input    - The loaded synonyms
%     Synonyms - The synonyms to return.
get_synonyms(Word, [rdf(Word, _, Value)|Tail], Synonyms, Output):-
    get_value(Value, Full), 
    !,
    append(Synonyms, Full, NewSynonyms),
    get_synonyms(Word, Tail, NewSynonyms, Output).

% get_synonyms(+Word, +RDFData, -Input, +Synonyms) - Load synonyms.
%     Word     - The word to get synonyms of.
%     RDFData  - The RDF data of synonyms.
%     Input    - The loaded synonyms
%     Synonyms - The synonyms to return.
get_synonyms(Word, [rdf(Subject, _, Value)|Tail], Synonyms, Output):-
    get_value(Value, Word),  
    !,
    append(Synonyms, Subject, NewSynonyms),
    get_synonyms(Word, Tail, NewSynonyms, Output).

% get_synonyms(+Word, +RDFData, -Input, +Synonyms) - Continuation.
%     Word     - The word to get synonyms of.
%     RDFData  - The RDF data of synonyms.
%     Input    - The loaded synonyms
%     Synonyms - The synonyms to return.
get_synonyms(Word, [_|Tail], Synonyms, Output):-
    get_synonyms(Word, Tail, Synonyms, Output).
