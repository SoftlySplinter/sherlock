% Main program file. Main asking routines stored here.
:- module(ask, [ask/1]).
:- use_module(['./rdf_utils.pl', './synonym.pl']).

ask:-
    read(Question),
    ask(Question).

% ask(+Input, -Answer, -Total) - Ask a question.
%     Input  - list of strings.
%     Answer - The results found.
%     Total  - The number of RDF references found.
%
%     Will print out it's confidence of the question being true.
ask(Input):-
    ask(Input, Answer, Total),
    printResults(Answer, Total).

% printResults(+Answer, +Total) - Special case for no Total.
%     Will print a statement for when there is no knowledge in the RDF 
%     database about any subject).
printResults(_, 0):-
    format('I\'m afraid I don\'t know anything about that topic.~n',[]),

    % Cut to stop backtracking, then fail.
    !, 
    fail.

% printResults(+Answer, +Total) - Print results.
%     Answer - The results found.
%     Total  - The number of RDF references relating to topics any 
%              subject in the question.
%
%     Will create a percentage value and then print results based on 
%     that percentage.
printResults(Answer, Total):-
    Confidence is (Answer/Total)*100,
    printResults(Confidence).

% printResults(+Confidence) - Print results for a confidence level of
%                             90% or more.
%     Confidence - The percentage confidence level.
printResults(Confidence):-
    Confidence >= 90,
    format('I have the upmost confidence that is true (~2f% confidence).~n', 
        [Confidence]), 
    !.

% printResults(+Confidence) - Print results for a confidence level of
%                             75% or more.
%     Confidence - The percentage confidence level.
printResults(Confidence):-
    Confidence >= 75,
    format('I am rather certain that is true (~2f% confidence)~n', Confidence),
    !.

% printResults(+Confidence) - Print results for a confidence level of
%                             50% or more.
%     Confidence - The percentage confidence level.
printResults(Confidence):-
    Confidence >= 50,
    format('I am somewhat sure of that (~2f% confidence)~n', Confidence), !.

% printResults(+Confidence) - Print results for any other confidence 
%                             level (i.e. not confident at all).
%     Confidence - The percentage confidence level.
printResults(Confidence):-
    format('I do not believe it is likely (~2f% confidence)~n', Confidence),
    !, % Stop backtracking and fail.
    fail.

% ask(+Input, -Answer, -Total) - Entry point for ask/6.
%     Input  - The input array.
%     Answer - The results.
%     Total  - The number of RDF references found.
ask(Input, Answer, Total):-
    ask(Input, Input, 0, 0, Answer, Total).

% ask(+Array, +Input, +IAnswer, +ITotal, -OAnswer, -OTotal) - End point 
%                                         for ask/6.
%     Array   - The array being read from. Empty.
%     Input   - The array to recurse into. Unused.
%     IAnswer - The Input Answer count.
%     ITotal  - The Input Total count.
%     OAnswer - The Output Answer count.
%     OTotal  - The Ouput Total count.
%
%     Will take the input Answer and Total scores (IAnswer and ITotal) 
%     and set them to the outputs (OAnser, OTotal).
ask([], _, IAnswer, ITotal, OAnswer, OTotal):-
    OAnswer is IAnswer,
    OTotal is ITotal.

% ask(+Array, +Input, +IAnswer, +ITotal, -OAnswer, -OTotal) - Calls 
%                                         ask_subject/4 and recurses.
%     Array   - The array being read from.
%     Input   - The array to recurse into.
%     IAnswer - The Input Answer count.
%     ITotal  - The Input Total count.
%     OAnswer - The Output Answer count.
%     OTotal  - The Ouput Total count.
%
%     Ensures there is an RDF reference for the next element in Array
%     and goes calls into ask_subject/4
ask([Subject|Tail], Input, IAnswer, ITotal, OAnswer, OTotal):-
    has_rdf(Subject),
    ask_subject(Subject, Input, IAnswer, NOAnswer), 
    !, % Don't allow backtracking as ITotal will changed.
    ask(Tail, Input, NOAnswer, ITotal + 1, OAnswer, OTotal).

% ask(+Array, +Input, +IAnswer, +ITotal, -OAnswer, -OTotal) - Finds 
%                                         synonyms and searchs on those.
%     Array   - The array being read from.
%     Input   - The array to recurse into.
%     IAnswer - The Input Answer count.
%     ITotal  - The Input Total count.
%     OAnswer - The Output Answer count.
%     OTotal  - The Ouput Total count.
%
%     Calls into ask_synonym/6 to check for plurals and synonyms.
ask([Subject|Tail], Input, IAnswer, ITotal, OAnswer, OTotal):-
    has_synonym(Subject),
    synonym(Subject, Synonyms),
    ask_synonym(Synonyms, Input, IAnswer, ITotal, NOAnswer, NOTotal), 
    !, % No backtracking as things may changed.
    ask(Tail, Input, NOAnswer, NOTotal, OAnswer, OTotal).

% ask(+Array, +Input, +IAnswer, +ITotal, -OAnswer, -OTotal) - Add a
%                                         little to the total if 
%                                         nothing was found.
%                                         
%     Array   - The array being read from.
%     Input   - The array to recurse into.
%     IAnswer - The Input Answer count.
%     ITotal  - The Input Total count.
%     OAnswer - The Output Answer count.
%     OTotal  - The Ouput Total count.
%
%     If no RDF was found for the subject add a little to the total.
ask([Subject|Tail], Input, IAnswer, ITotal, OAnswer, OTotal):-
    format('~w not found~n', Subject),
    ask(Tail, Input, IAnswer, ITotal + 0.01, OAnswer, OTotal),
    !.

% ask_synonym(+Array, +Input, +IAnswer, +ITotal, -OAnswer, -OTotal) - 
%                                         End point of ask_synonym/6.
%     Array   - The array being read from.
%     Input   - The array to recurse into.
%     IAnswer - The Input Answer count.
%     ITotal  - The Input Total count.
%     OAnswer - The Output Answer count.
%     OTotal  - The Ouput Total count.
%
%     Set output to inputs.
ask_synonym([], _, IAnswer, ITotal, OAnswer, OTotal):-
    OAnswer is IAnswer,
    OTotal is ITotal.

% ask_synonym(+Array, +Input, +IAnswer, +ITotal, -OAnswer, -OTotal) -  
%                                         Calls ask_subject/4 and recurses.
%     Array   - The array being read from.
%     Input   - The array to recurse into.
%     IAnswer - The Input Answer count.
%     ITotal  - The Input Total count.
%     OAnswer - The Output Answer count.
%     OTotal  - The Ouput Total count.
%
%     Ensures there is an RDF reference for the next element in Array
%     and goes calls into ask_subject/4
ask_synonym([Subject|Tail], Input, IAnswer, ITotal, OAnswer, OTotal):-
    has_rdf(Subject),
    ask_subject(Subject, Input, IAnswer, NOAnswer), !,
    ask_synonym(Tail, Input, NOAnswer, ITotal + 1.001, OAnswer, OTotal).

% ask_synonym(+Array, +Input, +IAnswer, +ITotal, -OAnswer, -OTotal) -
%                                         Continuating
%     Array   - The array being read from.
%     Input   - The array to recurse into.
%     IAnswer - The Input Answer count.
%     ITotal  - The Input Total count.
%     OAnswer - The Output Answer count.
%     OTotal  - The Ouput Total count.
%
%     Recurse onwards.
ask_synonym([_|Tail], Input, IAnswer, ITotal, OAnswer, OTotal):-
    ask_synonym(Tail, Input, IAnswer, ITotal, OAnswer, OTotal),
    !.

% ask_synonym(+Array, +Input, +IAnswer, +ITotal, -OAnswer, -OTotal) -
%                                         Continuating
%     Array   - The array being read from.
%     Input   - The array to recurse into.
%     IAnswer - The Input Answer count.
%     ITotal  - The Input Total count.
%     OAnswer - The Output Answer count.
%     OTotal  - The Ouput Total count.
%
%     Recurse onwards.
ask_synonym(Subject, Input, IAnswer, ITotal, OAnswer, OTotal):-
    ask_synonym([Subject], Input, IAnswer, ITotal, OAnswer, OTotal).

% ask_subject(+Subject, +Array, +IAnswer, +ITotal, -OAnswer, -OTotal) - 
%                                         End point for ask_subject/6
%     Subject - The Subject to search for.
%     Array   - The Array to recurse into on ask_subject/6.
%     IAnswer - The Input Answer count.
%     ITotal  - The Input Total count.
%     OAnswer - The Output Answer count.
%     OTotal  - The Output Total count.
%
%     Will take the inputs and set them to the outputs for return values.
ask_subject(_, [], IAnswer, OAnswer):-
    OAnswer is IAnswer.

% ask_subject(+Subject, +Array, +IAnswer, +ITotal, -OAnswer, -OTotal) - 
%                                         Works out confidences for the 
%                                         current Subject.
%     Subject - The subject to search for.
%     Array   - The array to recurse into.
%     IAnswer - The input answer count.
%     ITotal  - The input total count.
%     OAnswer - The output answer count.
%     OTotal  - The output total count.
%
%     First of this checks that the Subject is not equal to the Input as 
%     asking 'X is X?' is assumed to be true.
%
%     Then checks there is an RDF document.
%
%     Then calls are/4 to work out confidences.
%
%     Recurse with ITotal being incremented so that precents can be 
%     generated.
%
%     Note: this should take into account synonyms and related words 
%     (e.g. tadpole -> tadpoles)
ask_subject(Subject, [Input|Tail], IAnswer, OAnswer):-
    not_linked(Subject, Input), 
    !,
    has_rdf(Subject),
    are(Subject, [Input], IAnswer, NOAnswer), 
    !,
    ask_subject(Subject, Tail, NOAnswer, OAnswer).

% ask_subject(+Subject, +Array, +IAnswer, +ITotal, -OAnswer, -OTotal) - 
%                                         Continuation.
%     Subject - The subject to search for.
%     Array   - The array to recurse into.
%     IAnswer - The input answer count.
%     ITotal  - The input total count.
%     OAnswer - The output answer count.
%     OTotal  - The output total count.
%
%     Continuation point for subjects that are the same as the input, 
%     or which have no RDF document.
ask_subject(Subject, [_|Tail], IAnswer, OAnswer):-
    ask_subject(Subject, Tail, IAnswer, OAnswer).

% not_linked(+Word, +Other) - Ensures a word is not linked to another.
%     Word  - The word to ensure is not linked.
%     Other - The other word to check against.
%
%     Useful to ensure Sherlock is not checking a synonym is the same
%     as another.
%
%     This checks the synonyms. 
not_linked(Word, Other):-
    synonym(Other, Synonyms), 
    !,
    not_linked_synonym(Word, Synonyms).

% not_linked(+Word, +Other) - Ensures a word is not linked to another.
%     Word  - The word to ensure is not linked.
%     Other - The other word to check against.
%
%     Useful to ensure Sherlock is not checking a synonym is the same
%     as another.
not_linked(Word, Other):-
    %TODO - This may need to be above the aforementioned.
    Word \= Other.

% not_linked_sunonym(Word, Synonyms) - Ensures synonyms are not linked
%                                      to a word.
%     Word     - The word to ensure is not linked to the synonyms.
%     Synonyms - The list of synonyms.
%
%     This is the end point, which means they're not linked.
not_linked_synonym(_, []).

% not_linked_sunonym(Word, Synonyms) - Ensures synonyms are not linked
%                                      to a word.
%     Word     - The word to ensure is not linked to the synonyms.
%     Synonyms - The list of synonyms.
%
%     Call not_linked_synonym/2 with the singular word.
%
%     Sometimes there's only one synonym which doesn't get returned as
%     a list. Doing it this way allows Sherlock to deal with either 
%     type easily.
not_linked_synonym(Word, [Other|_]):-
    not_linked_synonym(Word, Other).

% not_linked_sunonym(Word, Synonyms) - Ensures synonyms are not linked
%                                      to a word.
%     Word     - The word to ensure is not linked to the synonyms.
%     Synonyms - The list of synonyms.
%
%     Continuation point.
not_linked_synonym(Word, [_|Tail]):-
    not_linked_synonym(Word, Tail).

% not_linked_sunonym(Word, Synonyms) - Ensures synonyms are not linked
%                                      to a word.
%     Word  - The word to ensure is not linked to the synonyms.
%     Other - The other word to check against.
%
%     As not_linked/2.
not_linked_synonym(Word, Other):-
    Word \= Other.

% are(+Subject, +Array, +IAnswer, -OAnswer) - End point for are/4.
%     Subject - The subject currently being searched for.
%     Array   - The input array being recursed through.
%     IAnswer - The input answer count.
%     OAnswer - The output answer count.
%
%     Take IAnswer and assigns it to OAnswer for return values.
are(_, [], IAnswer, OAnswer):-
    OAnswer is IAnswer.

% are(+Subject, +Array, +IAnswer, -OAnswer) - Search RDF data.
%     Subject - The subject currently being searched for.
%     Array   - The input array being recursed through.
%     IAnswer - The input answer count.
%     OAnswer - The output answer count.
%
%     Search RDF data for references to the subject.
are(Subject, [Lookup|Tail], IAnswer, OAnswer):-
    get_rdf(Subject, RDFData),
    rdf_references(Lookup, RDFData, NoReferences), !,
    are(Subject, Tail, IAnswer + NoReferences, OAnswer).

% are(+Subject, +Array, +IAnswer, -OAnswer) - Search RDF data.
%     Subject - The subject currently being searched for.
%     Array   - The input array being recursed through.
%     IAnswer - The input answer count.
%     OAnswer - The output answer count.
%
%     Continuation for are/4.
are(Subject, [_|T], IAnswer, OAnswer):-
    are(Subject, T, IAnswer, OAnswer).
