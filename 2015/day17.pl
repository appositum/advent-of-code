main :-
    container_combs(P1),
    combs_with_min_len(P2),
    write("Part 1: "),
    write(P1),
    write("\nPart 2: "),
    write(P2).

sum([], 0).
sum([H|T], Sum) :-
    sum(T, Acc),
    Sum is H + Acc.

sums_to_N(Lst, N) :-
    sum(Lst, N).

combinations([], []).
combinations([H|T], [H|T2]) :-
    combinations(T, T2).
combinations([_|T], T2) :-
    combinations(T, T2).

container_fits(Lst, Max_liters, Res) :-
    combinations(Lst, Res),
    sums_to_N(Res, Max_liters).

stream_representations(Stream, Lines) :-
    read_line_to_codes(Stream, Line),
    (   Line == end_of_file
    ->  Lines = []
    ;   atom_codes(FinalLine, Line),
        Lines = [FinalLine | FurtherLines],
        stream_representations(Stream, FurtherLines) ).

read_file(File, List):-
    open(File, read, Stream),
    stream_representations(Stream, Lines),
    maplist(atom_number, Lines, List),
    close(Stream).

% Part 1
container_combs(Container_combinations) :-
    read_file("inputs/day17.txt", Input),
    aggregate_all(count, container_fits(Input, 150, _Res), Container_combinations).

% Part 2
container_combs_len(Input, Lengths) :-
    container_fits(Input, 150, Res),
    length(Res, Lengths).

container_combs_len_list(Result) :-
    read_file("inputs/day17.txt", Input),
    findall(Lengths, container_combs_len(Input, Lengths), Result).

min(List, Min) :-
    aggregate(min(Element), member(Element, List), Min).

combs_with_min_len(Count) :-
    container_combs_len_list(Lengths),
    min(Lengths, Minimum),
    aggregate_all(count, member(Minimum, Lengths), Count).
