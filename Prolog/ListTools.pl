/***** List Tools Module *****/

/* Like the index Function in Haskell. 
 * Input: A integer Index, a List.
 * Output: The Element of the List at the Index Specified.*/
index(_, [], _) :-
    throw(error(empty_input_list, index/3)).
index(0, [X], X).
index(0, [H|_], H).
index(N, [_|T], E) :- 
    (integer(N) ->
        (N < 0 ->
            throw(error(negative_index, N, index/3))
        ;
            N1 is N - 1, 
            index(N1, T, E)
        )
    ;
        throw(error(not_integer_index, N, index/3))
    ).

/* Like the head Function in Haskell. 
 * Input: A List.
 * Output: The First Element of List.*/
head([],_) :-
    throw(error(empty_input_list, head/2)).
head([H], H).
head([H|_], H).

/* Like the tail Function in Haskell. 
 * Input: A List.
 * Output: A List without the first element.*/
tail([],_) :-
    throw(error(empty_input_list, tail/2)).
tail([T], T).
tail([_|T], T).

/* Like the drop Function in Haskell. 
 * Input: A Integer number, a List.
 * Output: A List without the number of elements specified from the head.*/
drop(_, [], _) :-
    throw(error(empty_input_list, drop/3)).
drop(0, List, Final_list) :-
    (list(List) -> 
        Final_list = List
    ;
        throw(error(wrong_input_list, List, drop/3))
    ).
drop(1, List, Final_list) :-
    (list(List) -> 
        tail(List, Final_list)
    ;
        throw(error(wrong_input_list, List, drop/3))
    ).
drop(N, List, Final_list) :-
    (integer(N) -> 
        (list(List) ->
            (N < 0 ->
                throw(error(negative_parameter, N, drop/3))
            ;
                N1 is N - 1,
                tail(List, Rlist),
                drop(N1, Rlist, Final_list)
            )
        ;
            throw(error(wrong_input_list, List, drop/3))
        )
    ;
        throw(error(wrong_input_number, N, drop/3))
    ).

/* Like the init Function in Haskell. 
 * Input: A List.
 * Output: A List without the last element.*/
init([], _) :-
    throw(error(empty_input_list, init/2)).
init([X], X).
init(List, Final_list) :-
    (list(List) -> 
        reverse(List, [_|List1]), 
        reverse(List1, Final_list)
    ;
        throw(error(wrong_input_list, List, init/2))
    ).
    
/* Remove Ns Elements From the Input List.
 * Input: A Integer number, a List.
 * Output: A List without the number of elements specified from the tail.*/
remove_from_tail(_, [], _) :-
    throw(error(empty_input_list, remove_from_tail/3)).
remove_from_tail(_, [_], []).
remove_from_tail(0, List, Final_list) :-
    (list(List) -> 
        Final_list = List
    ;
        throw(error(wrong_input_list, List, remove_from_tail/3))
    ).
remove_from_tail(1, List, Final_list) :-
    (list(List) -> 
        init(List, Final_list)
    ;
        throw(error(wrong_input_list, List, remove_from_tail/3))
    ).
remove_from_tail(N, List, Final_list) :-
    (integer(N) -> 
        (list(List) ->
            (N < 0 -> 
                throw(error(negative_parameter, N, remove_from_tail/3))
            ;
                N1 is N - 1,
                init(List, List1),
                remove_from_tail(N1, List1, Final_list)
            )
        ;
            throw(error(wrong_input_list, List, remove_from_tail/3))
        )
    ;
        throw(error(wrong_input_number, N, remove_from_tail/3))
    ).

/* Like the take Function in Haskell. 
 * Input: A Integer number, a List.
 * Output: A List with only the number of elements specified from the head.*/
take(_, [], _) :-
    throw(error(empty_input_list, take/3)).
take(_, [X], X).
take(N, List, Final_list) :-
    (integer(N) ->
        (list(List) -> 
            (N < 0 ->
                throw(error(negative_parameter, N, take_list/3))
            ;
                length(List, Len),
                N1 is Len - N,
                remove_from_tail(N1, List, Final_list)
            )
        ;
            throw(error(wrong_input_list, List, take/3))
        )
    ;
        throw(error(wrong_input_number, N, take/3))
    ).
    
/***** End Module *****/