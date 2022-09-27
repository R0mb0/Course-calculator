/***** List tools module. *****/

/*Like the head function in Haskell. 
 * Input: A list.
 * Output: The first element of list.*/
head([H], H).
head([H|_], H).

/*Like the tail function in Haskell. 
 * Input: A list.
 * Output: A list without the first element.*/
tail([T], T).
tail([_|T], T).

/*Like the drop function in Haskell. 
 * Input: An integer number, a list.
 * Output: A list without the number of elements specified from the head.*/
drop(0, List, Final_list) :-
    list(List),
    Final_list = List.
drop(1, List, Final_list) :-
    list(List),
    tail(List, Final_list).
drop(N, List, Final_list) :-
    integer(N),
    list(List),
    \+(N < 0),
    N1 is N - 1,
    tail(List, Rlist),
    drop(N1, Rlist, Final_list).

/*Like the init function in Haskell. 
 * Input: A list.
 * Output: A list without the last element.*/
init([X], X).
init(List, Final_list) :-
    list(List),
    reverse(List, [_|List1]), 
    reverse(List1, Final_list).

/*Remove ns elements from the input list.
 * Input: An integer number, a List.
 * Output: A list without the number of elements specified from the tail.*/
remove_from_tail(0, List, Final_list) :-
    list(List),
    Final_list = List.
remove_from_tail(1, List, Final_list) :-
    list(List),
    init(List, Final_list).
remove_from_tail(N, List, Final_list) :-
    integer(N),
    list(List),
    \+(N < 0),
    N1 is N - 1,
    init(List, List1),
    remove_from_tail(N1, List1, Final_list).

/*Like the take function in Haskell. 
 * Input: An integer number, a list.
 * Output: A list with only the number of elements specified from the head.*/
take(_, [X], X).
take(N, List, Final_list) :-
    integer(N),
    list(List),
    \+(N < 0),
    length(List, Len),
    N1 is Len - N,
    remove_from_tail(N1, List, Final_list).

/***** End module. *****/