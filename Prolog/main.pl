/*****  *****/
/**  **/

/*Modules aren't officially poirtate*/

/***** List Tools Module *****/

test(A, B, R) :-
    write(A),nl,
    write(B),nl,
    R is 1.

/** Like the index Function in Haskell **/
index(0, [X], X).
index(0, [H|_], H).
index(N, [_|T], E) :- 
    (N < 0 ->
        throw(error(negative_index, index/3))
    ;
        N1 is N-1, 
        index(N1, T, E)
    ).

/** Like the head Function in Haskell **/
head(Lst, Flst) :-
    index(0, Lst, Flst).

/** Like the tail Function in Haskell **/
tail([_|Tail], Tail).

/** Like the drop Function in Haskell **/
drop(0, Lst, Lst).
drop(1, Lst, Flst) :-
    tail(Lst, Flst).
drop(N, Lst, Flst) :-
    (N < 0 ->
        throw(error(negative_parameter, drop/3))
    ;
        N1 is N -1,
        tail(Lst, Rlst),
        drop(N1, Rlst, Flst)
    ).


/** Like the init Function in Haskell **/
init(Lst, Flst):-
    reverse(Lst, [_|Lst1]), 
    reverse(Lst1, Flst).

/** Tool to  Remove Tail's Elements of a List from an Index **/
remove_from_tail(0, Lst, Lst).
remove_from_tail(1, Lst, Flst) :-
    init(Lst, Flst).
remove_from_tail(N, Lst, Flst) :-
    (N < 0 -> 
        throw(error(negative_parameter, remove_from_tail/3))
    ;
        N1 is N -1,
        init(Lst, Rlst),
        remove_from_tail(N1, Rlst, Flst)
    ).


/** Like the take Function in Haskell **/
take(N, Lst, Flst):-
    (N < 0 ->
        throw(error(negative_parameter, take_list/3))
    ;
        length(Lst, Len),
        N1 is Len - N,
        remove_from_tail(N1, Lst, Flst)
    ).
    
/** Like the last Function in Haskell, but in this case returns N elements from the end of list **/
lastN(N, Lst, Flst):-
    (N < 0 ->
        throw(error(negative_parameter, lastN/3))
    ;
        length(Lst, Len),
        N1 is Len - N,
        drop(N1, Lst, Flst)
    ).
/***** End *****/

/***** Detection Module *****/
verify_lenght(Lst, Rb) :-
    length(Lst, N),
    (N < 31 -> 
        Rb = 0
    ;
        (N > 31 -> 
            Rb = 0
        ;
            Rb = 1
        )
    ).

verify_degrees(Num, Rb) :-
    (Num < 0 -> 
        Rb = 0
    ;
        (Num > 89 -> 
            Rb = 0
        ;
            Rb = 1
        )
    ).

verify_primes(Num, Rb) :-
    (Num < 0 -> 
        Rb = 0
    ;
        (Num > 59 -> 
            Rb = 0
        ;
            Rb = 1
        )
    ).

split(Lst, Flst) :-
    drop(17, Lst, Flst).

getLatitude(Lst, Flst) :-
    verify_lenght(Lst, N),
    ( N == 0 ->
        throw(error(invalid_argument, getLatitude/2))
    ;
        head(Lst, Sign),
        drop(2, Lst, A),
        take(2, A, B), 
        number_chars(Degrees, B),
        take(7, Lst, C),
        drop(5, C, D),
        number_chars(Primes, D),
        take(14, Lst, E),
        drop(8, E, F),
        number_chars(Latters, F),
        Flst = [Sign, Degrees, Primes, Latters]
    ).

getLongitude(Lst, Flst) :-
    verify_lenght(Lst, N),
    ( N == 0 ->
        throw(error(invalid_argument, getLongitude/2))
    ;
        split(Lst, Lst1),
        head(Lst1, Sign),
        drop(2, Lst1, A),
        take(2, A, B), 
        number_chars(Degrees, B),
        take(7, Lst1, C),
        drop(5, C, D),
        number_chars(Primes, D),
        take(14, Lst1, E),
        drop(8, E, F),
        number_chars(Latters, F),
        Flst = [Sign, Degrees, Primes, Latters]
    ).

verify_detection_body(Lst, Rb) :-
    index(1, Lst, A),
    verify_degrees(A, B),
    (B == 0 -> 
        throw(error(wrong_degrees, verify_detection_body/2))
    ;
        index(2, Lst, C),
        verify_primes(C, D),
        (D == 0 ->
            throw(error(wrong_primes, verify_detection_body/2))
        ;
            index(2, Lst, E),
            verify_primes(E, F),
            (F == 0 -> 
                throw(error(wrong_latters, verify_detection_body/2))
            ;
            Rb = 1
            )
        )
    ).

verify_latitude(Lst, Rb) :-
    index(0, Lst, A),
    (A == 'N' -> 
        Rb = 1
    ;
        (A == 'S' -> 
            Rb = 1
        ;
        throw(error(wrong_sign, verify_latitude/2))
        )
    ).

verify_longitude(Lst, Rb) :-
    index(0, Lst, A),
    (A == 'E' -> 
        Rb = 1
    ;
        (A == 'W' -> 
            Rb = 1
        ;
        throw(error(wrong_sign, verify_longitude/2))
        )
    ).
/***** End *****/

/***** Main & Main Util *****/
/*main :-
    write('Detections Properties Calculator V1.0'), nl,
    write('Waring: The Detections must be in D.M.G format and inserted into the program like: N 40 45 36.000 - E 73 59 2.400'), nl,
    write('Insert the First Detection...'), nl,
    read(A),
    atom_chars(A, Det1),
    write('Insert the Second Detection...'), nl,
    read(B),
    atom_chars(B, Det2),
    write('---> First Detection Inserted:'), nl,
    write(Det1), nl,
    write('---> Second Detection Inserted:'), nl,
    write(Det2).*/

main :- 
    write('Insert a string...'), nl,
    read(A), nl,
    atom_chars(A, Lst),
    /*write('Insert index...'), nl,
    read(Index), nl,*/
    getLatitude(Lst, Remain),
    getLongitude(Lst, Remain1),
    write(Remain),
    write(Remain1).


/***** End *****/