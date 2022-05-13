/*Modules aren't officially poirtate*/

/***** List Tools Module *****/

/* Like the index Function in Haskell. 
 * Input: A integer Index, a List.
 * Output: The Element of the List at the Index Specified.*/
index(_, [], _) :-
    throw(error(empty_input_list, index/3)).
index(0, [X], X).
index(0, [H|_], H).
index(N, [_|T], E) :- 
    (N < 0 ->
        throw(error(negative_index, index/3))
    ;
        N1 is N - 1, 
        index(N1, T, E)
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
 * Output: A List Without the First Element.*/
tail([],_) :-
    throw(error(empty_input_list, tail/2)).
tail([T], T).
tail([_|T], T).

/* Like the drop Function in Haskell 
 * Input: A Index, a List.
 * Output: A List Without */
drop(_, [], _) :-
    throw(error(empty_input_list, drop/3)).
drop(0, Lst, Lst).
drop(1, Lst, Flst) :-
    tail(Lst, Flst).
drop(N, Lst, Flst) :-
    (N < 0 ->
        throw(error(negative_parameter, drop/3))
    ;
        N1 is N - 1,
        tail(Lst, Rlst),
        drop(N1, Rlst, Flst)
    ).


/** Like the init Function in Haskell **/
init([], _) :-
    throw(error(empty_input_list, init/2)).
init([X], X).
init(Lst, Flst):-
    reverse(Lst, [_|Lst1]), 
    reverse(Lst1, Flst).

/** Tool to  Remove Tail's Elements of a List from an Index **/
remove_from_tail(_, [], _) :-
    throw(error(empty_input_list, remove_from_tail/3)).
remove_from_tail(_, [_], []).
remove_from_tail(0, Lst, Lst).
remove_from_tail(1, Lst, Flst) :-
    init(Lst, Flst).
remove_from_tail(N, Lst, Flst) :-
    (N < 0 -> 
        throw(error(negative_parameter, remove_from_tail/3))
    ;
        N1 is N - 1,
        init(Lst, Rlst),
        remove_from_tail(N1, Rlst, Flst)
    ).


/** Like the take Function in Haskell **/
take(_, [], _) :-
    throw(error(empty_input_list, take/3)).
take(_, [X], X).
take(N, Lst, Flst) :-
    (N < 0 ->
        throw(error(negative_parameter, take_list/3))
    ;
        length(Lst, Len),
        N1 is Len - N,
        remove_from_tail(N1, Lst, Flst)
    ).
    
/** Like the "last" Function in Haskell, but in this case returns N elements from the end of list **/
lastN(_, [], _) :-
    throw(error(empty_input_list, lastN/3)).
lastN(_, [X], X).
lastN(N, Lst, Flst) :-
    (N < 0 ->
        throw(error(negative_parameter, lastN/3))
    ;
        length(Lst, Len),
        N1 is Len - N,
        drop(N1, Lst, Flst)
    ).
/***** End *****/

/***** Detection Module *****/

/** Verifing the Lenght of the Detections String **/
verify_lenght([], _) :-
    throw(error(empty_input_list, verify_lenght/2)).
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

/** Verifing if the Degrees of detections are Real **/
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

/** Verifing if the Primes & Latters of detections are Real **/
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

verify_latitude([], _) :-
    throw(error(empty_input_list, verify_latitude/2)).
verify_latitude(Lst, Rb) :-
    index(0, Lst, A),
    (A == 'N' -> 
        Rb = 1
    ;
        (A == 'S' -> 
            Rb = 1
        ;
            Rb = 0
        )
    ).

verify_longitude([], _) :-
    throw(error(empty_input_list, verify_longitude/2)).
verify_longitude(Lst, Rb) :-
    index(0, Lst, A),
    (A == 'E' -> 
        Rb = 1
    ;
        (A == 'W' -> 
            Rb = 1
        ;
            Rb = 0
        )
    ).


/** Get the Longite part from the Detections string **/
split([], _) :-
    throw(error(empty_input_list, split/2)).
split(Lst, Flst) :-
    length(Lst, Len),
    (Len >= 17 ->
        drop(17, Lst, Flst)
    ;
        throw(error(input_list_has_not_enought_elements, split/2))
    ).
    

get_latitude([], _) :-
    throw(error(empty_input_list, get_latitude/2)).
get_latitude(Lst, Flst) :-
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

get_longitude([], _) :-
    throw(error(empty_input_list, get_longitude/2)).
get_longitude(Lst, Flst) :-
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

verify_detection_body([], _) :-
    throw(error(empty_input_list, verify_detection_body/2)).
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
            index(3, Lst, E),
            verify_primes(E, F),
            (F == 0 -> 
                throw(error(wrong_latters, verify_detection_body/2))
            ;
                Rb = 1
            )
        )
    ).

/** Verifing if the Sign of detections is Valid **/
check_sign(Lt, Rn) :-
    (Lt == 'S' ->
        Rn = (-1)
    ; 
        (Lt == 'W' -> 
            Rn = (-1)
        ;
            Rn = 1
        )
    ).

convert_to_decimal([], _) :-
    throw(error(empty_input_list, convert_to_decimal/2)).
convert_to_decimal(Lst, Num) :-
    index(0, Lst, Sign),
    check_sign(Sign, Sign1),
    index(1, Lst, Degrees),
    index(2, Lst, Primes),
    index(3, Lst, Latters),
    A is Latters / 60,
    B is Primes + A,
    C is B / 60,
    D is C + Degrees,
    Num is D * Sign1.

merge_coordinates(Num1, Num2, Flst) :-
    Flst = [Num1, Num2].

get_point([], _) :-
    throw(error(empty_input_list, get_point/2)).
get_point(Lst, Flst) :-
    get_latitude(Lst, Latitude),
    verify_latitude(Latitude, B1),
    get_longitude(Lst, Longitude),
    verify_longitude(Longitude, B2),
    (B1 == 0 ->
        throw(error(wrong_latitude, get_point/2))
    ;
        (B2 == 0 ->
            throw(error(wrong_longitude, get_point/2))
        ;
            convert_to_decimal(Latitude, Dlatitude),
            convert_to_decimal(Longitude, Dlongitude),
            merge_coordinates(Dlatitude, Dlongitude, Flst)
        )
    ).
    
/***** End *****/

/***** Properties Module *****/
distance([], _, _) :-
    throw(error(empty_first_input_list, distance/2)).
distance(_, [], _) :-
    throw(error(empty_second_input_list, distance/2)).
distance(Lst1, Lst2, Rn) :-
    index(0,Lst1,Lat1),
    index(1,Lst1,Long1),
    index(0,Lst2,Lat2),
    index(1,Lst2,Long2),
    A is pi / 180,
    B is Long1 - Long2,
    C is B * A,
    D is cos(C),
    Lat1n is Lat1 * A,
    Lat2n is Lat2 * A,
    E is cos(Lat2n),
    F is cos(Lat1n),
    G is D * E * F,
    H is sin(Lat2n),
    I is sin(Lat1n),
    L is H * I,
    M is L + G,
    N is acos(M),
    Rn is 6372.795477598 * N.
    

direction([], _, _) :-
    throw(error(empty_first_input_list, direction/2)).
direction(_, [], _) :-
    throw(error(empty_second_input_list, direction/2)).
direction(Lst1, Lst2, Rn) :-
    index(0,Lst1,Lat1),
    index(1,Lst1,Long1),
    index(0,Lst2,Lat2),
    index(1,Lst2,Long2),
    A is pi / 180,
    Lat1n is Lat1 * A,
    Lat2n is Lat2 * A,
    (Lat2 == Lat1 -> 
        Phi is pi / 180 * 0.000000001
    ;
        B is pi / 4,
        C is Lat1n / 2,
        D is C + B,
        E is tan(D),
        F is Lat2n / 2,
        G is F + B,
        H is tan(G),
        I is H / E,
        Phi is log(I)
        
    ),
    (Long2 == Long1 ->
        Lon is pi / 180 * 0.000000001
    ;
        L is Long1 - Long2,
        M is abs(L), 
        N is M * A,
        (N > 180 -> 
            Lon is N mod 180
        ;
            Lon = N
        )
    ),
    O is abs(Phi),
    P is atan2(Lon, O),
    Rn is P / pi * 180.
    
inverse_direction([], _, _) :-
    throw(error(empty_first_input_list, inverse_direction/2)).
inverse_direction(_, [], _) :-
    throw(error(empty_second_input_list, inverse_direction/2)).
inverse_direction(Lst1, Lst2, Rn) :-
    direction(Lst1, Lst2, A),
    Rn is A + 180.
/***** End *****/

/***** Main & Main Util *****/

/* Main. */
main :-
    write('Detections Properties Calculator V1.0'), nl,
    write('Waring: The Detections must be in D.M.G format and inserted into the program like: N 40 45 36.000 - E 73 59 2.400'), nl,
    write('Insert the First Detection...'), nl,
    read(A),
    atom_chars(A, Det1),
    write('Insert the Second Detection...'), nl,
    read(B),
    atom_chars(B, Det2),
    write('Proceed [yes./no.]?'), nl,
    read(C),
    (C == 'yes' ->
        write('First Detection in Decimal Format ---> '),
        get_point(Det1, P1),
        index(0, P1, Dlat1),
        index(1, P1, Dlong1),
        format('~3f', [Dlat1]), write(','), format('~3f', [Dlong1]), nl,
        write('Second Detection in Decimal Format ---> '),
        get_point(Det2, P2),
        index(0, P2, Dlat2),
        index(1, P2, Dlong2),
        format('~3f', [Dlat2]), write(','), format('~3f', [Dlong2]), nl,
        write('Distance between First & Second Detections ---> '),
        distance(P1, P2, Distance),
        format('~2f', [Distance]), write('Km'), nl,
        write('Positive direction between First & Second Detections ---> '),
        direction(P1, P2, Direction),
        format('~2f', [Direction]), write('°'), nl,
        write('Negative direction between First & Second Detections ---> '),
        inverse_direction(P1, P2, InverseDir),
        format('~2f', [InverseDir]), write('°')
    ;
        write('Aborted...')
    ).

/***** End *****/

add_test(X, Y, Z) :-
    ( integer(X) == integer(Y) ->
        Z is X + Y
    ;
        throw(error(not_a_integer, add_test/2))
    ).