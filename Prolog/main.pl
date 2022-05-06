/*****  *****/
/**  **/

/*Modules aren't officially poirtate*/

/***** List Tools Module *****/

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

split(Lst, Flst) :-
    drop(17, Lst, Flst).

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
        /*throw(error(wrong_sign, verify_latitude/2))*/
        Rb = 0
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
        /*throw(error(wrong_sign, verify_longitude/2))*/
        Rb = 0
        )
    ).

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

distance(Lst1, Lst2, Rn) :-
    index(0,Lst1,Lat1),
    index(1,Lst1,Long1),
    index(0,Lst2,Lat2),
    index(1,Lst2,Long2),
    A is Long1 - Long2,
    B is cos(A),
    C is cos(Lat2),
    D is cos(Lat1),
    E is B * C * D,
    F is sin(Lat2),
    G is sin(Lat1),
    H is F * G,
    I is H + E,
    L is acos(I),
    Rn is L * 6372.795477598.
    


direction(Lst1, Lst2, Rn) :-
    index(0,Lst1,Lat1),
    index(1,Lst1,Long1),
    index(0,Lst2,Lat2),
    index(1,Lst2,Long2),
    (Lat2 == Lat1 ->
        A is pi / 180,
        Phi is A * 0.000000001
    ;
        
        B is pi / 4,
        C is Lat1 / 2,
        D is B + C,
        E is tan(D),
        F is Lat2 / 2,
        G is B + F,
        H is tan(G),
        I is H / E,
        Z is abs(I),/*<---- Issues*/
        Phi is log10(Z)
    ),
    (Long2 == Long1 ->
        Lon is Phi
    ;
        L is Long1 - Long2,
        M is abs(L),
        O is pi / 180,
        N is O * M, /*<---- Issues*/
        (N > 180 ->
            Lon is N mod 180
        ;
        Lon = N
        )
    ),
    Rn is atan2(Lon, Phi).
    
inverse_direction(Lst1, Lst2, Rn) :-
    direction(Lst1, Lst2, A),
    Rn is A + 180.
/***** End *****/

/***** Tools Module *****/
/*round_n(D,Num,Rnum) :- 

    Num1 is Num * 10^D, <----- Issues
    Num2 is round(Num1), 
    nl, write('chiamata da round'), nl,
    write(Num2), nl,
    Rnum is Num2 / 10^D.*/
/***** End *****/

/***** Main & Main Util *****/
main :-
    write('Detections Properties Calculator V1.0'), nl,
    write('Waring: The Detections must be in D.M.G format and inserted into the program like: N 40 45 36.000 - E 73 59 2.400'), nl,
    write('Insert the First Detection...'), nl,
    read(A),
    atom_chars(A, Det1),
    write('Insert the Second Detection...'), nl,
    read(B),
    atom_chars(B, Det2),
    write('Proceed [yes./n.]?'), nl,
    read(C),
    (C == 'yes' ->
        write('First Detection in Decimal Format ---> '),
        get_point(Det1, P1),
        index(0, P1, Dlat1),
        index(1, P1, Dlong1),
        /*round_n(3, Dlat1, Rdlat1), <----- Issues
        round_n(3, Dlong1, Rdlong1),*/
        write(Dlat1), write(','), write(Dlong1), nl,
        write('Second Detection in Decimal Format ---> '),
        get_point(Det2, P2),
        index(0, P2, Dlat2),
        index(1, P2, Dlong2),
      /*round_n(3, Dlat2, Rdlat2), <----- Issues
        round_n(3, Dlong2, Rdlong2),*/
        write(Dlat2), write(','), write(Dlong2), nl,
        write('Distance between First & Second Detections ---> '),
        distance(P1, P2, Distance),
        write(Distance), write('Km'), nl,
        write('Positive direction between First & Second Detections ---> '),
        direction(P1, P2, Direction),
        write(Direction), write('°'), nl,
        write('Negative direction between First & Second Detections ---> '),
        inverse_direction(P1, P2, InverseDir),
        write(InverseDir), write('°')
    ;
        write('Aborted...')
    ).

/***** End *****/