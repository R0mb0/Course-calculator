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

/***** Detection Module *****/

/* Verify if the Input String Has the Right Lenght.
 * Input: A List.
 * Output: A Boolean that is 1 If the Input String Has the Right Lenght, 0 Otherwise.*/
verify_lenght([], _) :-
    throw(error(empty_input_list, verify_lenght/2)).
verify_lenght(List, Return_bool) :-
    (list(List) -> 
        length(List, N),
        (N < 31 -> 
            Return_bool = 0
        ;
            (N > 31 -> 
                Return_bool = 0
            ;
                Return_bool = 1
            )
        )  
    ;
        throw(error(wrong_input_list, List, verify_lenght/2))
    ).

/* Verify if the Degrees of Detection Are Real.
 * Input: A Integer Number.
 * Output: A Boolean that is 1 If the Degrees Are Real, 0 Otherwise.*/
verify_degrees(Num, Return_bool) :-
   (integer(Num) ->
        (Num < 0 -> 
            Return_bool = 0
        ;
            (Num > 89 -> 
                Return_bool = 0
            ;
                Return_bool = 1
            )
        )
    ;
        throw(error(wrong_input_number, Num, verify_degrees/2))
    ).

/* Verify if the Primes of Detection Are Real.
 * Input: A Integer Number.
 * Output: A Boolean that is 1 If the Primes Are Real, 0 Otherwise.*/
verify_primes(Num, Return_bool) :-
    (integer(Num) -> 
        (Num < 0 -> 
            Return_bool = 0
        ;
            (Num > 59 -> 
                Return_bool = 0
            ;
                Return_bool = 1
            )
        )  
    ;
        throw(error(wrong_input_number, Num, verify_primes/2))
    ).

/* Verify if the Latters of Detection Are Real.
 * Input: A Integer or Float Number.
 * Output: A Boolean that is 1 If the Latters Are Real, 0 Otherwise.*/
verify_latters(Num, Return_bool) :-
    (number(Num) ->
        (Num < 0 -> 
            Return_bool = 0
        ;
            (Num > 59 -> 
                Return_bool = 0
            ;
                Return_bool = 1
            )
        )
    ;
        throw(error(wrong_input_number, Num, verify_primes/2))
    ).

/* Verify if the Latitude Sign of Detection is Right.
 * Input: A Letter.
 * Output: A Boolean that is 1 If the Letter Is Right, 0 Otherwise.*/
verify_latitude(Letter, Return_bool) :-
    (nonvar(Letter) -> 
        (Letter == 'N' -> 
            Return_bool = 1
        ;
            (Letter == 'S' -> 
                Return_bool = 1
            ;
                Return_bool = 0
            )
        )
    ;
        throw(error(no_input_letter, verify_latitude/2))
    ).

/* Verify if the Longitude Sign of Detection is Right.
 * Input: A Letter.
 * Output: A Boolean that is 1 If the Letter Is Right, 0 Otherwise.*/
verify_longitude(Letter, Return_bool) :-
    (nonvar(Letter) -> 
        (Letter == 'E' -> 
            Return_bool = 1
        ;
            (Letter == 'W' -> 
                Return_bool = 1
            ;
                Return_bool = 0
            )
        )
    ;
        throw(error(wrong_input_list, verify_longitude/2))
    ).

/* Remove the Latitude string Part From the Detection string, Return the Longitude string Part.
 * Input: A List.
 * Output: A List containing the longitude string part.*/
split([], _) :-
    throw(error(empty_input_list, split/2)).
split(List, Final_list) :-
    (list(List) -> 
        length(List, Len),
        (Len >= 17 ->
            drop(17, List, Final_list)
        ;
            atom_chars(Print, List),
            throw(error(input_list_has_not_enought_elements, Print, split/2))
        )
    ;
        throw(error(wrong_input_list, List, split/2))
    ).    

/* Tranform the Input String Containign the Latitude part into a Latitude List, [Sign, Degrees, Primes, Latters].
 * Input: A List.
 * Output: A List containing the latide.*/
get_latitude([], _) :-
    throw(error(empty_input_list, get_latitude/2)).
get_latitude(List, Final_list) :-
    (list(List) -> 
        verify_lenght(List, N),
        ( N == 0 ->
            atom_chars(Print, List),
            throw(error(invalid_argument, Print, getLatitude/2))
        ;
            head(List, Sign),
            drop(2, List, A),
            take(2, A, B), 
            number_chars(Degrees, B),
            take(7, List, C),
            drop(5, C, D),
            number_chars(Primes, D),
            take(14, List, E),
            drop(8, E, F),
            number_chars(Latters, F),
            Final_list = [Sign, Degrees, Primes, Latters]
        )
    ;
        throw(error(wrong_input_list, List, get_latitude/2))
    ).

/* Tranform the Input String Containign the Longitude part into a Longitude List, [Sign, Degrees, Primes, Latters].
 * Input: A List.
 * Output: A List containing the longitude.*/
get_longitude([], _) :-
    throw(error(empty_input_list, get_longitude/2)).
get_longitude(List, Final_list) :-
    (list(List) -> 
        verify_lenght(List, N),
        ( N == 0 ->
            atom_chars(Print, List),
            throw(error(invalid_argument, Print, getLongitude/2))
        ;
            split(List, List1),
            head(List1, Sign),
            drop(2, List1, A),
            take(2, A, B), 
            number_chars(Degrees, B),
            take(7, List1, C),
            drop(5, C, D),
            number_chars(Primes, D),
            take(14, List1, E),
            drop(8, E, F),
            number_chars(Latters, F),
            Final_list = [Sign, Degrees, Primes, Latters]
        )
    ;
        throw(error(wrong_input_list, List, get_longitude/2))
    ).

/* Verify if the Coordinate Body is Right,
   (E.g. in the latitude case the body is the entire coordinate without the sign),
   This predicate is linked with get_point/2 in way to not write duplicate code.
 * Input: A List containing a latitude/longitude.
 * Output: A Boolean that is 1 If the Body is Right, 0 Otherwise.*/
verify_coordinate_body([], _) :-
    throw(error(empty_input_list, verify_coordinate_body/2)).
verify_coordinate_body(List, Return_bool) :-
    (list(List) -> 
        index(1, List, Degrees),
        verify_degrees(Degrees, B),
        (B == 0 -> 
            Return_bool = 0,
            throw(error(wrong_degrees_in, List, verify_coordinate_body/2))
        ;
            index(2, List, Primes),
            verify_primes(Primes, B1),
            (B1 == 0 ->
                Return_bool = 0,
                throw(error(wrong_primes_in, List, verify_coordinate_body/2))
            ;
                index(3, List, Latters),
                verify_latters(Latters, B2),
                (B2 == 0 -> 
                    Return_bool = 0,
                    throw(error(wrong_latters_in, List, verify_coordinate_body/2))
                ;
                    Return_bool = 1
                )
            )
        )
    ;
        throw(error(wrong_input_list, List, verify_coordinate_body/2))
    ).

/* Covert the Sign of the Coordinate Into a Number For the Decimal Conversion the Coordinate.
 * Input: A Letter.
 * Output: A Integer number.*/
check_sign(Letter, Return_num) :-
    (nonvar(Letter) ->
       (Letter == 'S' ->
            Return_num = (-1)
        ; 
            (Letter == 'W' -> 
                Return_num = (-1)
            ;
                Return_num = 1
            )
        )
    ;
        throw(error(no_input_letter, check_sign/2))
    ).

/* Covert a Coordinate (in D.M.G form) into Decimal form.
 * Input: A List containing a coordinate.
 * Output: A Integer number containing the coordinate in decimal form.*/
convert_to_decimal([], _) :-
    throw(error(empty_input_list, convert_to_decimal/2)).
convert_to_decimal(List, Return_num) :-
    (list(List) -> 
        index(0, List, Sign),
        check_sign(Sign, Sign1),
        index(1, List, Degrees),
        index(2, List, Primes),
        index(3, List, Latters),
        A is Latters / 60,
        B is Primes + A,
        C is B / 60,
        D is C + Degrees,
        Return_num is D * Sign1
    ;
        throw(error(wrong_input_list, List, convert_to_decimal/2))
    ).
    
/* Merge Two Numbers into a List that contain both.
 * Input: Two Integer/Float numbers.
 * Output: A List containing both the input numbers.*/
merge_coordinates(Num1, Num2, Final_list) :-
    (number(Num1) -> 
        (number(Num2) -> 
            Final_list = [Num1, Num2]
        ;
            throw(error(wrong_input_second_number, Num2, merge_coordinates/3))
        )
    ;
        throw(error(wrong_input_first_number, Num1, merge_coordinates/3))
    ).

/* Covert a Detection (in D.M.G form) into Decimal form.
 * Input: A List containing a detection.
 * Output: A List containing the detection in decimal form.*/
get_point([], _) :-
    throw(error(empty_input_list, get_point/2)).
get_point(List, Final_list) :-
    (list(List) -> 
        get_latitude(List, Latitude),
        index(0, Latitude, Sign1),
        verify_latitude(Sign1, B1),
        verify_coordinate_body(Latitude, _),
        get_longitude(List, Longitude),
        index(0, Longitude, Sign2),
        verify_longitude(Sign2, B2),
        verify_coordinate_body(Longitude, _),
        (B1 == 0 ->
            throw(error(wrong_sign_in, Latitude, get_point/2))
        ;
            (B2 == 0 ->
                throw(error(wrong_sign_in, Longitude, get_point/2))
            ;
                convert_to_decimal(Latitude, Dlatitude),
                convert_to_decimal(Longitude, Dlongitude),
                merge_coordinates(Dlatitude, Dlongitude, Final_list)
            )
        )
    ;
        throw(error(wrong_input_list, List, get_point/2))
    ).
    
/***** End Module *****/

/***** Properties Module *****/

/* Calculate the Distance Between two Detections in Decimal form.
 * Input: Two Lists containing detections in decimal form.
 * Output: A Integer number that represent the distance (in Km) between the two detections.*/
distance([], _, _) :-
    throw(error(empty_first_input_list, distance/3)).
distance(_, [], _) :-
    throw(error(empty_second_input_list, distance/3)).
distance(List1, List2, Rn) :-
    (list(List1) -> 
        (list(List2) ->
            index(0,List1,Lat1),
            (number(Lat1) -> 
                index(1,List1,Long1),
                (number(Long1) -> 
                    index(0,List2,Lat2),
                    (number(Lat2) -> 
                        index(1,List2,Long2),
                        (number(Long2) -> 
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
                            Rn is 6372.795477598 * N
                        ;
                            throw(error(expected_number, Long2, distance/3))
                        )
                    ;
                        throw(error(expected_number, Lat2, distance/3))
                    )
                ;
                    throw(error(expected_number, Long1, distance/3))
                )
            ;
                throw(error(expected_number, Lat1, distance/3))
            )
        ;
            throw(error(wrong_second_input_list, List2, distance/3))
        )
    ;
        throw(error(wrong_first_input_list, List1, distance/3))
    ).

/* Calculate the Direction Between two Detections in Decimal form.
 * Input: Two Lists containing detections in decimal form.
 * Output: A Integer number that represent the direction (in degrees) between the two detections.*/
direction([], _, _) :-
    throw(error(empty_first_input_list, direction/3)).
direction(_, [], _) :-
    throw(error(empty_second_input_list, direction/3)).
direction(List1, List2, Return_num) :-
    (list(List1) -> 
        (list(List2) -> 
            index(0,List1,Lat1),
            (number(Lat1) -> 
                index(1,List1,Long1),
                (number(Long1) -> 
                    index(0,List2,Lat2),
                    (number(Lat2) -> 
                        index(1,List2,Long2),
                        (number(Long2) -> 
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
                            Return_num is P / pi * 180
                        ;
                            throw(error(expected_number, Long2, direction/3))
                        )
                    ;
                        throw(error(expected_number, Lat2, direction/3))
                    )
                ;
                    throw(error(expected_number, Long1, direction/3))
                )
            ;
                throw(error(expected_number, Lat1, direction/3))
            )
        ;
            throw(error(wrong_second_input_list, List2, direction/3))
        )
    ;
        throw(error(wrong_first_input_list, List1, direction/3))
    ).
    
/* Calculate the Inverse Direction Between two Detections in Decimal form.
 * Input: Two Lists containing detections in decimal form.
 * Output: A Integer number that represent the inverse direction (in degrees) between the two detections.*/
inverse_direction([], _, _) :-
    throw(error(empty_first_input_list, inverse_direction/3)).
inverse_direction(_, [], _) :-
    throw(error(empty_second_input_list, inverse_direction/3)).
inverse_direction(List1, List2, Return_num) :-
    (list(List1) -> 
        (list(List2) -> 
            direction(List1, List2, Dir),
            Return_num is Dir + 180
        ;
            throw(error(wrong_second_input_list, List2, inverse_direction/3))
        )
    ;
        throw(error(wrong_first_input_list, List1, inverse_direction/3))
    ).
    
/***** End Module *****/

/***** Main & Main Util Module*****/

/* Main. */
main :-
    write('Detections Properties Calculator V1.0'), nl,
    write('Waring: The Detections must be in D.M.G format and inserted into the program like: `N 40 45 36.000 - E 73 59 2.400`.'), nl,
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
/***** End Module*****/