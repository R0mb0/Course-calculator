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