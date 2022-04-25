/*****  *****/
/**  **/

/*Modules aren't officially poirtate*/

/***** List Tools Module *****/

/** Tool to  Remove Head's Elements from an Index **/
remove_head([_|Tail], Tail).

remove_from_head(N, Lst, Flst) :-
    ( N==0 ->
         Flst = Lst
    ;
        N1 is N -1,
        remove_head(Lst, Remain),
        remove_from_head(N1, Remain, Flst)
    ).

/** Tool to  Remove Tail's Elements from an Index **/
remove_tail(X,Y):-
    reverse(X,[_|X1]), reverse(X1,Y).

remove_from_tail(N, Lst, Flst) :-
    ( N==0 ->
         Flst = Lst
    ;
        N1 is N -1,
        remove_tail(Lst, Remain),
        remove_from_tail(N1, Remain, Flst)
    ).

/** Tool to  Maintain only Head's Elements from an Index **/
maintain_head(N, Lst, Flst) :-
    length(Lst, Len),
    ( Len==N ->
         Flst = Lst
    ;
        remove_tail(Lst, Remain),
        maintain_head(N, Remain, Flst)
    ).


/** Tool to  Maintain only Tail's Elements from an Index **/
maintain_tail(N, Lst, Flst) :-
    length(Lst, Len),
    ( Len==N ->
         Flst = Lst
    ;
        remove_head(Lst, Remain),
        maintain_tail(N, Remain, Flst)
    ).

/***** End *****/

/***** Detection Module *****/
split(Lst, Flst) :-
    remove_from_tail(17, Lst, Flst).
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
    atom_chars(A, Str),
    write('Insert index...'), nl,
    read(Index), nl,
    maintain_tail(Index, Str, Remain),
    write(Remain).


/***** End *****/