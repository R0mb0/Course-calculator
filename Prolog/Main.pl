/***** Main & main util module. *****/

/*Verify if the two detections inserted are different.
* Input: Two Lists.
* Output An error if the two detections inserted aren't different.*/
verify_detections(List1, List2) :-
    List1 == List2,
    throw(error(inserted_the_same_detection_twice, verify_detections/2)).
verify_detections(List1, List2) :-
    (List1 \== List2).

/*Main.*/
main :-
    consult('ListTools.pl'),
    consult('Detection.pl'),
    consult('Properties.pl'),
    write('Detections Properties Calculator V1.0'), nl,
    write('Warning: the detections must be in D.M.G format and inserted into the program like: `N 40 45 36.000 - E 073 59 02.400`.'), nl,
    write('Insert the first detection...'), nl,
    read(A),
    atom_chars(A, Det1),
    write('Insert the second detection...'), nl,
    read(B),
    atom_chars(B, Det2),
    write('Proceed [yes./no.]?'), nl,
    read(C),
    (C == 'yes' ->
        verify_detections(Det1, Det2),
        write('First detection in decimal format ---> '),
        get_point(Det1, P1),
        nth(1, P1, Dlat1),
        nth(2, P1, Dlong1),
        format('~3f', [Dlat1]), write(','), format('~3f', [Dlong1]), nl,
        write('Second detection in decimal format ---> '),
        get_point(Det2, P2),
        nth(1, P2, Dlat2),
        nth(2, P2, Dlong2),
        format('~3f', [Dlat2]), write(','), format('~3f', [Dlong2]), nl,
        write('Distance between first & second detections ---> '),
        distance(P1, P2, Distance),
        format('~2f', [Distance]), write('Km'), nl,
        write('Positive direction between first & second detections ---> '),
        direction(P1, P2, Direction),
        format('~2f', [Direction]), write('°'), nl,
        write('Negative direction between first & second detections ---> '),
        inverse_direction(P1, P2, InverseDir),
        format('~2f', [InverseDir]), write('°')
    ;
        write('Aborted...')
    ).
/***** End module. *****/