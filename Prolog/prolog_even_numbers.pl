% Good Code:  Efficiently finds even numbers in a list.

even_numbers([], []).
even_numbers([H|T], [H|R]) :- H mod 2 =:= 0, even_numbers(T, R).
even_numbers([H|T], R) :- H mod 2 =\= 0, even_numbers(T, R).


% Bad Code: Inefficient and uses unnecessary cuts.

bad_even_numbers([], []).
bad_even_numbers([H|T], [H|R]) :- H mod 2 =:= 0, !, bad_even_numbers(T, R).
bad_even_numbers([H|T], R) :- H mod 2 =\= 0, !, bad_even_numbers(T, R).

