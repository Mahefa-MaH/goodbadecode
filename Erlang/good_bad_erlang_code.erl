-module(good_bad_code).
-export([good_code/0, bad_code/0]).

good_code() ->
    List = [1, 2, 3, 4, 5],
    lists:sum(List).

bad_code() ->
    List = [1,2,3,4,5],
    Sum = 0,
    Sum1 = Sum + lists:nth(1,List),
    Sum2 = Sum1 + lists:nth(2,List),
    Sum3 = Sum2 + lists:nth(3,List),
    Sum4 = Sum3 + lists:nth(4,List),
    Sum5 = Sum4 + lists:nth(5,List),
    Sum5.
