-module(bn_beans).

-export([types/1,
         all/1,
         count/1,
         max_harvest/1,
         value/2,
         payoff/1]).

-type bean() :: 'cocoa' | 'garden' | 'red' | 'black_eyed' |
                'soy' | 'green' | 'stink' | 'chili' |
                'blue' | 'wax' | 'coffee'.

-type count() :: 0..24.

-export_type([bean/0,
              count/0]).


-spec types(beamnanza:player_count()) -> [bean()].
types(3) ->
    all_types() -- [cocoa];
types(N) when N==4; N==5 ->
    all_types() -- [coffee];
types(N) when N==6; N==7 ->
    all_types() -- [cocoa, garden].

-spec all(beamnanza:player_count()) -> [bean()].
all(NoPlayers) ->
    Types = types(NoPlayers),
    BeanLists = [ lists:duplicate(count(Bean), Bean) ||
                    Bean <- Types ],
    lists:flatten(BeanLists).

-spec count(bean()) -> count().
count(Type) ->
    2 * (index(Type, all_types()) +1).

-spec max_harvest(bean()) -> count().
max_harvest(Bean) ->
    {Count, _Gold} = lists:last(payoff(Bean)),
    Count.
    

-spec value(bean(), count()) -> beamnanza:gold().
value(Bean, Count) ->
    Payoff = payoff(Bean),
    lookup(Count, Payoff).
    
-spec payoff(bean()) -> [{count(), beamnanza:gold()}].
payoff(cocoa) ->
    [{2,2}, {3,3}, {4,4}];
payoff(garden) ->
    [{2,2}, {3,3}];
payoff(red) ->
    standard_payoff([2,3,4,5]);
payoff(black_eyed) ->
    standard_payoff([2,4,5,6]);
payoff(soy) ->
    standard_payoff([2,4,6,7]);
payoff(green) ->
    standard_payoff([3,5,6,7]);
payoff(stink) ->
    standard_payoff([3,5,7,8]);
payoff(chili) ->
    standard_payoff([3,6,8,9]);
payoff(blue) ->
    standard_payoff([4,6,8,10]);
payoff(wax) ->
    standard_payoff([4,7,9,11]);
payoff(coffee) ->
    standard_payoff([4,7,10,12]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
all_types() ->
    [cocoa, garden, red, black_eyed, soy, green, stink, chili, 
     blue, wax, coffee].

index(Elem, List) ->
    index(Elem, List, 1).

index(X, [X|_Xs], Index) ->
    Index;
index(X, [_|Xs], Index) ->
    index(X, Xs, Index +1).

standard_payoff(List) ->
    lists:zip(List, lists:seq(1,4)).

lookup(Count, Payoff) ->
    lookup2(Count, lists:reverse(Payoff)).

lookup2(Count, [{C, P}|_]) when Count >= C ->
    P;
lookup2(Count, [_|Payoff]) ->
    lookup2(Count, Payoff);
lookup2(_, []) ->
    0.
