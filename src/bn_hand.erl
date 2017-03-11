-module(bn_hand).

-export([contents/1,
         take/1,
         add/2,
         trade/2]).

-export([start_link/0]).

-behaviour(gen_server).

-export([init/1,
         handle_cast/2,
         handle_call/3,
         terminate/2,
         code_change/3,
         handle_info/2]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type hand() :: pid().

-export_type([hand/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-spec contents(hand()) -> [bn_beans:bean()].
contents(Hand) ->
    gen_server:call(Hand, contents).

-spec take(hand()) -> bn_beans:bean() | 'empty'.
take(Hand) ->
    gen_server:call(Hand, take).

-spec add(hand(), bn_beans:bean()) -> 'ok'.
add(Hand, Bean) ->
    gen_server:cast(Hand, {add, Bean}).

-spec trade(hand(), [pos_integer()]) -> [bn_beans:bean()] | {'error', term()}.
trade(Hand, Indicies) ->
    gen_server:call(Hand, {trade, Indicies}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([]) ->
    {ok, queue:new()}.

handle_call(contents, _From, Contents) ->
    {reply, queue:to_list(Contents), Contents};
handle_call(take, _From, Contents) ->
    case queue:out(Contents) of 
        {empty, _} ->
            {reply, empty, Contents};
        {{value, Bean}, NewContents} ->
            {reply, Bean, NewContents}
    end;
handle_call({trade, Indicies}, _From, Contents) ->
    Cs = queue:to_list(Contents),
    case pick_indicies(Indicies, Cs) of
        {error, _} = Error ->
            {reply, Error, Contents};
        {Picked, NewContents} ->
            {reply, Picked, NewContents}
    end.

handle_cast({add, Bean}, Contents) ->
    {noreply, queue:in(Bean, Contents)}.

handle_info(Msg, State) ->
    {stop, {handle_info_not_implemented, Msg}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pick_indicies(Indicies, Cs) ->
    pick_indicies(lists:sort(Indicies), 1, Cs, [], queue:new()).


% All indices have been picked, roll up the rest.
pick_indicies([], _, Cs, Picked, Contents) ->
    NewContents = lists:foldl( fun(C, Acc) ->
                                       queue:in(C, Acc)
                               end,
                               Contents,
                               Cs ),
    {Picked, NewContents};
% Some indicies were too big for the input - this is an error.
pick_indicies(Is, _, [], _, _) ->
    {error, {too_big_indicies, Is}};
% We are at an index to include.
pick_indicies([I|Is], I, [C|Cs], Picked, Contents) ->
    pick_indicies(Is, I+1, Cs, [C|Picked], Contents);
% Skip this index.
pick_indicies(Is, I, [C|Cs], Picked, Contents) ->
    pick_indicies(Is, I+1, Cs, Picked, queue:in(C, Contents)). 
