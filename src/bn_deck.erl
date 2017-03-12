-module(bn_deck).

-export([take/1,
         remaining/1,
         exhaust_count/1,
         refill/2
        ]).

-export([start_link/1]).

-behaviour(gen_server).

-export([init/1,
         handle_cast/2,
         handle_call/3,
         terminate/2,
         code_change/3,
         handle_info/2]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type deck() :: pid().

-export_type([deck/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(state,
        {contents :: queue:queue(bn_beans:bean()),
         exhaust_count = 0 :: 0..3
        }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec take(deck()) -> bn_beans:bean() | 'empty'.
take(Deck) ->
    gen_server:call(Deck, draw).

-spec remaining(deck()) -> non_neg_integer().
remaining(Deck) ->
    gen_server:call(Deck, remaining).

-spec exhaust_count(deck()) -> 0..3.
exhaust_count(Deck) ->
    gen_server:call(Deck, exhaust_count).

-spec refill(deck(), [bn_beans:bean()]) -> ok | not_empty.
refill(Deck, Beans) ->
    gen_server:call(Deck, {refill, Beans}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link(Beans) ->
    gen_server:start_link(?MODULE, [Beans], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([Beans]) ->
    Contents = queue:from_list(shuffle(Beans)),
    {ok, #state{contents = Contents}}.

handle_call(draw, _From, #state{contents=Contents} = State) ->
    case queue:out(Contents) of
        {{value, Bean}, NewContents} ->
            {reply, Bean, State#state{contents = NewContents}};
        {empty, _} ->
            {reply, empty, State}
    end;
handle_call(remaining, _From, #state{contents = Contents} = State) ->
    {reply, queue:len(Contents), State};
handle_call(exhaust_count, _From, #state{exhaust_count = ExhaustCount} = State) ->
    {reply, ExhaustCount, State};
handle_call({refill, Beans}, _From, #state{contents = Contents} = State) ->
    case queue:is_empty(Contents) of
        true ->
            NewContents = queue:from_list(shuffle(Beans)),
            {reply, ok, State#state{contents = NewContents,
                                    exhaust_count = State#state.exhaust_count + 1}};
        false ->
            {reply, not_empty, State}
    end.


handle_cast(Msg, State) ->
    {stop, {cast_not_implemented, Msg}, State}.

handle_info(Msg, State) ->
    {stop, {handle_info_not_implemented, Msg}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


shuffle(List) ->
    Tagged = [ {rand:uniform(), N} ||
                 N <- List ],
    TagSorted = lists:sort(Tagged),
    {_,Sorted} = lists:unzip(TagSorted), 
    Sorted.


