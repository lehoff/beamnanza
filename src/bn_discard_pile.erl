-module(bn_discard_pile).

-export([add/2,
         empty/1]).

-export([start_link/0]).

-behaviour(gen_server).

-export([init/1,
         handle_cast/2,
         handle_call/3,
         terminate/2,
         code_change/3,
         handle_info/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type discard_pile() :: pid().

-export_type([discard_pile/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec add(discard_pile(), bn_beans:bean() | [bn_beans:bean()]) -> ok.
add(DiscardPile, Beans) ->
    gen_server:cast(DiscardPile, {add, Beans}).

-spec empty(discard_pile()) -> [bn_beans:bean()].
empty(DiscardPile) ->
    gen_server:call(DiscardPile, empty).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([]) ->
    {ok, []}.

handle_cast({add, Beans}, Contents) when is_list(Beans) ->
    {noreply, Beans ++ Contents};
handle_cast({add, Bean}, Contents) ->
    {noreply, [Bean|Contents]}.

handle_call(empty, _From, Contents) ->
    {reply, Contents, []}.

handle_info(Msg, State) ->
    {stop, {handle_info_not_implemented, Msg}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


