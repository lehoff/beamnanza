-module(bn_must_plant).

-export([add/2,
         contents/1,
         take/2]).

-export([start_link/0,
         stop/1]).

-behaviour(gen_server).

-export([init/1,
         handle_cast/2,
         handle_call/3,
         terminate/2,
         code_change/3,
         handle_info/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type must_plant() :: pid().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec add(must_plant(), bn_beans:bean() | [bn_beans:bean()]) -> 'ok'.
add(MustPlant, Beans) ->
    gen_server:cast(MustPlant, {add, Beans}).

-spec contents(must_plant()) -> [bn_beans:bean()].
contents(MustPlant) ->
    gen_server:call(MustPlant, contents).

-spec take(must_plant(), bn_beans:bean()) -> bn_beans:count() | 
                                             'no_such_bean' |
                                             'empty'.
take(MustPlant, Bean) ->
    gen_server:call(MustPlant, {take, Bean}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link() ->
    gen_server:start_link(?MODULE, [], []).

-spec stop(must_plant()) -> 'ok' | 'not_empty'.
stop(MustPlant) ->
    gen_server:call(MustPlant, stop).
     

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([]) ->
    {ok, []}.

handle_call(contents, _From, Contents) ->
    {reply, Contents, Contents};
handle_call({take, _Bean}, _From, []) ->
    {reply, empty, []};
handle_call({take, Bean}, _From, Contents) ->
    {ToTake, Remaining} = 
        lists:partition( fun(B) ->
                                 B == Bean
                         end,
                         Contents),
    case ToTake of 
        [] ->
            {reply, no_such_bean, Contents};
        _ ->
            Count = length(ToTake),
            {reply, Count, Remaining}
    end;
handle_call(stop, _From, []) ->
    {stop, normal, ok, []};
handle_call(stop, _From, Contents) ->
    {reply, not_empty, Contents}.

handle_cast({add, Beans}, Contents) when is_list(Beans) ->
    {noreply, Beans ++ Contents};
handle_cast({add, Bean}, Contents) ->
    {noreply, [Bean | Contents]}.

handle_info(Msg, State) ->
    {stop, {handle_info_not_implemented, Msg}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
