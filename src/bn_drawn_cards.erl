-module(bn_drawn_cards).

-export([contents/1,
         take/2]).

-export([start_link/2,
         stop/1]).

-behaviour(gen_server).

-export([init/1,
         handle_cast/2,
         handle_call/3,
         terminate/2,
         code_change/3,
         handle_info/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type drawn_cards() :: pid().

-export_type([drawn_cards/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec take(drawn_cards(), bn_beans:bean()) -> 'ok' | 'no_such_bean' | 'empty'.
take(DrawnCards, Bean) ->
    gen_server:call(DrawnCards, {take, Bean}).

-spec contents(drawn_cards()) -> [bn_beans:bean()].
contents(DrawnCards) ->
    gen_server:call(DrawnCards, contents).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec start_link(bn_beans:bean(), bn_beans:bean()) -> {'ok', pid()} | 
                                                      {'stop', term()} |
                                                      'ignore'.
start_link(Bean1, Bean2) ->
    gen_server:start_link(?MODULE, [Bean1, Bean2], []).
                   
-spec stop(drawn_cards()) -> 'ok' | 'not_empty'.   
stop(DrawnCards) ->
    gen_server:call(DrawnCards, stop).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([_Bean1,_Bean2]=Beans) ->
    {ok, Beans}.

handle_call({take, _Bean}, _From, []) ->
    {reply, empty, []};
handle_call({take, Bean}, _From, Contents) ->
    case lists:member(Bean, Contents) of
        true ->
            {reply, ok, lists:delete(Bean, Contents)};
        false ->
            {reply, no_such_bean, Contents}
    end;
handle_call(contents, _From, Contents) ->
    {reply, Contents, Contents};
handle_call(stop, _From, []) ->
    {stop, normal, ok, []};
handle_call(stop, _From, Contents) ->
    {reply, not_empty, Contents}.

handle_cast(Msg, State) ->
    {stop, {cast_not_implemented, Msg}, State}.

handle_info(Msg, State) ->
    {stop, {handle_info_not_implemented, Msg}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
