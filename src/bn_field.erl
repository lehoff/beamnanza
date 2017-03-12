-module(bn_field).

-export([plant/2,
         beans/1,
         harvest/1]).

-export([start_link/0]).

-behaviour(gen_server).

-export([init/1,
         handle_cast/2,
         handle_call/3,
         terminate/2,
         code_change/3,
         handle_info/2]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(state,
        { bean_type :: bn_beans:bean(),
          count :: bn_beans:count()}).

-type state() :: #state{} | 'empty_field'.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type field() :: pid().
-type harvest_result() :: {bn_beans:bean(), NoBeansHarvested:: bn_beans:count()}. 
                                  

-export_type([field/0,
              harvest_result/0]).


-spec plant(field(), bn_beans:bean()) -> {'forced_harvest', harvest_result()} |
                                         {'planted', bn_beans:count()}.
plant(Field, Bean) ->
    gen_server:call(Field, {plant, Bean}).

-spec beans(field()) -> {bn_beans:bean(), bn_beans:count()} | 'empty'.
beans(Field) ->
    gen_server:call(Field, beans).

-spec harvest(field()) -> {'harvest', harvest_result()} | empty.
harvest(Field) ->
    gen_server:call(Field, harvest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link() ->
    gen_server:start_link(?MODULE, [], []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([]) ->
    {ok, empty_field}.

handle_call({plant, Bean}, _From, empty_field) ->
    {reply, {planted, 1}, #state{bean_type = Bean,
                                 count = 1}};
handle_call({plant, Bean}, _From, #state{bean_type = Bean}=State) ->
    NewCount = State#state.count + 1,
    case bn_beans:max_harvest(Bean) of
        NewCount ->
            {reply, {forced_harvest, {Bean, NewCount}}, empty_field};
        _ ->
            {reply, {planted, NewCount}, State#state{ count = NewCount}}
    end;
handle_call({plant, Bean}, _From, State) ->
    Reply = {forced_harvest, {State#state.bean_type, State#state.count}},
    NewState = #state{bean_type = Bean, count = 1},
    {reply, Reply, NewState};
handle_call(beans, _From, #state{}=State) ->
    {reply, {State#state.bean_type, State#state.count}, State};
handle_call(beans, _From, empty_field=State) ->
    {reply, empty, State};
handle_call(harvest, _From, empty_field=State) ->
    {reply, empty, State};
handle_call(harvest, _From, #state{}=State) ->
    {reply, {State#state.bean_type, State#state.count}, empty_field}.

handle_cast(Msg, State) ->
    {stop, {cast_not_implemented, Msg}, State}.

handle_info(Msg, State) ->
    {stop, {handle_info_not_implemented, Msg}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
