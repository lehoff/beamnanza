-module(bn_earnings).

-export([add/2,
         buy_bean_field/1,
         bean_field_count/1,
         bean_field_price/1,
         total/1
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
-type earnings() :: pid().

-export_type([earnings/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec add(earnings(), bn_field:harvest_result()) -> bn_beans:count().
add(Earnings, HarvestResult) ->
    gen_server:call(Earnings, {harvest_result, HarvestResult}).

-spec buy_bean_field(earnings()) -> [bn_beans:bean()] | 
                                    'not_enough_gold' | 
                                    'cannot_buy_another_field'.
buy_bean_field(Earnings) ->
    gen_server:call(Earnings, buy_bean_field).

-spec bean_field_count(earnings()) -> 2..3.
bean_field_count(Earnings) ->
    gen_server:call(Earnings, bean_field_count).

-spec bean_field_price(earnings()) -> 2..3.
bean_field_price(Earnings) ->
    gen_server:call(Earnings, bean_field_price).

-spec total(earnings()) -> beamnanza:gold().
total(Earnings) ->
    gen_server:call(Earnings, total).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec start_link(beamnanza:player_count()) -> {'ok', pid()} |  
                                              {'stop', term()} |
                                              'ignore'.
start_link(PlayerCount) ->
    gen_server:start_link(?MODULE, [PlayerCount], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(state,
        {beans = [] :: [bn_beans:bean()],
         gold = 0 :: beamnanza:gold(),
         player_count :: beamnanza:player_count(),
         bean_fields :: 2..3
        }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([PlayerCount]) ->
    InitialBeanFieldCount = initial_bean_field_count(PlayerCount),
    {ok, #state{ player_count = PlayerCount,
                 bean_fields = InitialBeanFieldCount}}.

handle_call({harvest_result, {Bean, Count}}, _From, 
            #state{gold = Gold,
                   beans = Beans} = State) ->
    Profit = bn_beans:value(Bean, Count),
    NewBeans = add_beans(Profit, Bean, Beans),
    DiscardCount = Count - Profit,
    DiscardBeans = lists:duplicate(DiscardCount, Bean),
    {reply, DiscardBeans, State#state{beans = NewBeans,
                                      gold = Gold + Profit}};
handle_call(buy_bean_field, _From, #state{bean_fields = 3} = State) ->
    {reply, cannot_buy_another_field, State};
handle_call(buy_bean_field, _From, 
            #state{player_count = PlayerCount,
                   gold = Gold,
                   beans = Beans,
                   bean_fields = NoFields} = State) ->
    FieldPrice = field_price(PlayerCount),
    case Gold >= FieldPrice of
        true ->
            {ReturnedBeans, NewBeans} = lists:split(FieldPrice, Beans),
            {reply, ReturnedBeans, State#state{gold = Gold - FieldPrice,
                                               beans = NewBeans,
                                               bean_fields = NoFields + 1}};
        false ->
            {reply, not_enough_gold, State}
    end;
handle_call(bean_field_count, _From, #state{bean_fields = Count} = State) ->
    {reply, Count, State};
handle_call(bean_field_price, _From, #state{player_count = PlayerCount} = State) ->
    {reply, field_price(PlayerCount), State};
handle_call(total, _From, #state{gold = Gold} = State) ->
    {reply, Gold, State}.

handle_cast(Msg, State) ->
    {stop, {cast_not_implemented, Msg}, State}.

handle_info(Msg, State) ->
    {stop, {handle_info_not_implemented, Msg}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
initial_bean_field_count(3) -> 3;
initial_bean_field_count(_) -> 2.
    
field_price(NoPlayers) when NoPlayers == 6;
                            NoPlayers == 7 -> 2;
field_price(_) -> 3.

add_beans(Count, Bean, Beans) ->
    lists:duplicate(Count, Bean) ++ Beans.
