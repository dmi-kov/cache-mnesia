%%%-------------------------------------------------------------------
%%% @author dmitry
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Dec 2018 13:18
%%%-------------------------------------------------------------------
-module(cache).
-author("dmitry").

-behaviour(gen_server).

%% API
-export([start_link/0, get/1, set/2, set/3, delete_cache/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-include("../include/cache.hrl").

%%%===================================================================
%%% API
%%%===================================================================

-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->

    cache_db:init(),
    timer:apply_interval(?DELETE_INTERVAL * 1000, ?MODULE, delete_cache, []),

    {ok, #state{}}.

%% gen_server api
get(Key) ->
    gen_server:call(?MODULE, #'call.cache.get'{key = Key}).

set(Key, Value) ->
    gen_server:cast(?MODULE, #'cast.cache.set'{key = Key, value = Value, ttl = ?DEFAULT_TTL}).

set(Key, Value, [{ttl, TTL}]) when TTL =:= 0 orelse TTL =:= infinity ->
    gen_server:cast(?MODULE, #'cast.cache.set'{key = Key, value = Value, ttl = infinity});

set(Key, Value, [{ttl, TTL}]) ->
    gen_server:cast(?MODULE, #'cast.cache.set'{key = Key, value = Value, ttl = TTL}).


-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}}).
handle_call(#'call.cache.get'{key = Key}, _From, State) ->

    Reply =
        case cache_db:get(Key, get_timestamp()) of
            {atomic, [Value]} -> {ok, Value};
            {atomic, []} -> {error, not_found};
            {aborted, _} -> {error, internal_error} %%TODO add logging errors
        end,

    {reply, Reply, State}.

-spec(handle_cast(Request :: term(), State :: #state{}) -> {noreply, NewState :: #state{}}).
handle_cast(#'cast.cache.set'{key = Key, value = Value, ttl = infinity = TTL}, State) ->

        case cache_db:set(Key, Value, TTL, get_timestamp()) of
            {atomic, ok} -> ok;
            {aborted, _Reason} -> {error, internal_error}
        end,

    {noreply, State};

handle_cast(#'cast.cache.set'{key = Key, value = Value, ttl = TTL}, State) ->

        case cache_db:set(Key, Value, TTL, get_timestamp()) of
            {atomic, ok} -> ok;
            {aborted, _Reason} -> {error, internal_error}
        end,

    {noreply, State}.

-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}}).
handle_info(#'info.cache.delete'{}, State) ->

    cache_db:delete(get_timestamp()),

    {noreply, State}.

-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) -> ok.

-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, _State, _Extra) -> throw(not_implemented).

%%%===================================================================
%%% Internal functions
%%%===================================================================
delete_cache() -> ?MODULE ! #'info.cache.delete'{}.

get_timestamp() -> {Mega, Secs, _} = erlang:timestamp(), Mega * 1000000 + Secs.