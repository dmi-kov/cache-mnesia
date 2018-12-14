%%%-------------------------------------------------------------------
%%% @author dmitry
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Dec 2018 13:32
%%%-------------------------------------------------------------------
-module(cache_db).
-author("dmitry").

%% API
-export([init/0, get/2, set/4, delete/1]).

-include("../include/cache.hrl").

%%%===================================================================
%%% API
%%%===================================================================
-spec(init() -> {atomic, ok} | {aborted, Error :: term()}).
init() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(
        cache_table,
        [
            {ram_copies, [node()]},
            {attributes, record_info(fields, cache_table)}
        ]
    ).

-spec(set(Key :: term(), Value :: term(), Timestamp :: non_neg_integer(),
    TTL :: non_neg_integer()) -> {atomic, ok} | {aborted, Error :: term()}).
set(Key, Value, TTL, Timestamp) ->
    Fun = fun() ->
        mnesia:write(
            #cache_table{
                key = Key,
                value = Value,
                ttl = TTL,
                timestamp = Timestamp
            })
          end,
    mnesia:transaction(Fun).

-spec(get(Key :: term(), Timestamp :: non_neg_integer()) -> {atomic, Value :: list()} | {aborted, Error :: term()}).
get(Key, Current_ts) ->
    MatchHead = #cache_table{key = '$1', value = '$2', ttl = '$3', timestamp = '$4'},
    Guard =
        {'orelse',
            {'andalso',
                {'=:=', '$1', Key},
                {'=:=', '$3', infinity}},
            {'andalso',
                {'=:=', '$1', Key},
                {'>',
                    {'+', '$3', '$4'},
                    Current_ts}}},
    Result = '$2',
    Fun = fun() ->
            mnesia:select(cache_table, [{MatchHead, [Guard], [Result]}])
          end,
    mnesia:transaction(Fun).

-spec(delete(Timestamp :: non_neg_integer()) -> {atomic, ok} | {aborted, Error :: term()}).
delete(Current_ts) ->
    case read_keys(Current_ts) of
        {atomic, []} -> ok;
        {atomic, Keys} ->
            DeleteFun =
                fun() ->
                    lists:foreach(
                        fun(Key) ->
                            mnesia:delete({cache_table, Key})
                        end, Keys)
                end,
            mnesia:transaction(DeleteFun)
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
read_keys(Current_ts) ->
    MatchHead = #cache_table{key = '$1', value = '_', ttl = '$2', timestamp = '$3'},
    Guard =
        {'andalso',
            {'=/=', '$2', infinity},
            {'<',
                {'+', '$2', '$3'},
                Current_ts}},
    Result = '$1',
    Fun = fun() ->
        mnesia:select(cache_table, [{MatchHead, [Guard], [Result]}])
          end,
    mnesia:transaction(Fun).