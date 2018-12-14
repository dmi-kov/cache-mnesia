%%%-------------------------------------------------------------------
%%% @author dmitry
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Dec 2018 13:41
%%%-------------------------------------------------------------------
-author("dmitry").

-define(DEFAULT_TTL, 60).  %% default TTL in seconds
-define(DELETE_INTERVAL, 10). %% set interval in seconds to delete expired cache

-record(state, {}).
-record(cache_table, {key :: term(), value :: term(), ttl :: non_neg_integer(), timestamp :: non_neg_integer()}).

%% gen_server protocol
-record('cast.cache.set', {key :: term(), value :: term(), ttl :: [non_neg_integer() | atom()]}).

-record('call.cache.get', {key :: term()}).

-record('info.cache.delete', {}).


