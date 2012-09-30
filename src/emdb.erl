%%-------------------------------------------------------------------
%% This file is part of EMDB - Erlang MDB API
%%
%% Copyright (c) 2012 by Aleph Archives. All rights reserved.
%%
%%-------------------------------------------------------------------
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted only as authorized by the OpenLDAP
%% Public License.
%%
%% A copy of this license is available in the file LICENSE in the
%% top-level directory of the distribution or, alternatively, at
%% <http://www.OpenLDAP.org/license.html>.
%%
%% Permission to use, copy, modify, and distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%%-------------------------------------------------------------------

-module(emdb).


%%====================================================================
%% EXPORTS
%%====================================================================
-export([
         open/1,
         open/2,
         open/3
        ]).


%%====================================================================
%% Includes
%%====================================================================
-include("emdb.hrl").


%%====================================================================
%% Macros
%%====================================================================
-define(MDB_MAP_SIZE, 10485760). %% 10MB

%%====================================================================
%% PUBLIC API
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Create a new MDB database
%% @end
%%--------------------------------------------------------------------
open(DirName) ->
    open(DirName, ?MDB_MAP_SIZE).
open(DirName, MapSize) when is_integer(MapSize) andalso MapSize > 0 ->
    open(DirName, MapSize, 0).
open(DirName, MapSize, EnvFlags) when is_integer(MapSize) andalso MapSize > 0 andalso is_integer(EnvFlags) andalso EnvFlags >= 0 ->
    %% ensure directory exists
    ok = filelib:ensure_dir(DirName ++ "/"),
    decorate(emdb_drv:open(DirName, MapSize, EnvFlags)).

%%====================================================================
%% PRIVATE API
%%====================================================================

%% @private
decorate({ok, Handle}) ->
    CDB = emdb_oop:new(Handle),
    {ok, CDB};

decorate(Error) ->
    Error.
