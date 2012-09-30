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

-module(emdb_oop, [Handle]).


%%====================================================================
%% EXPORTS
%%====================================================================
-export([
         close/0,

         put/2,
         get/1,
         del/1,

         update/2,

         drop/0
        ]).


%%====================================================================
%% PUBLIC API
%%====================================================================

%%--------------------------------------------------------------------
%% @doc 
%% @end
%%--------------------------------------------------------------------
close() ->
    emdb_drv:close(Handle).

%%--------------------------------------------------------------------
%% @doc 
%% @end
%%--------------------------------------------------------------------
put(Key, Val) when is_binary(Key) andalso is_binary(Val) ->
    emdb_drv:put(Handle, Key, Val).


%%--------------------------------------------------------------------
%% @doc 
%% @end
%%--------------------------------------------------------------------
get(Key) when is_binary(Key) ->
    emdb_drv:get(Handle, Key).

%%--------------------------------------------------------------------
%% @doc 
%% @end
%%--------------------------------------------------------------------
del(Key) when is_binary(Key) ->
    emdb_drv:del(Handle, Key).

%%--------------------------------------------------------------------
%% @doc 
%% @end
%%--------------------------------------------------------------------
update(Key, Val) when is_binary(Key) andalso is_binary(Val) ->
    emdb_drv:update(Handle, Key, Val).


%%--------------------------------------------------------------------
%% @doc 
%% @end
%%--------------------------------------------------------------------
drop() ->
    emdb_drv:drop(Handle).

%%====================================================================
%% INTERNAL FUNCTIONS
%%====================================================================
