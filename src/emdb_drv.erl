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

-module(emdb_drv).

%%====================================================================
%% EXPORTS
%%====================================================================
-export([
         open/1,
         close/1,

         put/3,
         get/2,
         del/2,

         update/3,

         drop/1
        ]).


%% internal export (ex. spawn, apply)
-on_load(init/0).

%%====================================================================
%% MACROS
%%====================================================================
-define(EMDB_DRIVER_NAME, "emdb_drv").
-define(NOT_LOADED, not_loaded(?LINE)).


%%====================================================================
%% PUBLIC API
%%====================================================================

%%--------------------------------------------------------------------
%% @doc 
%% @end
%%--------------------------------------------------------------------
open(_DirName) ->
    ?NOT_LOADED.

%%--------------------------------------------------------------------
%% @doc 
%% @end
%%--------------------------------------------------------------------
close(_Handle) ->
    ?NOT_LOADED.

%%--------------------------------------------------------------------
%% @doc 
%% @end
%%--------------------------------------------------------------------
put(_Handle, _Key, _Val) ->
    ?NOT_LOADED.


%%--------------------------------------------------------------------
%% @doc 
%% @end
%%--------------------------------------------------------------------
get(_Handle, _Key) ->
    ?NOT_LOADED.

%%--------------------------------------------------------------------
%% @doc 
%% @end
%%--------------------------------------------------------------------
del(_Handle, _Key) ->
    ?NOT_LOADED.


%%--------------------------------------------------------------------
%% @doc 
%% @end
%%--------------------------------------------------------------------
update(_Handle, _Key, _Val) ->
    ?NOT_LOADED.

%%--------------------------------------------------------------------
%% @doc 
%% @end
%%--------------------------------------------------------------------
drop(_Handle) ->
    ?NOT_LOADED.

%%====================================================================
%% PRIVATE API
%%====================================================================

%%--------------------------------------------------------------------
%% @doc 
%% @end
%%--------------------------------------------------------------------
init() ->
    PrivDir = case code:priv_dir(?MODULE) of
        {error, _} ->
            EbinDir = filename:dirname(code:which(?MODULE)),
            AppPath = filename:dirname(EbinDir),
            filename:join(AppPath, "priv");
        Path ->
            Path
    end,
    erlang:load_nif(filename:join(PrivDir, ?EMDB_DRIVER_NAME), 0).


%%--------------------------------------------------------------------
%% @doc 
%% @end
%%--------------------------------------------------------------------
not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).
