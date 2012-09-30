EMDB====EMDB is a NIF library for the [Memory-Mapped Database](http://highlandsun.com/hyc/mdb/) database, aka. MDB.The main purpose of this package is to provide a **very fast** Riak [backend](http://wiki.basho.com/Storage-Backends.html).

But this module could also be used as a general key-value store to replace:

* [DETS](http://www.erlang.org/doc/man/dets.html)
* TokyoCabinet: [TCERL](http://code.google.com/p/tcerl/)
* [QDBM](http://fallabs.com/qdbm/)
* [Bitcask](https://github.com/basho/bitcask)
* [eLevelDB](https://github.com/basho/eleveldb)
* [BerkleyDB](http://www.oracle.com/technetwork/products/berkeleydb/overview/index.html)
* ...Requirements------------
* Erlang R14B04+* GCC 4.2+ or MS VisualStudio 2010+Build-----$ makeAPI---
The following functions were implemented:

* `open/1`: creates a new MDB database. This call also re-open an already existing one.
* `close/2`: closes the database
* `put/2`: inserts Key with value Val into the database. Assumes that the key is not present, 'key_exit' is returned otherwise.
* `get/1`: retrieves the value stored with Key in the database.
* `del/1`: Removes the key-value with key Key from database.
* `update/2`: inserts Key with value Val into the database if the key is not present, otherwise updates Key to value Val.
* `drop/1`: deletes all key-value pairs in the database.


Usage-----$ make

$ ./start.sh

	%% create a new database	1> {ok, Handle} = emdb:open("/tmp/emdb1").

	%% insert the key <<"a">> with value <<"1">>	2> ok = Handle:put(<<"a">>, <<"1">>).

	%% try to re-insert the same key <<"a">>	3> key_exist = Handle:put(<<"a">>, <<"2">>).

	%% add a new key-value pair	4> ok = Handle:put(<<"b">>, <<"2">>).

	%% search a non-existing key <<"c">>	5> none = Handle:get(<<"c">>).

	%% retrieve the value for key <<"b">>	6> {ok, <<"2">>} = Handle:get(<<"b">>).

	%% retrieve the value for key <<"a">>	7> {ok, <<"1">>} = Handle:get(<<"a">>).

	%% delete key <<"b">>	8> ok = Handle:del(<<"b">>).

	%% search a non-existing key <<"b">>
	9> none = Handle:get(<<"b">>).

	%% delete a non-existing key <<"z">>	10> none = Handle:del(<<"z">>).
	11> {ok, <<"1">>} = Handle:get(<<"a">>).
	%% update the value for key <<"a">>
	12> ok = Handle:update(<<"a">>, <<"7">>).

	%% check the new value for key <<"a">>
	13> {ok, <<"7">>} = Handle:get(<<"a">>).

	%% delete all key-value pairs in the database	14> ok = Handle:drop().

	%% try to retrieve key <<"a">> value	15> none = Handle:get(<<"a">>).

	%% close the database	16> ok = Handle:close().

	17> q().  
  

Performance-----------For maximum speed, this library use only binaries for both keys and values.
See the impressive [microbench](http://highlandsun.com/hyc/mdb/microbench/) against:

* Google's LevelDB
* SQLite
* Kyoto TreeDB
* BerkeleyDB

MDB performs better on 64-bit arch.


Supported OSes--------------

Should work on 32/64-bit architectures:

* Linux
* OSX
* FreeBSD
* Windows

TODO----

* Unit tests* PropEr testing
* Bulk "writing"

Volunteers are always welcome!Status
------
#### Work in progress. Don't use it in production!
LICENSE-------
EMDB is Copyright (C) 2012 by Aleph Archives, and released under the [OpenLDAP](http://www.OpenLDAP.org/license.html) License.

