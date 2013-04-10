/* -------------------------------------------------------------------------
 * This file is part of EMDB - Erlang MDB API                               
 *                                                                          
 * Copyright (c) 2012 by Aleph Archives. All rights reserved.               
 *                                                                          
 * -------------------------------------------------------------------------
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted only as authorized by the OpenLDAP
 * Public License.
 *
 * A copy of this license is available in the file LICENSE in the
 * top-level directory of the distribution or, alternatively, at
 * <http://www.OpenLDAP.org/license.html>.
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 * -------------------------------------------------------------------------*/

/*
 * C headers
 */

#include <sys/param.h>  /* for MAXPATHLEN constant */
#include <erl_nif.h>    /* for Erlang NIF interface */
#include "uthash.h"     /* for uthash */
#include "mdb.h"        /* for MDB interface */



#define FREE(p)   (NULL == (p) ? 0 : (free(p), p = NULL))

#define FAIL_FAST(Error, Goto)                    \
  do{                                             \
    err = Error;                                  \
    goto Goto;                                    \
}while(0)


struct emdb_map_t {
  MDB_env * env;
  MDB_dbi   dbi;

  UT_hash_handle  hh;
};


static ERL_NIF_TERM atom_ok;
static ERL_NIF_TERM atom_none;


static struct emdb_map_t * emdb_map = NULL;


/* emdb ret */
#define EMDB_RET_KEY_EXIST           "key_exist"

/* emdb errors */
#define EMDB_MALLOC_ERR              "error_malloc"
#define EMDB_MAKE_BINARY_ERR         "error_make_binary"
#define EMDB_CREATE_ERR              "error_create"
#define EMDB_MAPSIZE_ERR             "error_mapsize"
#define EMDB_OPEN_ERR                "error_open"
#define EMDB_TXN_BEGIN_ERR           "error_txn_begin"
#define EMDB_TXN_COMMIT_ERR          "error_txn_commit"
#define EMDB_OPEN_DBI_ERR            "error_open_dbi"
#define EMDB_INVALID_HANDLE_ERR      "error_invalid_handle"
#define EMDB_PUT_ERR                 "error_put"
#define EMDB_UPDATE_ERR              "error_update"
#define EMDB_KEY_NOT_FOUND           "error_key_not_found"
#define EMDB_DROP_ERR                "error_drop"



/*
 * Error handling callbacks
 */

static void emdb_free (struct emdb_map_t * emdb_obj)
{
  FREE(emdb_obj);
}


/*
 * Driver callbacks
 */

static ERL_NIF_TERM emdb_open_nif (ErlNifEnv * env,
                                   int argc, const ERL_NIF_TERM argv[])
{
  char dirname [MAXPATHLEN];
  struct emdb_map_t * node;
  MDB_txn * txn;
  char * err;
  ErlNifUInt64 mapsize;
  ErlNifUInt64 envflags;

  if (enif_get_string(env, argv[0], dirname, MAXPATHLEN, ERL_NIF_LATIN1) <= 0)
    return enif_make_badarg(env);
  
  if(! (node = calloc(1, sizeof(struct emdb_map_t))))
    FAIL_FAST(EMDB_MALLOC_ERR, err3);
  
  if (mdb_env_create(& (node -> env)))
    FAIL_FAST(EMDB_CREATE_ERR, err2);
  
  if (! enif_get_uint64(env, argv[1], & mapsize))
    return enif_make_badarg(env);

  if (mdb_env_set_mapsize(node -> env, mapsize))
    FAIL_FAST(EMDB_MAPSIZE_ERR, err2);
      
  if (! enif_get_uint64(env, argv[2], & envflags))
    return enif_make_badarg(env);

  if (mdb_env_open(node -> env, dirname, envflags, 0664))
    FAIL_FAST(EMDB_OPEN_ERR, err2);

  if (mdb_txn_begin(node -> env, NULL, 0, & txn))
    FAIL_FAST(EMDB_TXN_BEGIN_ERR, err2);

  if (mdb_open(txn, NULL, 0, & (node -> dbi)))
    FAIL_FAST(EMDB_OPEN_DBI_ERR, err1);

  if (mdb_txn_commit(txn))
    FAIL_FAST(EMDB_TXN_COMMIT_ERR, err2);

  HASH_ADD_PTR(emdb_map, env, node);

  return enif_make_tuple(env, 2,
                         atom_ok,
                         enif_make_ulong(env, (unsigned long) node -> env));

 err1:
  mdb_txn_abort(txn);
 err2:
  mdb_env_close(node -> env);
 err3:
  emdb_free(node);

  return enif_make_atom(env, err);
}

static ERL_NIF_TERM emdb_close_nif (ErlNifEnv * env,
                                    int argc, const ERL_NIF_TERM argv[])
{
  MDB_env * handle;
  struct emdb_map_t * node;
  unsigned long addr;

  if (! enif_get_ulong(env, argv[0], & addr))
    return enif_make_badarg(env);
  
  handle = (MDB_env *) addr;

  HASH_FIND_PTR(emdb_map, & handle, node);
  if (NULL == node)
    return enif_make_atom(env, EMDB_INVALID_HANDLE_ERR);

  HASH_DEL(emdb_map, node);

  mdb_env_close(handle);
  emdb_free(node);

  return atom_ok;
 }


static ERL_NIF_TERM emdb_put_nif (ErlNifEnv * env,
                                  int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary key;
  ErlNifBinary val;

  MDB_val mkey;
  MDB_val mdata;

  MDB_env * handle;
  MDB_txn * txn;

  struct emdb_map_t * node;
  unsigned long addr;
  char * err;
  int ret;

  if (! enif_get_ulong(env, argv[0], & addr))
    return enif_make_badarg(env);
  
  handle = (MDB_env *) addr;

  HASH_FIND_PTR(emdb_map, & handle, node);
  if (NULL == node)
    return enif_make_atom(env, EMDB_INVALID_HANDLE_ERR);

  if (! enif_inspect_iolist_as_binary(env, argv[1], &key))
    return enif_make_badarg(env);

  if (! enif_inspect_iolist_as_binary(env, argv[2], &val))
    return enif_make_badarg(env);

  if (mdb_txn_begin(handle, NULL, 0, & txn))
    FAIL_FAST(EMDB_TXN_BEGIN_ERR, err2);

  mkey.mv_size  = key.size;
  mkey.mv_data  = key.data;
  mdata.mv_size = val.size;
  mdata.mv_data = val.data;

  ret = mdb_put(txn, node -> dbi, & mkey, & mdata, MDB_NOOVERWRITE);
  if (MDB_KEYEXIST == ret)
    FAIL_FAST(EMDB_RET_KEY_EXIST, err1);
  if (ret)
    FAIL_FAST(EMDB_PUT_ERR, err1);

  if (mdb_txn_commit(txn))
    FAIL_FAST(EMDB_TXN_COMMIT_ERR, err2);
  
  return atom_ok;

 err1:
  mdb_txn_abort(txn);
 err2:
  return enif_make_atom(env, err);
}


static ERL_NIF_TERM emdb_get_nif (ErlNifEnv * env,
                                  int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary key;
  ErlNifBinary val = {0};
  ERL_NIF_TERM term;

  MDB_val mkey;
  MDB_val mdata;

  MDB_env * handle;
  MDB_txn * txn;

  struct emdb_map_t * node;
  char * err;
  unsigned long addr;

  if (! enif_get_ulong(env, argv[0], & addr))
    return enif_make_badarg(env);
  
  handle = (MDB_env *) addr;

  HASH_FIND_PTR(emdb_map, & handle, node);
  if (NULL == node)
    return enif_make_atom(env, EMDB_INVALID_HANDLE_ERR);

  if (! enif_inspect_iolist_as_binary(env, argv[1], &key))
    return enif_make_badarg(env);

  mkey.mv_size  = key.size;
  mkey.mv_data  = key.data;

  if (mdb_txn_begin(handle, NULL, 0, & txn))
    FAIL_FAST(EMDB_TXN_BEGIN_ERR, err);

  if(mdb_get(txn, node -> dbi, & mkey, & mdata))
    {
      mdb_txn_abort(txn);
      return atom_none;
    }

  val.size = mdata.mv_size;
  val.data = mdata.mv_data;
  
  term = enif_make_binary(env, &val);
  mdb_txn_abort(txn);

  if (! term)
    FAIL_FAST(EMDB_MAKE_BINARY_ERR, err);

  return enif_make_tuple(env, 2,
                         atom_ok,
                         term);

 err:
  return enif_make_atom(env, err);
}


static ERL_NIF_TERM emdb_del_nif (ErlNifEnv * env,
                                  int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary key;

  MDB_val mkey;

  MDB_env * handle;
  MDB_txn * txn;

  struct emdb_map_t * node;
  char * err;
  unsigned long addr;
  int ret;

  if (! enif_get_ulong(env, argv[0], & addr))
    return enif_make_badarg(env);
  
  handle = (MDB_env *) addr;

  HASH_FIND_PTR(emdb_map, & handle, node);
  if (NULL == node)
    return enif_make_atom(env, EMDB_INVALID_HANDLE_ERR);

  if (! enif_inspect_iolist_as_binary(env, argv[1], &key))
    return enif_make_badarg(env);

  mkey.mv_size  = key.size;
  mkey.mv_data  = key.data;

  if (mdb_txn_begin(handle, NULL, 0, & txn))
    FAIL_FAST(EMDB_TXN_BEGIN_ERR, err);

  ret = mdb_del(txn, node -> dbi, & mkey, NULL);

  if (mdb_txn_commit(txn))
    FAIL_FAST(EMDB_TXN_COMMIT_ERR, err);

  if(ret)
    return atom_none;
  
  return atom_ok;

 err:
  return enif_make_atom(env, err);
}


static ERL_NIF_TERM emdb_update_nif (ErlNifEnv * env,
                                     int argc, const ERL_NIF_TERM argv[])
{
  ErlNifBinary key;
  ErlNifBinary val;

  MDB_val mkey;
  MDB_val mdata;

  MDB_env * handle;
  MDB_txn * txn;

  struct emdb_map_t * node;
  unsigned long addr;
  char * err;

  if (! enif_get_ulong(env, argv[0], & addr))
    return enif_make_badarg(env);
  
  handle = (MDB_env *) addr;

  HASH_FIND_PTR(emdb_map, & handle, node);
  if (NULL == node)
    return enif_make_atom(env, EMDB_INVALID_HANDLE_ERR);

  if (! enif_inspect_iolist_as_binary(env, argv[1], &key))
    return enif_make_badarg(env);

  if (! enif_inspect_iolist_as_binary(env, argv[2], &val))
    return enif_make_badarg(env);

  if (mdb_txn_begin(handle, NULL, 0, & txn))
    FAIL_FAST(EMDB_TXN_BEGIN_ERR, err2);

  mkey.mv_size  = key.size;
  mkey.mv_data  = key.data;
  mdata.mv_size = val.size;
  mdata.mv_data = val.data;

  if (mdb_put(txn, node -> dbi, & mkey, & mdata, 0))
    FAIL_FAST(EMDB_UPDATE_ERR, err1);

  if (mdb_txn_commit(txn))
    FAIL_FAST(EMDB_TXN_COMMIT_ERR, err2);

  return atom_ok;

 err1:
  mdb_txn_abort(txn);
 err2:
  return enif_make_atom(env, err);
}


static ERL_NIF_TERM emdb_drop_nif (ErlNifEnv * env,
                                   int argc, const ERL_NIF_TERM argv[])
{
  MDB_env * handle;
  MDB_txn * txn;
  struct emdb_map_t * node;
  unsigned long addr;
  char * err;
  int ret;

  if (! enif_get_ulong(env, argv[0], & addr))
    return enif_make_badarg(env);
  
  handle = (MDB_env *) addr;

  HASH_FIND_PTR(emdb_map, & handle, node);
  if (NULL == node)
    return enif_make_atom(env, EMDB_INVALID_HANDLE_ERR);

  if (mdb_txn_begin(handle, NULL, 0, & txn))
    FAIL_FAST(EMDB_TXN_BEGIN_ERR, err2);

  ret = mdb_drop(txn, node -> dbi, 0);
  if (ret)
    FAIL_FAST(EMDB_DROP_ERR, err1);

  if (mdb_txn_commit(txn))
    FAIL_FAST(EMDB_TXN_COMMIT_ERR, err2);

  return atom_ok;

 err1:
  mdb_txn_abort(txn);
  
 err2:
  return enif_make_atom(env, err);
 }


static int emdb_load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
  {
    atom_ok    = enif_make_atom(env, "ok");
    atom_none  = enif_make_atom(env, "none");

    return (0);
  }

static int emdb_reload(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
  return (0);
}
  

static int emdb_upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM load_info)
{
  return (0);
}


static void emdb_unload(ErlNifEnv* env, void* priv)
{
  return;
}



static ErlNifFunc nif_funcs [] = {
  {"open",        3, emdb_open_nif},
  {"close",       1, emdb_close_nif},
  {"put",         3, emdb_put_nif},
  {"get",         2, emdb_get_nif},
  {"del",         2, emdb_del_nif},
  {"update",      3, emdb_update_nif},
  {"drop",        1, emdb_drop_nif}
};

/* driver entry point */
ERL_NIF_INIT(emdb_drv,
             nif_funcs,
             & emdb_load,
             & emdb_reload,
             & emdb_upgrade,
             & emdb_unload)
