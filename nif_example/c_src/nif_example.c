
#include "erl_nif.h"
#include "example.hpp"


static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
    return 0;
}

static void
unload(ErlNifEnv* env, void* priv)
{
}

static ERL_NIF_TERM
hello_from_c(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_atom(env, "hello");
}

static ERL_NIF_TERM
hello_from_cpp(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_int(env, helloWorld());
}

static ErlNifFunc nif_funcs[] = {
    {"hello_from_c", 0, hello_from_c},
    {"hello_from_cpp", 0, hello_from_cpp}
};

ERL_NIF_INIT(nif_example, nif_funcs, load, NULL, NULL, unload);
