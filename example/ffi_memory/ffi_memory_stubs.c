#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <stdlib.h>
#include <string.h>

/* (1) FFI: allocate [n] OCaml blocks of [wosize] words each on the OCaml heap.
 * Should be tracked by pyro-caml */
CAMLprim value ml_test_alloc_ocaml_blocks(value v_n, value v_wosize) {
    CAMLparam2(v_n, v_wosize);
    CAMLlocal1(blk);
    long n = Long_val(v_n);
    long wosize = Long_val(v_wosize);
    for (long i = 0; i < n; i++) {
        blk = caml_alloc(wosize, 0);
        for (long j = 0; j < wosize; j++) Store_field(blk, j, Val_long(i));
    }
    CAMLreturn(Val_unit);
}

/* (2a) Naked malloc: allocate [m] buffers of [bytes] each and keep them in a
 * C-side static. Nothing is allocated on the OCaml heap, so Memprof should 
 * report exactly zero. */
static void **g_naked = NULL;
static long g_naked_n = 0;

CAMLprim value ml_test_alloc_malloc_naked(value v_m, value v_bytes) {
    CAMLparam2(v_m, v_bytes);
    long m = Long_val(v_m);
    size_t bytes = (size_t) Long_val(v_bytes);
    if (g_naked != NULL) {
        for (long i = 0; i < g_naked_n; i++) free(g_naked[i]);
        free(g_naked);
    }
    g_naked = (void **) malloc((size_t) m * sizeof(void *));
    g_naked_n = m;
    for (long i = 0; i < m; i++) {
        void *p = (bytes > 0) ? malloc(bytes) : NULL;
        if (p != NULL) memset(p, 1, bytes);
        g_naked[i] = p;
    }
    CAMLreturn(Val_unit);
}

/* (2b) malloc behind a custom block. The custom block itself is a small
 * OCaml-heap allocation that Memprof always samples. The [bytes] behind the
 * pointer are off-heap: Memprof sees them ONLY if their size is declared to the
 * GC via the mem argument of caml_alloc_custom_mem. With mem = 0 the buffer is
 * invisible (only the wrapper counts); with mem = bytes it is sampled weighted
 * by that size. This is how Bigarray-style off-heap memory becomes visible. */
static void test_buf_finalize(value v) { free(*((void **) Data_custom_val(v))); }

static struct custom_operations test_buf_ops = {
    "pyro_caml.test.malloc_buf",
    test_buf_finalize,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default,
    custom_fixed_length_default
};

CAMLprim value ml_test_alloc_malloc_custom(value v_bytes, value v_declare) {
    CAMLparam2(v_bytes, v_declare);
    CAMLlocal1(v);
    size_t bytes = (size_t) Long_val(v_bytes);
    int declare = Bool_val(v_declare);
    void *p = (bytes > 0) ? malloc(bytes) : NULL;
    if (p != NULL) memset(p, 1, bytes);
    /* The third arg declares the off-heap size to the GC. When declared,
     * Memprof samples the custom block weighted by that size (the off-heap
     * memory becomes visible); when 0, only the on-heap wrapper is sampled. */
    v = caml_alloc_custom_mem(&test_buf_ops, sizeof(void *),
                              declare ? bytes : 0);
    *((void **) Data_custom_val(v)) = p;
    CAMLreturn(v);
}
