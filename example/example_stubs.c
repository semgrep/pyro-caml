#include <caml/alloc.h>
#include <caml/callback.h> // caml_callback_res
#include <caml/fail.h>     // caml_get_value_or_raise
#include <caml/mlvalues.h>

CAMLprim value ml_comp_and_callback(value v_callback) {
    CAMLparam1(v_callback);
    CAMLlocalresult(res);
    CAMLlocal1(v1);
    for (int i = 0; i < 100; i++) {
        res = caml_callback_res(v_callback, Val_unit);
        (void)caml_get_value_or_raise(res);
        // do comp
    }
    CAMLreturn(v1);
}
