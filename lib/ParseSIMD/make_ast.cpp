extern "C" {
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
}

void mk_evar(value &_ans, char *str) {
  CAMLparam1(_ans);
  _ans = caml_alloc(1, 0 /*EVar*/);
  Store_field(_ans, 0, caml_copy_string(str));
  CAMLreturn0;
}

void mk_econst(value &_ans, int n) {
  CAMLparam1(_ans);
  _ans = caml_alloc(1, 1 /*EConst*/);
  Store_field(_ans, 0, Val_int(n));
  CAMLreturn0;
}

value mk_binop(char op, value _l, value _r) {
  CAMLparam2(_l, _r);
  CAMLlocal1(_ans);
  _ans = caml_alloc(3, 2 /*EBinOp*/);
  char str[2] = {'\0', '\0'};
  str[0] = op;
  Store_field(_ans, 0, caml_copy_string(str));
  Store_field(_ans, 1, _l);
  Store_field(_ans, 2, _r);

  CAMLreturn(_ans);
}

value mk_binop(char *str, value _l, value _r) {
  CAMLparam2(_l, _r);
  CAMLlocal1(_ans);
  _ans = caml_alloc(3, 2 /*EBinOp*/);
  Store_field(_ans, 0, caml_copy_string(str));
  Store_field(_ans, 1, _l);
  Store_field(_ans, 2, _r);

  CAMLreturn(_ans);
}
