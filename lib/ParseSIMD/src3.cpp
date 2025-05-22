#include "hexadecimal.hpp"
#include <cassert>
#include <cstring>

extern "C" {
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
}

static value Val_some(value v) {
  CAMLparam1(v);
  CAMLlocal1(some);
  some = caml_alloc(1, 0);
  Store_field(some, 0, v);
  CAMLreturn(some);
}

char *text = nullptr;
unsigned length = 0;
unsigned pos = 0;

void init(char *s, unsigned len) {
  text = s;
  length = len;
  pos = 0;
}
bool is_ws(char c) {
  switch (c) {
  case '\n':
  case ' ':
    return true;
  }
  return false;
}
void ws() {
  while (pos < length && is_ws(text[pos]))
    pos++;
}
inline bool is_alpha(char c) { return ('a' <= c) && (c <= 'z'); }

inline bool is_digit(char c) { return ('a' <= c) && (c <= 'z'); }
inline bool is_alpha_digit(char c) { return is_alpha(c) || is_digit(c); }

inline bool pchar(char c) {
  if (pos < length && text[pos] == c) {
    pos++;
    return true;
  }
  return false;
}

bool lookahead_paren() {
  unsigned curpos = pos;
  while (curpos < length && is_ws(text[curpos]))
    curpos++;
  return (curpos < length && text[curpos] == '(');
}

bool ident_or_keyword(unsigned &left, unsigned &right) {
  ws();
  int oldpos = pos;
  left = pos;
  unsigned idlen = 0;
  if (pos < length && is_alpha(text[pos])) {
    pos++;
    idlen++;
  }

  left = pos;
  right = left;
  if (idlen > 0)
    while (pos < length && is_alpha_digit(text[pos])) {
      right++;
      pos++;
    }
  if (right > left) {
    return true;
  } else {
    pos = oldpos;
    return false;
  }
}

bool ident(unsigned &left, unsigned &right) {
  return ident_or_keyword(left, right);
}

void mkevar(value &_ans, char *str) {
  CAMLparam1(_ans);
  _ans = caml_alloc(1, 0 /*EVar*/);
  Store_field(_ans, 0, caml_copy_string(str));
  CAMLreturn0;
}
bool econst(value &ans) { return false; }
bool eident(value &ans) {
  unsigned left, right;
  if (ident(left, right)) {
    // TODO: check keywords
    assert(right > left);
    char *str = new char[right - left + 1];
    memset(str, '\0', right - left + 1);
    memcpy(str, text, right - left);
    mkevar(ans, str);
    return true;
  }
  return false;
}
bool expr_mul(value &ans);
bool expr_plus(value &ans) {
  CAMLparam0();
  CAMLlocal1(_head);
  if (!expr_mul(_head))
    return false;
  // if (lookahead_paren()){
  //     bool b1=pchar();
  //     bool b0 =
  // }
  //   TODO:
  return false;
}
bool expr_mul(value &ans) { return false; }

bool primary(value &ans) {
  if (lookahead_paren()) {
    bool b1 = pchar('(');
    bool b0 = expr_plus(ans);
    bool b2 = pchar('(');
    return (b0 && b1 && b2);
  } else {
    if (eident(ans)) {
      return true;
    } else if (econst(ans)) {
      return true;
    } else
      return false;
  }
}

unsigned parse_expr(value &ans) { return 0; }
#define Val_pair(dest, a, b)                                                   \
  dest = caml_alloc(2, 0);                                                     \
  Store_field(dest, 0, a);                                                     \
  Store_field(dest, 1, b);

extern "C" value p_expr_stub(value _str) {
  CAMLparam1(_str);
  CAMLlocal2(_p, _ans);
  auto len = caml_string_length(_str);
  unsigned pos = 0;
  const char *str = String_val(_str);
  value ans;

  if (parse_expr(ans)) {
    Val_pair(_p, _ans, Val_int(pos));
    CAMLreturn(Val_some(_p));
  }
  CAMLreturn(Val_none);
}