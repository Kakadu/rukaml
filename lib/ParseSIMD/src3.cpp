#include "hexadecimal.hpp"
#include <cassert>
#include <cstring>

extern "C" {
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
}
#define log(fmt, ...) ;
// #define log(...) printf(__VA_ARGS__);
static value Val_some(value v) {
  CAMLparam1(v);
  CAMLlocal1(some);
  some = caml_alloc(1, 0 /*Some*/);
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

typedef unsigned rollback_t;
rollback_t make_rollback() { return pos; }
void rollback(rollback_t dest) { pos = dest; }

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

inline bool is_digit(char c) { return ('0' <= c) && (c <= '9'); }
inline bool is_alpha_digit(char c) { return is_alpha(c) || is_digit(c); }

inline bool pstring(char *str) {
  unsigned len = strlen(str);
  for (auto i = 0; i < len; ++i)
    if (pos < length && text[pos] == str[i]) {
      pos++;
    } else {
      return false;
    }
  return true;
}

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
  right = pos;

  log("%s %d %s length=%d\n", __FILE__, __LINE__, __func__, length);
  if (pos < length)
    log("text[pos] = %c\n", text[pos]);
  if (pos < length && is_alpha(text[pos])) {
    log("%s %d %s\n", __FILE__, __LINE__, __func__);
    pos++;
    right++;
  } else
    return false;

  if (right > left)
    while (pos < length && is_alpha_digit(text[pos])) {
      right++;
      pos++;
    }
  log("%s %d left=%d right=%d\n", __FILE__, __LINE__, left, right);
  if (right > left) {
    return true;
  } else {
    pos = oldpos;
    return false;
  }
}

bool ident(unsigned &left, unsigned &right) {
  log("%s %d %s\n", __FILE__, __LINE__, __func__);
  return ident_or_keyword(left, right);
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

bool econst(value &_ans) {
  ws();
  int oldpos = pos;
  unsigned left = pos;
  unsigned right = pos;
  unsigned acc = 0;

  log("%s on pos=%d, length=%d\n", __func__, pos, length);

  if (pos < length)
    log("text[pos] = '%c'\n", text[pos]);
  if (pos < length && is_digit(text[pos])) {
    acc = text[pos] - '0';
    pos++;
    right++;
  } else
    return false;
  log("%s on pos=%d, acc=%d\n", __func__, pos, acc);
  if (right > left)
    while (pos < length && is_digit(text[pos])) {
      acc *= 10;
      acc += text[pos] - '0';
      right++;
      pos++;
    }
  if (right > left) {
    log("%s parsed %d\n", __func__, acc);
    mk_econst(_ans, acc);
    // caml_callback(*caml_named_value("expr_printer"), _ans);
    return true;
  } else {
    pos = oldpos;
    return false;
  }
}

bool eident(value &_ans) {
  unsigned left, right;
  log("%s %d %s\n", __FILE__, __LINE__, __func__);
  if (ident(left, right)) {
    log("right=%d left=%d\n", right, left);
    // TODO: check keywords
    assert(right > left);
    char *str = new char[right - left + 1];
    memset(str, '\0', right - left + 1);
    memcpy(str, text + left, right - left);
    log("str = '%s'\n", str);
    mk_evar(_ans, str);
    // caml_callback(*caml_named_value("expr_printer"), _ans);
    return true;
  }
  return false;
}

bool expr_mul(value &_ans);
bool primary(value &ans);
bool expr_plus(value &_ans) {
  CAMLparam0();
  CAMLlocal3(_head, _acc, _tmp);
  if (!expr_mul(_head))
    return false;
  log("%s %d\n", __func__, __LINE__);
  static char opers[2] = {'+', '-'};
  // TODO: kind of cheating
  _acc = _head;
  for (unsigned i = 0; i < sizeof(opers); ++i) {
    ws();
    auto rb1 = make_rollback();
    log("%s %d c=%c\n", __func__, __LINE__, opers[i]);
    if (pchar(opers[i])) {
      log("%s %d\n", __func__, __LINE__);
      if (expr_mul(_tmp)) {
        log("%s %d\n", __func__, __LINE__);
        _acc = mk_binop(opers[i], _acc, _tmp);
        // caml_callback(*caml_named_value("expr_printer"), _tmp);
      } else {
        log("%s %d\n", __func__, __LINE__);
        rollback(rb1);
        break;
      }
    }
  }

  _ans = _acc;
  log("%s %d _ans = \n", __func__, __LINE__);
  // caml_callback(*caml_named_value("expr_printer"), _ans);
  CAMLreturnT(bool, true);
}

bool expr_mul(value &_ans) {
  CAMLparam0();
  CAMLlocal3(_head, _acc, _tmp);
  if (!primary(_ans))
    CAMLreturnT(bool, false);
  log("%s %d \n", __func__, __LINE__);
  _acc = _ans;
  static char opers[3] = {'*', '/', '>'};
  for (auto i = 0; i < sizeof(opers); ++i) {
    auto rb1 = make_rollback();
    log("%s %d \n", __func__, __LINE__);
    ws();
    if (pchar(opers[i])) {
      if (eident(_tmp)) {
        _acc = mk_binop(opers[i], _acc, _tmp);
      } else if (econst(_tmp)) {
        _acc = mk_binop(opers[i], _acc, _tmp);
      } else if (pchar('(') && expr_plus(_tmp) && pchar(')')) {
        _acc = mk_binop(opers[i], _acc, _tmp);
      } else {
        rollback(rb1);
        break;
      }
    } else {
      rollback(rb1);
    }
  }
  // now try keywords
  const unsigned STR_OPS_LEN = 1;
  static char *keywords[STR_OPS_LEN] = {"mod"};
  for (auto i = 0; i < STR_OPS_LEN; ++i) {
    auto rb1 = make_rollback();
    ws();
    if (pstring(keywords[i])) {
      if (eident(_tmp)) {
        _acc = mk_binop(keywords[i], _acc, _tmp);
      } else if (econst(_tmp)) {
        _acc = mk_binop(keywords[i], _acc, _tmp);
      } else if (pchar('(') && expr_plus(_tmp) && pchar(')')) {
        _acc = mk_binop(keywords[i], _acc, _tmp);
      } else {
        rollback(rb1);
        break;
      }
    } else {
      rollback(rb1);
    }
  }
  _ans = _acc;
  log("%s finished\n", __func__);
  // caml_callback(*caml_named_value("expr_printer"), _ans);

  CAMLreturnT(bool, true);
}

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

unsigned parse_expr(value &ans) { return expr_plus(ans); }
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
  init((char *)str, len);
  // log("%s %d\n", __FILE__, __LINE__);

  if (parse_expr(_ans)) {
    Val_pair(_p, _ans, Val_int(pos));
    fflush(stdout);
    CAMLreturn(Val_some(_p));
  }
  fflush(stdout);
  CAMLreturn(Val_none);
}