# IRML
Данный модуль является OCaml оберткой (wrapper) для [форка](https://github.com/ns-58/son_c) репозитория с фреймворком для JIT-компиляции, онсованном на графовом представлении программы Sea-Of-Nodes  ([оригинальный репозиторий](https://github.com/dstogov/ir)).


## Структура
Связывания для функций и структур фреймворка находятся в файле [`bindings.ml`](bindings.ml)

Необходимые константы (компиляционые флаги, аргументы-типы для создания вершин и тд) находятся в файле [`consts.ml`](consts.ml)

Вспомогательные функции находятся в файле  [`helpers.ml`](helpers.ml)

## Сборка
Подтягиваем файлы из форка:

``` fish
git submodule update --init 
```

Сборка модуля:

``` fish
dune build
```

## Использование
(из под x86_64 для x86_64)

Грубый план:

    1. Построение графа для функции (в т.ч. сопутствующие peephole-оптимизации, такие как свертка констант, удаление общих подвыражений (cse) и т.п.)

    2. Оптимизации

    3. Компиляция в память с помощью DinAsm (пока что единственный способ компиляции, предоставляемый фреймворком)

    4. Дизассемблирование (чтобы получить ассемблерный файл)


На примере компиляции программы, печатающей факториал от 5, продемонстрируем  назначение имеющихся связываний, констант и вспомогательных функций:

### Строим граф функции, вычисляющей факториал
Для рекурсивных функций необходимо создавать thunk (память в которую, мы "обещаем" записать адрес функции в памяти после этапа DynAsm), для того чтобы функция могла использовать его, например, в вызовах. 
С помощью функций из вспомогательного модуля `Code_buf` предварительно выделяем 10 KB для thunk'ов, создаем thunk для адреса функции factorial и сообщаем о его добавлении дизассемблеру:

``` ocaml
let open Helpers in
let code_buf = Code_buf.create 10240 in
let th, size = Code_buf.add_thunk code_buf in
ir_disasm_add_symbol_w "factorial" th size;

```

Инициализируем граф: создаем контекст, проверяем, что фреймворк может работать в текущем окружении, добавляем компил. флаги и выделяем начальные буферы для хранения 16 и 48 констант и вершин соответственно:
```ocaml
let open Bindings in
let ctx = ir_create_ctx () in
ir_consistency_check ();
let flags =
    let ( + ) = Unsigned.UInt32.add in
    Consts.(ir_function + ir_opt_folding + ir_opt_cfg + ir_opt_codegen)
in
let consts, insns = 16l, 48l in
ir_init ctx flags consts insns;
```

Теперь строим граф для функции factorial. Функции вида `ir_emit...` просто добавляют вершину в граф, в то время как `ir_fold...` перед добавлением запускают серию peephole-оптимизаций для добавляемой вершины. Цифра в конце названия функции отвечает за кол-во входящих ребер (суммарно  control-flow и data-flow; control-flow всегда передаются в начале). 
Тип вершины определяется константой.

Создаём стартовую вершину, вершину для единственного параметра (1 обозначает порядковый номер параметра слева направо) и вершину для константы 1:
``` ocaml
let open Bindings in
let start = ir_emit0 ctx Consts.ir_start in
let n = ir_param ctx Consts.ir_i64 start "n" 1 in
let one =  ir_fold1 ctx Consts.ir_copy_i64 @@ ir_const_i64 ctx 1L in
```
Ветвление 'if n <= 1 then 1 ...':

If вершина принимает ребро из текущей control вершины (здесь start) и из data вершины, сооветствующей условию перехода (здесь создается с помощью `ir_fold2`).

Каждая ветка начинается с вершины-проекции If: `Consts.ir_if_true` или `Consts.ir_if_false` и заканчивается end вершиной, которая так же принимает ребро из текущей control вершины (здесь tr)
``` ocaml
let open Bindings in
let br = ir_emit2 ctx Consts.ir_if start @@ ir_fold2 ctx Consts.ir_le n one 
let tr = ir_emit1 ctx Consts.ir_if_true br in
let one' = ir_fold1 ctx Consts.ir_copy_i64 @@ ir_const_i64 ctx 1L in 
let end_tr = ir_emit1 ctx Consts.ir_end tr in
```
Теперь 'else n * fac (n - 1)':

Суффикс константы вызова (здесь i64) обозначает тип возвращаемого значения.

Полученный в самом начале адрес thunk'а используется как адрес вызываемой функции (используется `ir_const_addr_w`).

```ocaml
let open Bindings in
  let fls = ir_emit1 ctx Consts.ir_if_false cond in
  let pred = ir_fold2 ctx Consts.ir_sub_i64 n one in
  let call = ir_emit3 ctx Consts.ir_call1_i64 fls (ir_const_addr_w ctx th) pred in
  let mul =  ir_fold2 ctx Consts.ir_mul_i64 n call in 
  let end_fl = ir_emit1 ctx Consts.ir_end call in
```
> [!NOTE] Если у функции больше одного аргумента, то Call-вершина принимает более 3 ребер (1 от control вершины, по 1 от data вершины для адреса функции и для каждого из аргументов), и необходимо использовать функции `ir_emitN` (4 отвечает за колиество принимаемых ребер) в паре с `ir_set_op`:
```ocaml
    (* ЭТО ОТСТУПЛЕНИЕ ОТ ФАКТОРИАЛА*)
    let open Bindings in
    let call = ir_emitN ctx Consts.ir_call_i64 4l  in
    ir_set_op ctx call 1l control;
    ir_set_op ctx call 2l f;
    ir_set_op ctx call 3l a1;
    ir_set_op ctx call 4l a1;
```

Наконец, ветки сливаются с помощью Merge (принимает ребра от end вершин мз кажой ветки) и Phi вершин (для сливаемого значения), функция возврашает значение с помощью Return вершины (здесь m используется как актуальная control вершина), а ребро до Return вершины добавляется в стартовую вершину:
```ocaml
  let open Bindings in
  let m = ir_emit2  ctx Consts.ir_merge2 end_tr end_fl in
  let phi = ir_emit3 ctx Consts.ir_phi2_i64 m one' mul in
  let ret = ir_emit2 ctx Consts.ir_return m phi in
  ir_set_op ctx start 1l ret
```

### Оптимизации и компиляция функции в память

Оптимизации и вспомогательные построения:
```ocaml
  let open Bindings in
  ir_build_def_use_lists ctx;
  let _ = ir_sccp ctx in
  ir_build_cfg ctx;
  ir_build_dominators_tree ctx;
  ir_find_loops ctx;
```
[Эффективно распределяем data вершины по базовым блокам ](https://bernsteinbear.com/assets/img/click-gvn.pdf):
```ocaml

  Bindings.ir_gcm ctx;
```
Scheduling, распределение регистров, выбор инструкций:
```ocaml
  let open Bindings in
  ir_schedule ctx;
  ir_match ctx;
  ir_assign_virtual_registers ctx;
  ir_compute_live_ranges ctx;
  ir_coalesce ctx;
  ir_reg_alloc ctx;
  ir_schedule_blocks ctx;
```
Компиляция в память:
```ocaml
let open Bindings in
let fac_sz_buf = Helpers.size_buf () in
let fac_entry = ir_emit_code ctx fac_sz_buf in
```

Записываем полученный адрес в выделенный ранее thunk и сохраняем контекст (понадобится при дизассемблировании):
```ocaml
let open Helpers
let open Bindings
Code_buf.fix_thunk code_buf th entry;
ir_disasm_add_symbol_w "factorial" entry size;
let fac_ctx = ctx in
```

### Подготовка к  дизассемблированию, использование cторонних функций (например, runtime)

Открываем файл для результата компиляции в ассмеблер 
```ocaml
let s_out = Bindings.fopen 'out.s' "w" in
```
В main нам понадобится функция печати. Предположим, что она есть в другом ассемблерном файле, с которым мы позже собираемся слинковаться и называется print.

Явно импортируем нужную функцию в асcемблерный файл и создаем thunk:
``` ocaml
ignore @@ fprintf s_out (".extern " ^ asm_name ^ "\n");
let th, sz = add_thunk code_buf in
ir_disasm_add_symbol_w asm_name th sz;
```
В этот thunk мы не будем писать, так как так и не узнаем нужный адрес до линковки.

Объявляем глобальной функцию main:

```ocaml
ignore @@ fprintf s_out ".global main\n\n";

```

### Дизассемблирование
Пропустим пояснения, относительно построения и компиляции main. В качестве адреса функции factorial в ней по прежнему нужно использовать (ссылку на) thunk.

```ocaml
...
let main_sz_buf = ... in
let main_entry = ... in
let main_ctx = ... in
```
Вызов дизассемблера для функций:
```ocaml
ignore @@ ir_disasm "factorial" fac_entry fac_sz_buf false fac_ctx s_out;
ignore @@ ir_disasm "main" main_entry main_sz_buf false man_ctx s_out;

```
