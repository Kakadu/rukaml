  $ ocamlopt -dcmm -dsel -dscheduling -c sum.ml
  
  cmm:
  (data)
  (data
   int 3063
   global "camlSum__sum_rec_77"
   "camlSum__sum_rec_77":
   global "camlSum__sum_rec_60_closure"
   "camlSum__sum_rec_60_closure":
   addr "camlSum__sum_rec_60"
   int 72057594037927941)
  (data
   int 3063
   global "camlSum__sum_75"
   "camlSum__sum_75":
   global "camlSum__sum_11_closure"
   "camlSum__sum_11_closure":
   addr "camlSum__sum_11"
   int 72057594037927941)
  (data
   int 4087
   global "camlSum__set_of_closures_76"
   "camlSum__set_of_closures_76":
   global "camlSum__loop_41_closure"
   "camlSum__loop_41_closure":
   addr "caml_curry4"
   int 288230376151711751
   addr "camlSum__loop_41")
  (data global "camlSum__gc_roots" "camlSum__gc_roots": int 0)
  (function{sum.ml:1,8-148} camlSum__sum_11 (arr/345: val)
   (let Psubint/348 (+ (or (>>u (load_mut int (+a arr/345 -8)) 9) 1) -2)
     (let_mut sum/350: int 1
       (let
         sequence/354
           (seq
             (let_mut i/356: int 1
               (let bound/366 (+ Psubint/348 -2)
                 (catch
                   (if (> i/356 bound/366) (exit 1)
                     (catch rec (exit 2) with(2)
                       (let
                         (Paddint_arg/357
                            (load_mut int (+a (+a arr/345 (<< i/356 2)) -4))
                          Paddint/359 (+ (+ sum/350 Paddint_arg/357) -1))
                         (assign sum/350 Paddint/359))
                       (let *id_prev*/365 i/356 (assign i/356 (+ i/356 2))
                         (if (== *id_prev*/365 bound/366) (exit 1) []))
                       (exit 2)))
                 with(1) [])))
             1)
         sum/350))))
  
  *** After instruction selection
  camlSum__sum_11(R/0[%rax]) {sum.ml:1,8-148}
    arr/29 := R/0[%rax]
    I/30 := int mut[arr/29 + -8]{sum.ml:2,10-26}
    I/31 := I/30
    I/31 := I/31 >>u 9{sum.ml:2,10-26}
    I/32 := I/31
    I/32 := I/32 | 1{sum.ml:2,10-26}
    Psubint/33 := I/32
    Psubint/33 := Psubint/33 + -2{sum.ml:2,10-30}
    I/34 := 1
    sum/35 := I/34
    I/36 := 1
    i/37 := I/36
    bound/38 := Psubint/33
    bound/38 := bound/38 + -2{sum.ml:4,15-20}
    catch
      if i/37 >s bound/38 then
        exit(1)
      else
        catch rec
          exit(2)
        with(2)
          Paddint_arg/39 := int mut[arr/29 + i/37 * 4 + -4]{sum.ml:5,18-40}
          Paddint/40 := sum/35 + Paddint_arg/39 + -1{sum.ml:5,11-40}
          sum/35 := Paddint/40
          *id_prev*/41 := i/37
          I/42 := i/37
          I/42 := I/42 + 2
          i/37 := I/42
          if *id_prev*/41 ==s bound/38 then
            exit(1)
          endif
          poll call
          exit(2)
          
        endcatch
      endif
    with(1)
      
      
    endcatch
    sequence/43 := 1
    R/0[%rax] := sum/35
    return R/0[%rax]
  *** After instruction scheduling
  camlSum__sum_11: {sum.ml:1,8-148}
    prologue
    L102:
    arr/29[%rbx] := R/0[%rax]
    I/30[%rax] := int mut[arr/29[%rbx] + -8] {sum.ml:2,10-26}
    I/31[%rax] := I/31[%rax] >>u 9 {sum.ml:2,10-26}
    I/32[%rax] := I/32[%rax] | 1 {sum.ml:2,10-26}
    Psubint/33[%rdi] := I/32[%rax]
    Psubint/33[%rdi] := Psubint/33[%rdi] + -2 {sum.ml:2,10-30}
    I/34[%rax] := 1
    I/36[%rsi] := 1
    bound/38[%rdi] := bound/38[%rdi] + -2 {sum.ml:4,15-20}
    if i/37[%rsi] >s bound/38[%rdi] goto L100
    L101:
    Paddint_arg/39[%rdx] := int mut[arr/29[%rbx] + i/37[%rsi] * 4 + -4] {sum.ml:5,18-40}
    Paddint/40[%rax] := sum/35[%rax] + Paddint_arg/39[%rdx] + -1 {sum.ml:5,11-40}
    *id_prev*/41[%rdx] := i/37[%rsi]
    I/42[%rsi] := I/42[%rsi] + 2
    if *id_prev*/41[%rdx] ==s bound/38[%rdi] goto L100
    {arr/29[%rbx]* sum/35[%rax] i/37[%rsi] bound/38[%rdi]}
    poll call returning to L101
    L100:
    reload retaddr
    return R/0[%rax]
    
  (function{sum.ml:10,13-107} camlSum__loop_41
       (len/338: val arr/337: val acc/336: val i/335: val)
   (if (< i/335 len/338)
     (let Paddint_arg/342 (load_mut int (+a (+a arr/337 (<< i/335 2)) -4))
       (app{sum.ml:11,18-69} "camlSum__loop_41" len/338 arr/337
         (+ (+ acc/336 Paddint_arg/342) -1) (+ i/335 2) val))
     acc/336))
  
  *** After instruction selection
  camlSum__loop_41(R/0[%rax] R/1[%rbx] R/2[%rdi] R/3[%rsi]) {sum.ml:10,13-107}
    len/29 := R/0[%rax]
    arr/30 := R/1[%rbx]
    acc/31 := R/2[%rdi]
    i/32 := R/3[%rsi]
    poll call{sum.ml:10,13-107}
    if i/32 <s len/29 then
      Paddint_arg/33 := int mut[arr/30 + i/32 * 4 + -4]{sum.ml:11,38-60}
      I/34 := i/32
      I/34 := I/34 + 2{sum.ml:11,62-69}
      I/35 := acc/31 + Paddint_arg/33 + -1{sum.ml:11,31-61}
      R/0[%rax] := len/29
      R/1[%rbx] := arr/30
      R/2[%rdi] := I/35
      R/3[%rsi] := I/34
      tailcall "camlSum__loop_41" R/0[%rax] R/1[%rbx] R/2[%rdi] R/3[%rsi]{sum.ml:11,18-69}
    else
      R/0[%rax] := acc/31
      return R/0[%rax]
    endif
  *** After instruction scheduling
  camlSum__loop_41: {sum.ml:10,13-107}
    prologue {sum.ml:10,13-107}
    L106: {sum.ml:10,13-107}
    {len/29[%rax]* arr/30[%rbx]* acc/31[%rdi]* i/32[%rsi]*}
    poll call {sum.ml:10,13-107}
    if i/32[%rsi] >=s len/29[%rax] goto L105
    Paddint_arg/33[%rdx] := int mut[arr/30[%rbx] + i/32[%rsi] * 4 + -4] {sum.ml:11,38-60}
    I/34[%rsi] := I/34[%rsi] + 2 {sum.ml:11,62-69}
    I/35[%rdi] := acc/31[%rdi] + Paddint_arg/33[%rdx] + -1 {sum.ml:11,31-61}
    tailcall "camlSum__loop_41" R/0[%rax] R/1[%rbx] R/2[%rdi] R/3[%rsi] {sum.ml:11,18-69}
    L105:
    R/0[%rax] := acc/31[%rdi]
    reload retaddr
    return R/0[%rax]
    
  (function{sum.ml:14,12-68} camlSum__sum_rec_60 (arr/360: val)
   (app{sum.ml:16,2-18} "camlSum__loop_41"
     (or (>>u (load_mut int (+a arr/360 -8)) 9) 1) arr/360 1 1 val))
  
  *** After instruction selection
  camlSum__sum_rec_60(R/0[%rax]) {sum.ml:14,12-68}
    arr/29 := R/0[%rax]
    I/30 := 1
    I/31 := 1
    I/32 := int mut[arr/29 + -8]{sum.ml:15,12-28}
    I/33 := I/32
    I/33 := I/33 >>u 9{sum.ml:15,12-28}
    I/34 := I/33
    I/34 := I/34 | 1{sum.ml:15,12-28}
    R/0[%rax] := I/34
    R/1[%rbx] := arr/29
    R/2[%rdi] := I/31
    R/3[%rsi] := I/30
    tailcall "camlSum__loop_41" R/0[%rax] R/1[%rbx] R/2[%rdi] R/3[%rsi]{sum.ml:16,2-18}
  *** After instruction scheduling
  camlSum__sum_rec_60: {sum.ml:14,12-68}
    L110:
    arr/29[%rbx] := R/0[%rax]
    I/30[%rsi] := 1
    I/31[%rdi] := 1
    I/32[%rax] := int mut[arr/29[%rbx] + -8] {sum.ml:15,12-28}
    I/33[%rax] := I/33[%rax] >>u 9 {sum.ml:15,12-28}
    I/34[%rax] := I/34[%rax] | 1 {sum.ml:15,12-28}
    tailcall "camlSum__loop_41" R/0[%rax] R/1[%rbx] R/2[%rdi] R/3[%rsi] {sum.ml:16,2-18}
    
  (data
   int 3840
   global "camlSum"
   "camlSum":
   addr "camlSum__sum_11_closure"
   addr "camlSum__loop_41_closure"
   addr "camlSum__sum_rec_60_closure")
  (data)
  (data)
  (data)
  (function camlSum__entry () 1)
  
  *** After instruction selection
  camlSum__entry()
    I/29 := 1
    R/0[%rax] := I/29
    return R/0[%rax]
  *** After instruction scheduling
  camlSum__entry:
    L111:
    I/29[%rax] := 1
    return R/0[%rax]
    
  (data)
$ ls
$ objdump -d sum.o
