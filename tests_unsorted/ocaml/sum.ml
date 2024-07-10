let sum arr =
  let l = Array.length arr - 1 in
  let sum = ref 0 in
  for i = 0 to l - 1 do
    sum := !sum + Array.unsafe_get arr i
  done;
  !sum
;;

let rec loop len arr acc i =
  if i < len then loop len arr (acc + Array.unsafe_get arr i) (i + 1) else acc
;;

let sum_rec arr =
  let len = Array.length arr in
  loop len arr 0 0
;;
