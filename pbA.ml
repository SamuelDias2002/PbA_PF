open Printf
(* Variaveis necessárias *)
let cont2 = ref 0
let cont1 = ref 0 

(* Funções recurisvas  *)

let rec sch2 a =
  
  if a < 0 || a > 20 then failwith "ERRO" else 
    incr cont2;
  if a == 0 then 1 else
  if a == 1 then 2
  else ((6*a-3)*sch2(a-1)/(a+1))-((((a-2)* sch2(a-2)/(a+1))))

let rec sch1 a =
  
  let rec soma n =
  let rec soma_aux i n acc =
      if i > n-2 then acc 
      else soma_aux (i+1) n (acc+(sch1(i)*sch1(n-i-1))) in 
      soma_aux 1 n 0 in

  if a < 0 && a > 20 then failwith "ERRO" else
    incr cont1;
  if a == 0 then 1 else
  if a == 1 then 2
  else 3*sch1(a-1)+soma(a)



(* Incompleta - Função recurisva usando modulo Zarith

let rec sch3  b = 

  if b < Z.zero || b > (Z.of_int 10000) then failwith "ERRO" else
  if b == Z.zero then Z.one else
  if b == Z.one then (Z.of_int 2)
  else (((Z.of_int 6)) Z.mul (b Z.sub (Z.of_int  3)) Z.mul sch(b Z.sub Z.one) Z.div (b Z.add Z.one) ) Z.sub ((((b Z.sub (Z.of_int  2)) Z.mul sch(b Z.add (Z.of_int  2)) Z.div (b Z.add Z.one))))
*)

let main =
let (a, b) = Scanf.scanf "%d %d" (fun a b -> (a,b)) in
let funcao_sch2 = sch2 a in
let funcao_sch1 = sch1 a in
let funcao_sch = sch3 b in
Printf.printf "%d %d\n%d %d\n" funcao_sch1 (!cont1) funcao_sch2 (!cont2)
let funcao_scb = sch b in
let _ = Z.print b;
