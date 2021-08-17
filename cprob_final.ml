open Scanf
open Printf
open List

let rec guloso listaTrocos total trocos =
  if total = 0 then length trocos
  else let moeda = find ((>=) total) listaTrocos in
    guloso listaTrocos (total - moeda) (moeda::trocos)

let rec verifica x listaTrocos = function
  | [] -> 0
  | h::t -> try if guloso (rev listaTrocos) x [] > (guloso (rev listaTrocos) (x-h) []) + 1 then x 
        else verifica x listaTrocos t
      with Not_found -> verifica x listaTrocos t

let rec sistemaTrocos listaTrocos maximo minimo =
  if minimo == maximo then -1
  else
    let newlist = filter (fun moeda -> moeda < minimo) listaTrocos in
    let resultado = verifica minimo listaTrocos newlist in
    if resultado == minimo then minimo
    else sistemaTrocos listaTrocos maximo (minimo+1)

let leitura() =
  let n = scanf "%d\n" (fun a -> a) in
  if n > 100 then exit 1
  else let listaTrocos = init n (fun _ -> scanf "%d\n" (fun a -> a)) in
    let checklist = filter (fun moeda -> moeda > 500) listaTrocos in
    if checklist = [] then (n, listaTrocos)
    else exit 1

let n, listaTrocos = leitura()

let maximo = if List.length listaTrocos > 2 then (nth listaTrocos (length listaTrocos - 1)) + ( nth listaTrocos (length listaTrocos - 2)) else 0

let minimo = if List.length listaTrocos > 2 then nth listaTrocos 2 + 2 else 0

let resultado = sistemaTrocos listaTrocos maximo minimo

let() =
  if resultado = -1 || minimo = 0 then printf "YES\n"
  else
    printf "%d\n" (resultado)