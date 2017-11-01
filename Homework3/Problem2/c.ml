type aexp = 
  | CONST of int 
  | VAR of var
  | ADD of aexp * aexp 
  | MUL of aexp * aexp 
  | SUB of aexp * aexp
  
and bexp = 
  | TRUE 
  | FALSE 
  | EQ of aexp * aexp 
  | LT of aexp * aexp 
  | NOT of bexp 
  | AND of bexp * bexp

and stmt =
  | ASSIGN of var * aexp 
  | SKIP
  | SEQ of stmt * stmt
  | IF of bexp * stmt * stmt
  | WHILE of bexp * stmt
  | BLOCK of var * aexp * stmt
  | READ of var
  | PRINT of aexp 
and var = string
type program = stmt

type value = Int of int
type loc = Loc of int 
type env = var -> loc
type mem = loc -> value

let empty_env = fun x -> raise (Failure "Environment is empty")
let extend_env (x,v) e = fun y -> if x = y then v else (e y)
let apply_env e x = e x

let empty_mem = fun _ -> raise (Failure "Memory is empty")
let extend_mem (l,v) m = fun y -> if l = y then v else (m y)
let apply_mem m l = m l

let counter = ref 0
let new_location () = counter:=!counter+1; Loc (!counter)

exception NotImplemented

let rec eval_aexp : aexp -> env -> mem -> int
=fun a env mem -> raise NotImplemented

and eval_bexp : bexp -> env -> mem -> bool
=fun b env mem -> raise NotImplemented 

let rec eval : stmt -> env -> mem -> mem
=fun s env mem -> 
  match s with
  | READ x -> extend_mem (apply_env env x, Int (read_int())) mem (* Do not modify *)
  | PRINT e -> print_int (eval_aexp e env mem); print_newline(); mem (* Do not modify *)
  | _ -> raise NotImplemented (* TODO *)

let execute : program -> unit 
=fun pgm -> ignore (eval pgm empty_env empty_mem)
