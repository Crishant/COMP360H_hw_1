(* COMP 360H Project 1:  an interpreter for an imperative language.
 *
 * Christian Diaz Herrera
 * Cristina Gonzalez
 * Nishant Aggarwal
 *)

module E = Ast.Expression
module S = Ast.Stm

(* 'a IdentMap.t:  the type of maps from identifiers to 'a.
 *)
module IdentMap = Map.Make(Ast.Id)

(* MultipleDeclaration x is raised when x is declared more than once in a
 * block.
 *)
exception MultipleDeclaration of Ast.Id.t

(* UnboundVariable x is raised when x is used but not declared.
 *)
exception UnboundVariable of Ast.Id.t

(* UndefinedFunction f is raised when f is called but has not been defined.
 *)
exception UndefinedFunction of Ast.Id.t

(* TypeError s is raised when an operator or function is applied to operands
 * of the incorrect type.  s is any (hopefuly useful) message.
 *)
exception TypeError of string

(* Values.
 *)
module Value = struct
  type t = 
    | V_Undefined
    | V_None
    | V_Int of int
    | V_Bool of bool
    | V_Str of string
    [@@deriving show]

  (* to_string v = a string representation of v (more human-readable than
   * `show`.
   *)
  let to_string (v : t) : string =
    match v with
    | V_Undefined -> "?"
    | V_None -> "None"
    | V_Int n -> Int.to_string n
    | V_Bool b -> Bool.to_string b
    | V_Str s -> s
end

(* An implementation of the I/O API.  This is a little bit complex, because
 * this one implementation allows for a few variations:
 * - The input and output channel can be set by the client (default to
 *   standard input and output).
 * - The display of prompts (for the prompt_* functions) can be turned off
 *   (default on).
 * These variations let us use this module for interactive use (use the
 * defaults) and testing (redirect the i/o channels to a programmatic stream
 * and turn off the display of prompts.
 *
 * A client makes changes to the defaults by setting `in_channel`,
 * `out_channel`, and `show_prompts`.
 *)
module Api = struct

  (* Raised when a function is invoked that is not in the API.
   *)
  exception ApiError of string

  (* in_channel:  input channel (for get_*, prompt_* ).
   *)
  let in_channel : Scanf.Scanning.in_channel ref = 
    ref Scanf.Scanning.stdin

  (* out_channel:  output channel (for print_*, prompt_* when prompts are
   * displayed).
   *)
  let out_channel : Out_channel.t ref = ref Out_channel.stdout

  (* show_prompts:  true to display prompts, false to not display.
   *)
  let show_prompts : bool ref = ref true

  (* output oc s:  output `s` to `oc` and flush `oc`.
   *)
  let output (oc : Out_channel.t) (s : string) : unit =
    Out_channel.output_string oc s ;
    Out_channel.flush oc

  (* outputnl oc s = output `s ^ '\n'` to `oc` and flush `oc`.
   *)
  let outputnl (oc : Out_channel.t) (s : string) : unit =
    output oc (s ^ "\n")

  (* The API definition.  The API is specified by a
   * (string*(Value.t->Value.t)) list.  Each element names an API function
   * and provides the code to be executed when the function is called.
   *)
  let api : (Value.t list -> Value.t) IdentMap.t =
    [
      ("print_bool", fun vs ->
        match vs with
        | [Value.V_Bool n] -> 
          outputnl (!out_channel) (Bool.to_string n) ; Value.V_None
        | _ -> raise @@ TypeError "Bad argument type for print_bool"
      )
    ; ("get_bool", fun vs ->
        match vs with
        | [] -> Value.V_Bool (Scanf.bscanf !in_channel " %B" (fun b -> b))
        | _ -> raise @@ TypeError "Bad argument type for get_bool"
      )
    ; ("prompt_bool", fun vs ->
        match vs with
        | [Value.V_Str s] ->
          if !show_prompts then output (!out_channel) s else () ;
            Value.V_Bool (Scanf.bscanf !in_channel " %B" (fun b -> b))
        | _ -> raise @@ TypeError "Bad argument type for prompt_bool"
      )
    ; ("print_int", fun vs ->
        match vs with
        | [Value.V_Int n] -> 
          outputnl (!out_channel) (Int.to_string n) ; Value.V_None
        | _ -> raise @@ TypeError "Bad argument type for print_int"
      )
    ; ("get_int", fun vs ->
        match vs with
        | [] -> Value.V_Int (Scanf.bscanf !in_channel " %d" (fun n -> n))
        | _ -> raise @@ TypeError "Bad argument type for get_int"
      )
    ; ("prompt_int", fun vs ->
        match vs with
        | [Value.V_Str s] ->
          if !show_prompts then output (!out_channel) s else () ;
            Value.V_Int (Scanf.bscanf !in_channel " %d" (fun n -> n))
        | _ -> raise @@ TypeError "Bad argument type for prompt_int"
      )
    ; ("print_str", fun vs ->
         match vs with
         | [Value.V_Str s] -> 
           outputnl (!out_channel) s ; Value.V_None
         | _ -> raise @@ TypeError "Bad argument type for print_s"
      )
    ; ("get_str", fun vs ->
        match vs with
        | [] -> Value.V_Str (Scanf.bscanf !in_channel "%s" (fun s -> s))
        | _ -> raise @@ TypeError "Bad argument type for get_str"
      )
    ; ("prompt_str", fun vs ->
        match vs with
        | [Value.V_Str s] ->
          if !show_prompts then output (!out_channel) s else () ;
            Value.V_Str (Scanf.bscanf !in_channel " %s" (fun s -> s))
        | _ -> raise @@ TypeError "Bad argument type for prompt_str"
      )
    ] |> List.to_seq |> IdentMap.of_seq

  (* do_call f vs invokes the API function corresponding to `f` with argument
   * list `vs`.
   *
   * Raises ApiError f: if f is not an API function.
   *)
  let do_call (f : string) (vs : Value.t list) : Value.t =
    try
      IdentMap.find f api vs
    with
    | Not_found -> raise @@ ApiError f


end

(* The code for Env/Frame Stuffs
*)
module IdMap = Map.Make(Ast.Id)

    (* Environments.
     *
     * A value of type t is a map from identifiers to values.  We use σ to range
     * over environments and standard function notation when describing them.
     *)
    module Env = struct

      type bindingTable = Value.t IdMap.t

      type t =
      | FunctionFrame of bindingTable list
      | ReturnFrame of Value.t

      (* The type of environments.
       *)

      let rec lookup' (currFrame : bindingTable list) (x : Ast.Id.t) : Value.t =
        match currFrame with
          | [] -> raise (UnboundVariable x)
          | y :: ys ->
              try
                  IdMap.find x y
              with
                  | Not_found -> lookup' ys x

      let lookup (currFrame : t) (x : Ast.Id.t) : Value.t =
            match currFrame with
            | FunctionFrame currFrame' -> lookup' currFrame' x
            | ReturnFrame _ -> failwith @@ "Lookup in ReturnFrame"

      let rec varBounded (currFrame : bindingTable list) (x : Ast.Id.t) : bool * bindingTable =
        match currFrame with
        | [] -> raise (UnboundVariable x)
        | y :: ys -> match IdMap.mem x y with
                     | true -> (true, y)
                     | false -> varBounded ys x

      let rec update' (tables : bindingTable list) (x : Ast.Id.t) (v : Value.t): bindingTable list =
        match tables with
        | [] -> raise (UnboundVariable x)
        | currMap :: rest ->
            if IdMap.mem x currMap then
                (IdMap.add x v currMap) :: rest
            else
                let updatedRest = update' rest x v in
                    currMap :: updatedRest


      let update (currFrame : t) (x : Ast.Id.t) (v : Value.t) : t =
        match currFrame with
            | ReturnFrame _ -> failwith "Update in a return Frame"
            | FunctionFrame currFrame -> FunctionFrame (update' currFrame x v)




      let newVarDec (currFrame : t) (x : Ast.Id.t) (v : Value.t) : t =
              match currFrame with
              | ReturnFrame _ -> failwith @@ "Variable Declaration in a Return Frame"
              | FunctionFrame currFrame' -> match currFrame' with
                                              | [] -> failwith @@ "VarDec in EmptyFrame"
                                              | y :: ys -> FunctionFrame ((IdMap.add x v y) :: ys)

      let addBlock (currFrame : t) : t =
            match currFrame with
            | ReturnFrame _ ->  failwith @@ "Unimplemented"
            | FunctionFrame currFrame' -> match currFrame' with
                            | [] -> FunctionFrame (IdMap.empty :: [])
                            | y :: ys -> FunctionFrame (IdMap.empty :: y :: ys)

      let removeBlock (currFrame : t) : t =
            match currFrame with
            | ReturnFrame _ ->  failwith @@ "Unimplemented"
            | FunctionFrame currFrame' -> match currFrame' with
                            | [] -> failwith @@ "No Block to Remove"
                            | y :: ys -> FunctionFrame (ys)
      let isFuncFrame (currFrame : t) : bool =
            match currFrame with
            | FunctionFrame _ -> true
            | ReturnFrame _ -> false

      let newFuncFrame : t =
        FunctionFrame [IdMap.empty]

      let newReturnFrame (v : Value.t) : t =
        ReturnFrame v

      let empty : t = FunctionFrame [IdMap.empty]

end

(* Code for all the Functions stuff
*)
module FunMap = Map.Make(Ast.Id)

module Fun = struct

    type t = (Ast.Id.t list * S.t list) FunMap.t

    let collectFun (l : Ast.Program.fundef list) (funMap : t) : t =
        match l with
        | [] -> funMap
        | (Ast.Program.FunDef (name, params, body)) :: xs ->  FunMap.add name (params, body) funMap

    let collectFun (l : Ast.Program.fundef list) : t =
        collectFun l FunMap.empty

    let findFunc (funMap : t) (x : Ast.Id.t) : Ast.Id.t list * S.t list=
        try
            FunMap.find x funMap
        with
            | Not_found -> raise (UndefinedFunction x)

    let rec initFun' (env : Env.t) (paramList : (Ast.Id.t * Value.t) list) : Env.t =
        match paramList with
            | [] -> env
            | (i, v) :: xs -> let env' = Env.newVarDec env i v in
                                                initFun' env' xs

    let initFun (paramList : (Ast.Id.t * Value.t) list) : Env.t =
        let env = Env.newFuncFrame in
            initFun' env paramList


end

let binop (op : E.binop) (v : Value.t) (v' : Value.t) : Value.t =
  match (op, v, v') with
  | (E.Plus, Value.V_Int n, Value.V_Int n') -> Value.V_Int (n + n')
  | (E.Minus, Value.V_Int n, Value.V_Int n') -> Value.V_Int(n - n')
  | (E.Div, Value.V_Int n, Value.V_Int n') -> Value.V_Int(n / n')
  | (E.Times, Value.V_Int n, Value.V_Int n') -> Value.V_Int (n * n')
  | (E.And, Value.V_Bool n, Value.V_Bool n') -> Value.V_Bool (n && n')
  | (E.Mod, Value.V_Int n, Value.V_Int n') -> Value.V_Int (n mod n')
  | (E.Or, Value.V_Bool n, Value.V_Bool n') -> Value.V_Bool(n || n')
  | (E.Eq, Value.V_Bool n, Value.V_Bool n') -> Value.V_Bool (n = n')
  | (E.Eq, Value.V_Int n, Value.V_Int n') -> Value.V_Bool(n = n')
  | (E.Le, Value.V_Int n, Value.V_Int n') -> Value.V_Bool (n <= n')
  | (E.Ge, Value.V_Int n, Value.V_Int n') -> Value.V_Bool (n >= n')
  | (E.Ne, Value.V_Bool n, Value.V_Bool n') -> Value.V_Bool (n <> n')
  | (E.Ne, Value.V_Int n, Value.V_Int n') -> Value.V_Bool(n <> n')
  | (E.Lt, Value.V_Int n, Value.V_Int n') -> Value.V_Bool(n < n')
  | (E.Gt, Value.V_Int n, Value.V_Int n') -> Value.V_Bool(n > n')
  | _ -> failwith @@ "Something went Wrong!"

let rec zip (l1 : Ast.Id.t list) (l2 : Value.t list) : (Ast.Id.t * Value.t) list = 
  match l1, l2 with 
  | [],[] -> []
  | x::xs, y::ys -> (x, y) :: zip xs ys
  | _ -> failwith @@ "No lists"


  

  let rec eval (sigma : Env.t) (e : E.t) (f: Fun.t) : Value.t * Env.t =
    match e with
    | E.Var x -> (Env.lookup sigma x, sigma)
    | E.Num n -> (Value.V_Int n, sigma)
    | E.Bool b -> (Value.V_Bool b, sigma)
    | E.Str s -> (Value.V_Str s, sigma)
    | E.Binop (op, e1, e2) ->
      let (v1, sigma1) = eval sigma e1 f in
      let (v2, sigma2) = eval sigma1 e2 f in
      (binop op v1 v2, sigma2)
    | E.Assign (x, e) ->
      let (v, sigma') = eval sigma e f in
      let sigma2 = Env.update sigma' x v in
      (v, sigma2)
    | E.Not e ->
      let (v, sigma') = eval sigma e f in
      (match v with
       | Value.V_Bool b -> (Value.V_Bool (not b), sigma')
       | _ -> failwith "Type Error")
    | E.Neg e ->
      let (v, sigma') = eval sigma e f in
      (match v with
       | Value.V_Int n -> (Value.V_Int (-n), sigma')
       | _ -> failwith "Type Error")
    | E.Call (func, l) ->
      let (vl, sigma') = eval_all l sigma f in
      let (xl, sl) = Fun.findFunc f func in
      let xvl = zip xl vl in
      let sigma2 = Fun.initFun xvl in
      (match exec_stm sl sigma2 with
       | Env.ReturnFrame v -> (v, sigma')
       | _ -> failwith "Not a return frame")


  and eval_all(el: E.t list) (sigma: Env.t) (f: Fun.t) : Value.t list * Env.t = 
  match el with 
  | [] -> ([], sigma)
  | x :: xs -> 
    let (v, sigma') = eval sigma x f  in
    let (vs, sigma2) = eval_all xs sigma' f in
    (v::vs, sigma2)

  and exec_stm(stm: S.t)(sigma: Env.t)(f : Fun.t): Env.t =
  match stm with 
  | S.Skip -> sigma
  | S.VarDec l -> 
    match l with 
    | x :: xs -> 
      match x with
      | var -> 
        let sigma' = Env.newVarDec sigma var V_Undefined in
        exec_stm xs sigma' f
      | (var, e) -> 
        let (v, sigma') = eval sigma e f in
        let sigma2 = Env.newVarDec sigma' var v in
        exec_stm xs sigma2 f
  | S.Expr e ->
    let (v, sigma') = eval sigma e f in
    sigma'
  | S.Block l -> stm_list l sigma f
  | S.If(e, s0, s1) -> 
    let (v, sigma') = eval sigma e f in
    match v with
    | Value.V_Bool true -> let (_,sigma2) = exec_stm s0 sigma' f in sigma2
    |_ -> let (_,sigma2) = exec_stm s1 sigma' f in sigma2
  | S.While(e, s) -> loop e s sigma f
  | S.Return(e) ->
    match e with
    | e ->  e
    |_ -> Value.V_None

  and loop (e : E.t) (s : S.t) (sigma : Env.t) (f : Fun.t) : Env.t =
    let (v, sigma') = eval sigma e f in
    match v with
    | Value.V_Bool false -> sigma'
    | _ -> failwith @@ "Not a bool"
    | Value.V_Bool true -> let sigma2 = exec_stm s sigma' f in
        match sigma2 with
        | ReturnFrame _ -> sigma2
        | FunctionFrame _ -> loop e s sigma2 f
    
  and stm_list (ss : S.t list)(sigma: Env.t)(f : Fun.t) : Env.t =
    match ss with
    | [] -> sigma
    | x::xs -> 
      let sigma' = exec_stm x sigma f in
        match sigma' with 
        | Env.FunctionFrame _ -> 
            let sigma2 = exec_stm xs sigma' f in
              sigma2
        | Env.ReturnFrame _ -> 
            sigma'

    
(* exec p :  execute the program p according to the operational semantics
 * provided as a handout.
 *)
  let exec (stm : Ast.Program.t) : Value.t =
    let f = Fun.collectFun stm in
    let funName = Ast.Id.t "main" in
    let (param_list, stmt_list) = Fun.findFunc f funName in
    let env = Env.newFuncFrame in
    let (v, sigma) = eval env (E.Call param_list stmt_list) f in
        v

