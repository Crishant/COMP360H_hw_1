(* COMP 360H Project 1:  an interpreter for an imperative language.
 *
 * Christian Diaz Herrera
 * Cristina Gonzalez
 * Nishant Aggarwal
 *)

module E = Ast.Expression
module S = Ast.Stm
module F = Fun

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

    (* Stack Implementation using Lists.
     * Source: http://wide.land/modules/ex_stacks.html
     *
     *)

    module ListStack = struct
      type 'a stack = 'a list

      let empty = []
      let is_empty s = s = []
      let push x s = x :: s
      let peek = function
        | []   -> failwith @@ "Empty"
        | x::_ -> x
      let pop = function
        | []    -> failwith @@ "Empty"
        | _::xs -> xs
    end


    (* Environments.
     *
     * A value of type t is a map from identifiers to values.  We use σ to range
     * over environments and standard function notation when describing them.
     *)
    module Env = struct

      type bindingTable = Value.t IdMap.t

      type frame =
      | FunctionFrame of bindingTable list
      | ReturnFrame of Value.t

      type t = frame ListStack.stack

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

      let lookup (sigma : t) (x : Ast.Id.t) : Value.t =
        let currFrame = ListStack.peek sigma in
            match currFrame with
            | FunctionFrame currFrame' -> lookup' currFrame' x
            | ReturnFrame _ -> failwith @@ "Lookup in ReturnFrame"

      (* TODO: Fix Update *)
      let varBounded (currFrame : bindingTable list) (x : Ast.Id.t) : bool =
        match currFrame with
        | [] -> false
        | y :: ys -> IdMap.mem x y

      let rec update' (currFrame : bindingTable list) (x : Ast.Id.t) (v : Value.t) : t =
        match currFrame with
        | [] -> raise (UnboundVariable x)
        | y :: ys -> match IdMap.mem x y with
                        | true -> IdMap.add x v y
                        | false -> update' ys x v

      (*  update σ x v = σ{x → v}.
       *)
      let update (sigma : t) (x : Ast.Id.t) (v : Value.t) : t =
        let currFrame = ListStack.peek sigma in
            match currFrame with
            | FunctionFrame currFrame'-> let varInFrame = varBounded currFrame' x in
                                match varInFrame with
                                | true -> update' currFrame' x t
                                | false -> raise (UnboundVariable x)
            | ReturnFrame _ -> failwith @@ "Update in a return Frame"



      (* TODO: Fix newVarDec *)
      let newVarDec (sigma : t) (x : Ast.Id.t) (v : Value.t) : t =
          let currFrame = ListStack.peek sigma in
              match currFrame with
              | FunctionFrame currFrame' -> match currFrame' with
                                              | [] -> failwith @@ "VarDec in EmptyFrame"
                                              | y :: ys -> IdMap.add x v y
              | ReturnFrame _ -> failwith @@ "Variable Declaration in a Return Frame"

      let newVarDec (sigma : t) (x : Ast.Id.t) : t =
        let currFrame = ListStack.peek sigma in
            match currFrame with
            | FunctionFrame currFrame' -> match currFrame' with
                                            | [] -> failwith @@ "VarDec in EmptyFrame"
                                            | y :: ys -> IdMap.add x Value.V_None y
            | ReturnFrame _ -> failwith @@ "Variable Declaration in a Return Frame"

      (* TODO: Add a new block scope to the top frame*)
      let addBlock (sigma : t) : t =
        let currFrame = ListStack.peek sigma in
            match currFrame with
            | FunctionFrame y :: ys ->
            | ReturnFrame _ ->  failwith @@ "Unimplemented"

      (* TODO: Drop a block scope from the top frame *)
      let removeBlock
        let currFrame = ListStack.peek sigma in
            match currFrame with
            | FunctionFrame y :: ys -> ma
            | ReturnFrame ->  failwith @@ "Unimplemented"

      (* Bool on Weather top frame is returnFrame or FunctionFrame*)
      let isFuncFrame (sigma : t) : bool =
        let currFrame = ListStack.peek sigma in
            match currFrame with
            | FunctionFrame _ -> true
            | ReturnFrame _ -> false

      let addFrame (sigma : t) : t =
        ListStack.push IdMap.empty sigma

      let dropFrame (sigma : t) : t =
        ListStack.pop sigma

      (*  empty = σ, where dom σ = ∅.
       *)
      let empty : t = ListStack.empty

end

(* Code for all the Functions stuff
*)
module FunMap = Map.Make(Ast.Id)

module Fun = struct

    type t = (Ast.Id.t list * S.t list) FunMap.t

    let collectFun (l : Ast.Program.funDef list) (funMap : t) : t =
        match l with
        | [] -> funMap
        | (Ast.Program.FunDef (name, params, body)) :: xs ->  FunMap.add name (params, body) funMap
        | _ -> failwith "Error Collecting Functions"

    let collectFun (l : Ast.Program.funDef list) : t =
        collectFun l FunMap.empty

    let findFunc (funMap : t) (x : Ast.Id.t) : Ast.Id.t list * S.t list=
        try
            FunMap.find x funMap
        with
            | Not_found -> raise (UndefinedFunction x)

    let initFun (env : Env.t) (paramList : (Ast.Id.t * Value.t) list) : Env.t =
        let env' = Env.addFrame env in
            initFun' env' paramList

    let initFun' (env : Env.t) (paramList : (Ast.Id.t * Value.t) list) : Env.t =
        match paramlist with
            | [] -> env
            | (i, v) :: xs -> let env' = Env.newVarDec env i v in
                                                initFun' env' xs

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

  let rec eval (sigma : Env.t) (e : E.t) : Value.t * Env.t=
  (*! end !*)
    match e with
    | E.Var x -> failwith @@ "Unimplemented"
    | E.Num n -> failwith @@ "Unimplemented"
    | E.Bool n -> failwith @@ "Unimplemented"
    | E.Str n -> failwith @@ "Unimplemented"
    | E.Binop (op, e, e') -> failwith @@ "Unimplemented"
    | E.Assign (x, v) -> failwith @@ "Unimplemented"
    | E.Not n -> failwith @@ "Unimplemented"
    | E.Neg e -> failwith @@ "Unimplemented"
    | E.Call (x,l) -> failwith @@ "Unimplemented"

  and exec_stm(stm: S.t)(sigma: Env.t): Env.t = 
  match stm with 
  | S.Skip -> failwith @@ "Unimplemented"
  | S.VarDec l -> failwith @@ "Unimplemented"
  | S.Expr e -> failwith @@ "Unimplemented"
  | S.Block l -> failwith @@ "Unimplemented"
  | S.If(e, s0, s1) -> failwith @@ "Unimplemented"
  | S.While(e, s) -> failwith @@ "Unimplemented"
  | S.Return(e) -> failwith @@ "Unimplemented" 
    
(* exec p :  execute the program p according to the operational semantics
 * provided as a handout.
 *)
  and exec (stm : Ast.Program.t) (sigma : Env.t) : Env.t =
    failwith @@ "Unimplemented"


