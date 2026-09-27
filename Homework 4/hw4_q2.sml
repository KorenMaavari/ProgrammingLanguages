use "hw4_q1.sml";

(* TODO, REPLACE WITH PATH TO YOUR PARSER *)
use "hw3_q3.sml";

exception LispError;

(* Helper function - feel free to delete *)
fun first (x, _) = x;

local
    fun tokenize x = 
        String.tokens (fn c: char => c = #" ") 
            (String.translate (fn #"(" => "( " | #")" => " )" | c => str c) x);

    (* Helper functions - feel free to delete *)
    (* ====================================== *)
    fun is_digit c = c >= #"0" andalso c <= #"9";

    fun is_number str =
        let
            fun check [] = true
            | check (c::cs) = is_digit c andalso check cs
            
            val chars = String.explode str
        in
            if List.null chars then false else check chars
        end;
        
    fun char_to_int c = ord(c) - ord(#"0")

    fun string_to_int str =
        let
            fun convert [] acc = acc
            | convert (c::cs) acc = convert cs (10 * acc + char_to_int c)
        in
            convert (String.explode str) 0
        end;

    fun sexp_to_int sexp =
        case sexp of
            ATOM (SYMBOL s) => string_to_int s
          | _ => raise LispError;
    (* ====================================== *)

    fun sexp_list_to_ml_list (ATOM NIL) = []
      | sexp_list_to_ml_list (CONS (h, t)) = h :: sexp_list_to_ml_list t
      | sexp_list_to_ml_list _ = raise LispError;

    fun evalSExp (ATOM NIL) env = (ATOM NIL, env)

      | evalSExp (ATOM (SYMBOL s)) env =
            if s = "t" then (ATOM (SYMBOL "t"), env)
            else if is_number s then (ATOM (SYMBOL s), env)
            else (find s env, env)

      | evalSExp (CONS (ATOM (SYMBOL "quote"), CONS (arg, ATOM NIL))) env =
            (arg, env)

      | evalSExp (CONS (ATOM (SYMBOL "cons"), CONS (a, CONS (b, ATOM NIL)))) env =
            let val (va, _) = evalSExp a env
                val (vb, _) = evalSExp b env
            in (CONS (va, vb), env) end

      | evalSExp (CONS (ATOM (SYMBOL "car"), CONS (arg, ATOM NIL))) env =
            (case evalSExp arg env of
                (CONS (h, _), _) => (h, env)
              | _ => raise LispError)

      | evalSExp (CONS (ATOM (SYMBOL "cdr"), CONS (arg, ATOM NIL))) env =
            (case evalSExp arg env of
                (CONS (_, t), _) => (t, env)
              | _ => raise LispError)

      | evalSExp (CONS (ATOM (SYMBOL "atom"), CONS (arg, ATOM NIL))) env =
            (case evalSExp arg env of
                (ATOM _, _) => (ATOM (SYMBOL "t"), env)
              | _ => (ATOM NIL, env))

      | evalSExp (CONS (ATOM (SYMBOL "null"), CONS (arg, ATOM NIL))) env =
            (case evalSExp arg env of
                (ATOM NIL, _) => (ATOM (SYMBOL "t"), env)
              | _ => (ATOM NIL, env))

      | evalSExp (CONS (ATOM (SYMBOL "eq"), CONS (a, CONS (b, ATOM NIL)))) env =
            (case (evalSExp a env, evalSExp b env) of
                ((ATOM x, _), (ATOM y, _)) =>
                    if x = y then (ATOM (SYMBOL "t"), env) else (ATOM NIL, env)
              | _ => (ATOM NIL, env))

      | evalSExp (CONS (ATOM (SYMBOL "cond"), clauses)) env =
            evalCond clauses env

      | evalSExp (CONS (ATOM (SYMBOL "+"),   CONS (a, CONS (b, ATOM NIL)))) env = evalArith (op +)   a b env
      | evalSExp (CONS (ATOM (SYMBOL "-"),   CONS (a, CONS (b, ATOM NIL)))) env = evalArith (op -)   a b env
      | evalSExp (CONS (ATOM (SYMBOL "*"),   CONS (a, CONS (b, ATOM NIL)))) env = evalArith (op * )  a b env
      | evalSExp (CONS (ATOM (SYMBOL "/"),   CONS (a, CONS (b, ATOM NIL)))) env = evalArith (op div) a b env
      | evalSExp (CONS (ATOM (SYMBOL "mod"), CONS (a, CONS (b, ATOM NIL)))) env = evalArith (op mod) a b env

      | evalSExp (CONS (ATOM (SYMBOL "="),   CONS (a, CONS (b, ATOM NIL)))) env = evalCmp (op =)  a b env
      | evalSExp (CONS (ATOM (SYMBOL "/="),  CONS (a, CONS (b, ATOM NIL)))) env = evalCmp (op <>) a b env
      | evalSExp (CONS (ATOM (SYMBOL "<"),   CONS (a, CONS (b, ATOM NIL)))) env = evalCmp (op <)  a b env
      | evalSExp (CONS (ATOM (SYMBOL ">"),   CONS (a, CONS (b, ATOM NIL)))) env = evalCmp (op >)  a b env

      | evalSExp (lam as CONS (ATOM (SYMBOL "lambda"), _)) env = (lam, env)

      | evalSExp (lbl as CONS (ATOM (SYMBOL "label"), _)) env = (lbl, env)

      | evalSExp (CONS (lam as CONS (ATOM (SYMBOL "lambda"), _), args)) env =
            applyFunc lam (List.map (fn a => #1 (evalSExp a env)) (sexp_list_to_ml_list args)) env

      | evalSExp (CONS (lbl as CONS (ATOM (SYMBOL "label"), _), args)) env =
            applyFunc lbl (List.map (fn a => #1 (evalSExp a env)) (sexp_list_to_ml_list args)) env

      | evalSExp (CONS (ATOM (SYMBOL fname), args)) env =
            applyFunc (find fname env) (List.map (fn a => #1 (evalSExp a env)) (sexp_list_to_ml_list args)) env

      | evalSExp _ _ = raise LispError

    and evalArith oper a b env =
            let val va = sexp_to_int (#1 (evalSExp a env))
                val vb = sexp_to_int (#1 (evalSExp b env))
            in (ATOM (SYMBOL (Int.toString (oper (va, vb)))), env) end

    and evalCmp pred a b env =
            let val va = sexp_to_int (#1 (evalSExp a env))
                val vb = sexp_to_int (#1 (evalSExp b env))
            in if pred (va, vb) then (ATOM (SYMBOL "t"), env)
               else (ATOM (SYMBOL "nil"), env) end

    and evalCond (ATOM NIL) env = (ATOM NIL, env)
      | evalCond (CONS (CONS (cond_e, CONS (then_e, ATOM NIL)), rest)) env =
            (case #1 (evalSExp cond_e env) of
                ATOM NIL => evalCond rest env
              | _        => evalSExp then_e env)
      | evalCond _ _ = raise LispError

    and applyFunc (CONS (ATOM (SYMBOL "lambda"),
                         CONS (params, CONS (body, ATOM NIL)))) evaledArgs env =
            let val names = List.map (fn ATOM (SYMBOL s) => s | _ => raise LispError)
                                     (sexp_list_to_ml_list params)
                val _ = if List.length names <> List.length evaledArgs then raise LispError else ()
                val newScope = ListPair.foldl (fn (n, v, e) => define n e v) (initEnv ()) (names, evaledArgs)
                val (result, _) = evalSExp body (pushEnv newScope env)
            in (result, env) end

      | applyFunc (CONS (ATOM (SYMBOL "label"),
                         CONS (ATOM (SYMBOL name),
                               CONS (lam as CONS (ATOM (SYMBOL "lambda"), _), ATOM NIL))))
                  evaledArgs env =
            let val labelExp = CONS (ATOM (SYMBOL "label"), CONS (ATOM (SYMBOL name), CONS (lam, ATOM NIL)))
                val CONS (_, CONS (params, CONS (body, ATOM NIL))) = lam
                val names = List.map (fn ATOM (SYMBOL s) => s | _ => raise LispError)
                                     (sexp_list_to_ml_list params)
                val _ = if List.length names <> List.length evaledArgs then raise LispError else ()
                val newScope = ListPair.foldl (fn (n, v, e) => define n e v)
                                   (define name (initEnv ()) labelExp) (names, evaledArgs)
                val (result, _) = evalSExp body (pushEnv newScope env)
            in (result, env) end

      | applyFunc _ _ _ = raise LispError

in
    fun eval string_exp env =
        (evalSExp (parse (tokenize string_exp)) env)
        handle LispError => (ATOM (SYMBOL "lisp-error"), env)
             | Undefined => (ATOM (SYMBOL "lisp-error"), env)
             | Empty     => (ATOM (SYMBOL "lisp-error"), env)
end;
