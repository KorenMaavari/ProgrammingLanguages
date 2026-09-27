datatype Atom = SYMBOL of string | NIL;
datatype SExp = ATOM of Atom | CONS of (SExp * SExp);

exception Undefined;
exception Empty;

fun initEnv () : string -> SExp =
    fn _ => raise Undefined;

fun define (name: string) (oldEnv: string -> 'a) (value: 'a) =
    fn query =>
        if query = name
        then value
        else oldEnv query;

fun emptyNestedEnv () : (string -> SExp) list =
    [initEnv ()];

fun pushEnv env stack =
    env :: stack;

fun popEnv [] = raise Empty
  | popEnv (_ :: rest) = rest;

fun topEnv [] = raise Empty
  | topEnv (env :: _) = env;

fun defineNested name [] value = raise Empty
  | defineNested name (env :: rest) value =
        define name env value :: rest;

fun find name [] = raise Undefined
  | find name (env :: rest) =
        env name
        handle Undefined => find name rest;