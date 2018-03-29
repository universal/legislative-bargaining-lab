module GAMS exposing (..)

import Dict exposing (Dict)
import QOBDD exposing (..)


type Op
    = Mult
    | Add
    | Minus


prettyOp : Op -> String
prettyOp op =
    case op of
        Mult ->
            "*"

        Add ->
            "+"

        Minus ->
            "-"


type Exp
    = Num Int
    | Var String
    | BinOp Op Exp Exp


add : Exp -> Exp -> Exp
add =
    BinOp Add


minus : Exp -> Exp -> Exp
minus =
    BinOp Minus


mult : Exp -> Exp -> Exp
mult =
    BinOp Mult


prettyExp : Exp -> String
prettyExp exp =
    case exp of
        Num int ->
            toString int

        Var string ->
            string

        BinOp Add (Num i) (Num k) -> toString (i + k)
        BinOp Add (Num 0) exp -> prettyExp exp
        BinOp Add exp (Num 0) -> prettyExp exp
        BinOp Minus (Num i) (Num k) -> toString (i - k)
        BinOp Mult (Num 0) exp2 -> "0"
        BinOp Mult exp (Num 0) -> "0"
        BinOp op exp exp2 ->
            "(" ++ prettyExp exp ++ " " ++ prettyOp op ++ " " ++ prettyExp exp2 ++ ")"


type Stmt
    = Assign String Exp


(:=) : String -> Exp -> Stmt
(:=) var exp =
    Assign var exp


prettyStmts : List Stmt -> String
prettyStmts stmts =
    String.concat (List.map prettyStmt stmts)


prettyStmt : Stmt -> String
prettyStmt stmt =
    case stmt of
        Assign var exp ->
            var ++ " = " ++ prettyExp exp ++ ";\n"


vars : Int -> Dict Int String
vars n =
    Dict.fromList (List.map (\i -> ( i, "p0(\"" ++ toString i ++ "\", g)" )) (List.range 1 n))


stmt : QOBDD -> ( List Stmt, String )
stmt qobdd =
    let
        ( stmts, v, vs ) =
            stmtTree (vars qobdd.vars) qobdd.bdd
    in
    ( stmts ++ [ "%1" := v ], setVars vs )



-- Generates a string in the form of
-- set nodes /19588, 19586, 19590, 19582, 19584, 19592/;


setVars : List Int -> String
setVars vars =
    let
        context v =
            "set nodes /" ++ v ++ "/;"
    in
    context (String.concat <| List.intersperse ", " <| List.map toString vars)


stmtTree : Dict Int String -> BDD -> ( List Stmt, Exp, List Int )
stmtTree vars =
    let
        term i =
            "t(g, \"" ++ toString i ++ "\")"

        ident i =
            case Dict.get i vars of
                Nothing ->
                    Debug.crash ("Error: " ++ toString i ++ " not found in " ++ toString vars)

                Just v ->
                    v

        ref i =
            ( [], Var (term i), [] )

        node i ( s1, v1, vars1 ) label ( s2, v2, vars2 ) =
            let
                check = ident 1
                player label v2 =
                    case (Var (ident label)) of
                        Var check -> Num 0
                        x -> (mult (minus (Num 1) x) v2)
                assignment =
                    term i := add (mult (Var (ident label)) v1) (mult (minus (Num 1) (Var (ident label))) v2)
            in
            ( s1 ++ s2 ++ [ assignment ], Var (term i), i :: vars1 ++ vars2 )
    in
    QOBDD.foldBDD ( [], Num 0, [] ) ( [], Num 1, [] ) ref node

stmtAlphaWin : QOBDD -> ( List Stmt, String )
stmtAlphaWin qobdd =
    let
        actors = List.range 1 qobdd.vars
        varList = vars qobdd.vars
        result = List.map (\i -> stmtTreeAlphaWin varList i qobdd.bdd) actors
        out = List.map (\stmts -> \v -> \vs -> ( stmts ++ [ "%1" := v ], setVars vs )) result
    in
    ( out )

stmtTreeAlphaWin : Dict Int String -> Int -> BDD -> ( List Stmt, Exp, List Int )
stmtTreeAlphaWin vars checkIdent =
    let
        term i =
            "talpha_win(\"" ++ toString checkIdent ++ "\", g, \"" ++ toString i ++ "\")"

        ident i =
            case Dict.get i vars of
                Nothing ->
                    Debug.crash ("Error: " ++ toString i ++ " not found in " ++ toString vars)

                Just v ->
                    v

        ref i =
            ( [], Var (term i), [] )

        node i ( s1, v1, vars1 ) label ( s2, v2, vars2 ) =
            let
                check = ident checkIdent
                player label v2 =
                    case (Var (ident label)) of
                        Var check -> Num 0
                        x -> (mult (minus (Num 1) x) v2)
                assignment =
                    term i := add (mult (Var (ident label)) v1) (player label v2)
            in
            ( s1 ++ s2 ++ [ assignment ], Var (term i), i :: vars1 ++ vars2 )
    in
    QOBDD.foldBDD ( [], Num 0, [] ) ( [], Num 1, [] ) ref node
