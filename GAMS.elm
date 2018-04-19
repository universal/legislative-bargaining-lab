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
add exp1 exp2 =
    case ( exp1, exp2 ) of
        ( Num n1, Num n2 ) ->
            Num (n1 + n2)

        ( Num 0, _ ) ->
            exp2

        ( _, Num 0 ) ->
            exp1

        _ ->
            BinOp Add exp1 exp2


minus : Exp -> Exp -> Exp
minus exp1 exp2 =
    case ( exp1, exp2 ) of
        ( Num n1, Num n2 ) ->
            Num (n1 - n2)

        _ ->
            BinOp Minus exp1 exp2


mult : Exp -> Exp -> Exp
mult exp1 exp2 =
    case ( exp1, exp2 ) of
        ( Num n1, Num n2 ) ->
            Num (n1 * n2)

        ( Num 0, _ ) ->
            Num 0

        ( Num 1, _ ) ->
            exp2

        ( _, Num 0 ) ->
            Num 0

        ( _, Num 1 ) ->
            exp1

        _ ->
            BinOp Mult exp1 exp2


prettyExp : Exp -> String
prettyExp exp =
    case exp of
        Num int ->
            toString int

        Var string ->
            string

        BinOp op exp exp2 ->
            "(" ++ prettyExp exp ++ " " ++ prettyOp op ++ " " ++ prettyExp exp2 ++ ")"

replaceNum : String -> Int -> Exp -> Exp
replaceNum var const exp =
    case exp of
        Var v2 ->
            if var == v2 then
                Num const
            else
                Var v2
        BinOp op exp1 exp2 -> BinOp op (replaceNum var const exp1) (replaceNum var const exp2)
        _ -> exp

replaceNumStmt : String -> Int -> Stmt -> Stmt
replaceNumStmt rep const (Assign var exp) =
    Assign var (replaceNum rep const exp)

replaceNumStmts : (String, Int) -> List Stmt -> List Stmt
replaceNumStmts (var, const) stmts =
    List.map (replaceNumStmt var const) stmts

simplifyExp : Exp -> Exp
simplifyExp exp =
    case exp of
        (BinOp Add exp1 (BinOp Minus (Num 1) exp2)) ->
            if exp1 == exp2 then
                Num 1
            else
                BinOp Add (simplifyExp exp1) (BinOp Minus (Num 1) (simplifyExp exp2))
        (BinOp Mult (Num i) (Num j)) -> Num (i * j)
        (BinOp Mult (Num 0) exp2) -> Num 0
        (BinOp Mult (Num 1) exp2) -> exp2
        (BinOp Mult exp1 (Num 0)) -> Num 0
        (BinOp Mult exp1 (Num 1)) -> exp1
        (BinOp Add (Num i) (Num j)) -> Num (i+j)
        (BinOp Minus (Num i) (Num j)) -> Num (i-j)
        (BinOp Add exp1 (Num 0)) -> exp1
        (BinOp Add (Num 0) exp2) -> exp2
        (BinOp op exp1 exp2) ->
            let
                sexp1 = (simplifyExp exp1)
                sexp2 = (simplifyExp exp2)
            in
                if exp1 == sexp1 && exp2 == sexp2 then
                    BinOp op exp1 exp2
                else
                    simplifyExp (BinOp op sexp1 sexp2)
        _ -> exp

simplifyStmt : Stmt -> Stmt
simplifyStmt (Assign var exp) =
    Assign var (simplifyExp exp)

numStmt : Stmt -> Bool
numStmt (Assign var exp) =
    case exp of
        Num i -> True
        _ -> False

extractVar : Stmt -> (String, Int)
extractVar (Assign var exp) =
    case exp of
        Num i -> (var, i)
        _ -> Debug.crash "selected a num statement where i shouldn't have"

simplifyStmts : List Stmt -> List Stmt
simplifyStmts stmts =
    let
        simplified = List.map simplifyStmt stmts
        (nums, rest) = List.partition numStmt simplified
        result = if List.length rest > 0 then
                List.map simplifyStmt (List.foldl replaceNumStmts rest (List.map extractVar nums))
            else
                List.map simplifyStmt nums
    in
        if List.length nums == 0  || List.length rest == 1 then
            result
        else
            simplifyStmts result


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
    Dict.fromList (List.map (\i -> ( i, "PI(g, \"" ++ toString i ++ "\")" )) (List.range 0 (n - 1)))


stmt : QOBDD -> ( List Stmt, String )
stmt qobdd =
    let
        ( stmts, v, vs ) =
            stmtTree (vars qobdd.vars) qobdd.bdd
    in
    ( simplifyStmts stmts ++ [ "%1" := v ], setVars vs )



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
                assignment =
                    term i := add (mult (Var (ident label)) v1) (mult (minus (Num 1) (Var (ident label))) v2)
            in
            ( s1 ++ s2 ++ [ assignment ], Var (term i), i :: vars1 ++ vars2 )
    in
    QOBDD.foldBDD ( [], Num 0, [] ) ( [], Num 1, [] ) ref node

stmtDiff : QOBDD -> ( List Stmt, String )
stmtDiff qobdd = stmtAlphaSimplified stmtTreeDiff qobdd

stmtAlphaDiff : QOBDD -> ( List Stmt, String )
stmtAlphaDiff qobdd = stmtAlphaSimplified stmtTreeAlphaDiff qobdd


stmtAlphaSimplified : (Dict Int String -> Int -> BDD -> ( List Stmt, Exp, List Int )) -> QOBDD -> ( List Stmt, String )
stmtAlphaSimplified f qobdd =
     let
        -- List Int
        actors = List.range 0 (qobdd.vars - 1)
        -- Dict Int String
        varList = vars qobdd.vars
        -- List ( List Stmt, Exp, List Int )
        -- list of tuples, with each tuple containing a list of stmts, an exp, and a list of Ints
        result = List.map (\i -> f varList i qobdd.bdd) actors
        mergeTrees : ( List Stmt, Exp, List Int ) -> ( List Stmt, String ) -> ( List Stmt, String )
        mergeTrees (stmts, v, vs) (merged_smts, merged_vars) = (merged_smts ++ (simplifyStmts stmts) ++ [ "%1" := v ], merged_vars)
     in
     ( List.foldl mergeTrees ([], "") result )


stmtTreeDiff : Dict Int String -> Int -> BDD -> ( List Stmt, Exp, List Int )
stmtTreeDiff vars checkIdent =
    let
        term i =
            "diff_t(\"" ++ toString checkIdent ++ "\", \"" ++ toString i ++ "\")"

        ident i =
            case Dict.get i vars of
                Nothing ->
                    Debug.crash ("Error: " ++ toString i ++ " not found in " ++ toString vars)

                Just v ->
                    v

        ref i =
            ( [], Var (term i), [] )
        check = Var (ident checkIdent)
        node i ( s1, v1, vars1 ) label ( s2, v2, vars2 ) =
            let
                player label v2 =
                    let
                        playerVar = Var (ident label)
                    in
                    if check == playerVar then
                        (mult (Num -1) (mult (Num 1) v2))
                    else
                        (mult (Num 1) v2)
                assignment =
                    term i := add (mult (Num 1) v1) (player label v2)
            in
            ( s1 ++ s2 ++ [ assignment ], Var (term i), i :: vars1 ++ vars2 )
    in
    QOBDD.foldBDD ( [], Num 0, [] ) ( [], Num 1, [] ) ref node

stmtTreeAlphaDiff : Dict Int String -> Int -> BDD -> ( List Stmt, Exp, List Int )
stmtTreeAlphaDiff vars checkIdent =
    let
        term i =
            "alphadiff_t(g, \"" ++ toString checkIdent ++ "\", \"" ++ toString i ++ "\")"

        ident i =
            case Dict.get i vars of
                Nothing ->
                    Debug.crash ("Error: " ++ toString i ++ " not found in " ++ toString vars)

                Just v ->
                    v

        ref i = ( [], Var (term i), [] )

        check = Var (ident checkIdent)

        node i ( s1, v1, vars1 ) label ( s2, v2, vars2 ) =
            let
                lplayer label v1 =
                    let
                        playerVar = Var (ident label)
                    in
                    if check == playerVar then
                        (mult (minus (Num 1) check) (mult playerVar v1))
                    else
                        (mult playerVar v1)

                rplayer label v2 =
                    let
                        playerVar = Var (ident label)
                    in
                    if check == playerVar then
                        (mult (mult (Num -1) check) (mult (minus (Num 1) playerVar) v2))
                    else
                        (mult (minus (Num 1) playerVar) v2)

                assignment =
                    term i := add (lplayer label v1) (rplayer label v2)
            in
            ( s1 ++ s2 ++ [ assignment ], Var (term i), i :: vars1 ++ vars2 )
    in
    QOBDD.foldBDD ( [], Num 0, [] ) ( [], Num 1, [] ) ref node