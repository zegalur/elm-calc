module Expression exposing (..)


import Debug as D


type alias Expression = List Lexem

type Lexem
    = L_Const Float    -- Some number
    -- | L_ParOpen        -- Parentheses open
    -- | L_ParClose       -- Parentheses close
    | L_ArOp ArOp      -- Binary arithmetic operation
    | L_UnOp UnOp      -- Unary operator (function)

type ArOp
    = AOP_Add
    | AOP_Sub
    | AOP_Mul
    | AOP_Div
    | AOP_Pow
    | AOP_Root
    | AOP_nCr -- combination (n r)
    | AOP_nPr -- permutation 

type UnOp
    = UOP_Log
    | UOP_10x -- 10 ^ x
    | UOP_Ln
    | UOP_Exp -- e ^ x
    | UOP_Sin
    | UOP_Cos
    | UOP_Tan
    | UOP_Sin1 -- inverse
    | UOP_Cos1 -- inverse
    | UOP_Tan1 -- inverse
    | UOP_HSin
    | UOP_HCos
    | UOP_HTan
    | UOP_HSin1 -- inverse
    | UOP_HCos1 -- inverse
    | UOP_HTan1 -- inverse
    | UOP_1x   -- 1 / x
    | UOP_x2
    | UOP_Sqrt
    | UOP_Neg -- negate
    | UOP_x3
    | UOP_Fact
    | UOP_3Root
    -- special unary operators that is not accessible on calculator directly
    | UOP_DegToRad
    | UOP_RadToGrad
    | UOP_GradToDeg

type AngleUnit
    = AU_Rad
    | AU_Deg
    | AU_Grad

initialExpression : Expression
initialExpression = [ ]

getLastLexem : Expression -> Maybe (Expression, Lexem)
getLastLexem e =
    let
        loop expr head =
            case expr of
                []         -> Nothing
                last :: [] -> Just (head, last)
                x    :: xs -> loop xs (head ++ [x])            
    in
        loop e []

-- Nothing in the case of an error
eval : AngleUnit -> Expression -> Maybe Float
eval angleUnit expr =
    let val = evalTree angleUnit (convertToTree expr)
    in if (isNaN val) || (isInfinite val) || ((abs val) >= 10^100)
        then Nothing
        else Just val
                    

type EvalTree
    = ET_Const Float
    | ET_ArOp ArOp EvalTree EvalTree
    | ET_UnOp UnOp EvalTree

convertToTree : Expression -> EvalTree
convertToTree expr =
    let
        opPriority arOp =
            case arOp of
                AOP_Add  -> 2
                AOP_Sub  -> 2
                AOP_Mul  -> 5
                AOP_Div  -> 5
                AOP_Pow  -> 7
                AOP_Root -> 7
                AOP_nCr  -> 9
                AOP_nPr  -> 9
        -- 
        findNextOp leftExpr rightExpr lastPr lastOp lastLeft lastRight =
            case rightExpr of
                []
                    -> (lastOp, lastLeft, lastRight)
                       
                ((L_ArOp op) as l) :: xs
                  -> if (opPriority op) < lastPr
                      then findNextOp (leftExpr ++ [l]) xs (opPriority op) (Just op) leftExpr xs
                      else findNextOp (leftExpr ++ [l]) xs lastPr lastOp lastLeft lastRight

                l :: xs
                    -> findNextOp (leftExpr ++ [l]) xs lastPr lastOp lastLeft lastRight
    in case expr of
         [] -> ET_Const 0
         (L_Const n) :: [] -> ET_Const n
         _  -> case findNextOp [] expr 100 Nothing [] [] of
                   (Nothing, _, _)
                       -> case expr of
                              (L_UnOp unOp) :: xs
                                  -> ET_UnOp unOp (convertToTree xs)
                              _ -- otherwise
                                  -> ET_Const 0 -- TODO: error
                   (Just op, l, r)
                       -> ET_ArOp op (convertToTree l) (convertToTree r)

isWholeNum : Float -> Bool
isWholeNum n = (n - (toFloat (floor n))) == 0
                           
evalFact : Float -> Float
evalFact n =
    if not (isWholeNum n) || n < 0 || n > 69
        then 0 / 0 -- NaN
        else let f x = if x == 0 then 1 else x * f(x-1)
             in toFloat (f (round n))

evalComb : Float -> Float -> Float
evalComb n r = (evalFact n) / ( (evalFact r) * (evalFact (n-r)) )

evalPerm : Float -> Float -> Float
evalPerm n r = (evalFact n) / (evalFact (n-r))

cosh : Float -> Float
cosh x = (e^x + e^ -x) / 2
sinh : Float -> Float
sinh x = (e^x - e^ -x) / 2
tanh : Float -> Float
tanh x = sinh x / cosh x
asinh : Float -> Float
asinh x = logBase e (x + sqrt (1 + x^2))
acosh : Float -> Float
acosh x = logBase e (x + sqrt (x^2 - 1))
atanh : Float -> Float
atanh x = (logBase e ((1 + x) / (1 - x))) / 2
               
evalTree : AngleUnit -> EvalTree -> Float
evalTree angleUnit t =
 let gradFunc x = radians ((pi / 200) * x)
                  
     auFunc = case angleUnit of
                  AU_Grad -> gradFunc
                  AU_Deg  -> degrees
                  AU_Rad  -> radians
                             
     radToDeg  x = 180 * x / pi
     radToRad  x = x
     radToGrad x = 200 * x / pi
                   
     auInvFunc = case angleUnit of
                  AU_Grad -> radToGrad
                  AU_Deg  -> radToDeg
                  AU_Rad  -> radToRad
 in case t of
        ET_Const n
            -> n
        ET_ArOp op a b
            -> let
                va = evalTree angleUnit a
                vb = evalTree angleUnit b
               in
                case op of
                 AOP_Add  -> va + vb
                 AOP_Sub  -> va - vb
                 AOP_Mul  -> va * vb
                 AOP_Div  -> va / vb
                 AOP_Pow  -> va ^ vb
                 AOP_Root -> va ^ (1 / vb)
                 AOP_nCr  -> evalComb va vb
                 AOP_nPr  -> evalPerm va vb
                            
        ET_UnOp op x
            -> let vx = evalTree angleUnit x
               in case op of
                   UOP_Log  -> logBase 10 vx
                   UOP_10x  -> 10 ^ vx
                   UOP_Ln   -> logBase e vx
                   UOP_Exp  -> e ^ vx
                   UOP_Sin  -> sin (auFunc vx)
                   UOP_Cos  -> cos (auFunc vx)
                   UOP_Tan  -> tan (auFunc vx)
                   UOP_Sin1 -> auInvFunc (asin vx)
                   UOP_Cos1 -> auInvFunc (acos vx)
                   UOP_Tan1 -> auInvFunc (atan vx)
                   UOP_HSin  -> sinh vx
                   UOP_HCos  -> cosh vx
                   UOP_HTan  -> tanh vx
                   UOP_HSin1 -> asinh vx
                   UOP_HCos1 -> acosh vx
                   UOP_HTan1 -> atanh vx
                   UOP_1x   -> 1 / vx
                   UOP_x2   -> vx * vx
                   UOP_Sqrt -> sqrt vx
                   UOP_Neg  -> (-vx)
                   UOP_x3   -> vx * vx * vx
                   UOP_Fact -> evalFact vx
                   UOP_3Root -> vx ^ (1/3)
                   UOP_DegToRad   -> vx * pi / 180
                   UOP_RadToGrad  -> radToGrad vx 
                   UOP_GradToDeg  -> vx * 180 / 200
