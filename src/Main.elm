module Main exposing (..)

-- TI-30Xa calculator emulator
    
import Browser
import Css                    exposing (..)
import Html                   
import Html.Styled            exposing (..)
import Html.Styled.Attributes exposing (css, href, src)
import Html.Styled.Events     exposing (onClick)

import Round                  as R
import Debug                  as D
import Either                 exposing (..)

import Expression             exposing (..)
import Button                 exposing (..)
import CalcTheme              exposing (..)
import EditableNum            as EN
import StatData               as SD

main : Program Flags Model Msg
main = Browser.document
         { init          = init
         , view          = view 
         , update        = update
         , subscriptions = subscriptions }

       
type alias Flags = ()
       
type alias Model =
    { state      : State
    , memory     : Memory
    , on2ndFunc  : Bool
    , angleUnit  : AngleUnit
    , onHypFunc  : Bool
    , statData   : SD.StatData
    , storedNums : (Float, Float, Float) -- M1, M2, M3
    , onMemFunc  : Bool -- waiting for memory function argument
    , lastMemOp  : MemOp
    }
    
type alias Memory =
    { expression  : Expression  --
    , exprStack   : List Expression
    , curNum      : EN.EditableNum -- current number on display
    , lastArOp    : Maybe (ArOp, Float) -- last operation
    }
        
type State
    = State_Off
    | State_WaitOp
    | State_EditNum
    | State_Error
    
type Msg
    = NoOp
    | OnButton ButtonOperation ButtonOperation
      

-- All buttons (by columns)
buttons : List (List Button)
buttons =
  [ [ Button "2nd"   ""      BC_2nd    BO_2nd              BO_2nd
    , Button "HYP"   "K"     BC_AdvOp  BO_Hyp              BO_NoOp
    , Button "Pi"    "x<>y"  BC_AdvOp  BO_InsPi            BO_NoOp
    , Button "S+"    "S-"    BC_AdvOp  BO_AddDataP         BO_RemDataP
    , Button "STO"   "EXC"   BC_AdvOp  (BO_Mem MOP_Store)  (BO_Mem MOP_Exc)
    , Button "RCL"   "SUM"   BC_AdvOp  (BO_Mem MOP_Recall) (BO_Mem MOP_Sum)
    , Button "a b/c" "d/c"   BC_AdvOp  BO_NoOp             BO_NoOp
    , Button "<-"    "F<>D"  BC_AdvOp  BO_BS               BO_NoOp
    ]
  , [ Button "DRG"   "DRG>"  BC_AdvOp  BO_DRG          BO_DRG2
    , Button "SIN"   "SIN1"  BC_AdvOp  (BO_Un UOP_Sin) (BO_Un UOP_Sin1)
    , Button "1/x"   "FRQ"   BC_AdvOp  (BO_Un UOP_1x)  BO_NoOp
    , Button "EE"    "n"     BC_AdvOp  BO_NoOp         (BO_Stat SOP_Num)
    , Button "7"     "CSR"   BC_NumPad (BO_Num 7)      BO_ClearStat
    , Button "4"     "FLO"   BC_NumPad (BO_Num 4)      BO_NoOp
    , Button "1"     "x3"    BC_NumPad (BO_Num 1)      (BO_Un UOP_x3)
    , Button "0"     "3sX"   BC_NumPad (BO_Num 0)      (BO_Un UOP_3Root)
    ]
  , [ Button "LOG"   "10x"   BC_AdvOp  (BO_Un UOP_Log) (BO_Un UOP_10x)
    , Button "COS"   "COS1"  BC_AdvOp  (BO_Un UOP_Cos) (BO_Un UOP_Cos1)
    , Button "x2"    "x_"    BC_AdvOp  (BO_Un UOP_x2)  (BO_Stat SOP_Mean)
    , Button "("     "Sx"    BC_AdvOp  BO_OpenPar      (BO_Stat SOP_Sum)
    , Button "8"     "nCr"   BC_NumPad (BO_Num 8)      (BO_Arc AOP_nCr)
    , Button "5"     "SCI"   BC_NumPad (BO_Num 5)      BO_NoOp
    , Button "2"     "%"     BC_NumPad (BO_Num 2)      BO_NoOp
    , Button "."     "FIX"   BC_NumPad BO_AddP         BO_NoOp
    ]
  , [ Button "LN"    "eX"    BC_AdvOp  (BO_Un UOP_Ln)   (BO_Un UOP_Exp)
    , Button "TAN"   "TAN1"  BC_AdvOp  (BO_Un UOP_Tan)  (BO_Un UOP_Tan1)
    , Button "sqrt"  "Sxn1"  BC_AdvOp  (BO_Un UOP_Sqrt) (BO_Stat SOP_SamD)
    , Button ")"     "Sx2"   BC_AdvOp  BO_ClosePar      (BO_Stat SOP_Sum2)
    , Button "9"     "nPr"   BC_NumPad (BO_Num 9)       (BO_Arc AOP_nPr)
    , Button "6"     "ENG"   BC_NumPad (BO_Num 6)       BO_NoOp
    , Button "3"     "x!"    BC_NumPad (BO_Num 3)       (BO_Un UOP_Fact)
    , Button "+/-"   ""      BC_NumPad BO_Sign          BO_NoOp
    ]
  , [ Button "OFF"  ""       BC_AdvOp  BO_Off           BO_Off
    , Button "yX"   "xSy"    BC_AdvOp  (BO_Arc AOP_Pow) (BO_Arc AOP_Root)
    , Button "/"    "Sxn"    BC_MainOp (BO_Arc AOP_Div) (BO_Stat SOP_PopD)
    , Button "x"    "P>R"    BC_MainOp (BO_Arc AOP_Mul) BO_NoOp
    , Button "-"    "R>P"    BC_MainOp (BO_Arc AOP_Sub) BO_NoOp
    , Button "+"    "DMS>DD" BC_MainOp (BO_Arc AOP_Add) BO_NoOp
    , Button "="    "DD>DMS" BC_MainOp BO_Eval          BO_NoOp
    ]
  ]
 
    
initialMemory : Memory
initialMemory =
    { expression  = initialExpression
    , curNum      = EN.initialVal
    , lastArOp    = Nothing
    , exprStack   = []
    }

initialModel : Model
initialModel =
    { state      = State_Off
    , memory     = initialMemory
    , on2ndFunc  = False
    , onHypFunc  = False
    , angleUnit  = AU_Deg
    , statData   = SD.initialStatData
    , storedNums = (0, 0, 0)
    , onMemFunc  = False
    , lastMemOp  = MOP_Store
    }
    
init : Flags -> ( Model, Cmd Msg )
init _ = ( initialModel, Cmd.none)
        
subscriptions : Model -> Sub Msg
subscriptions model = Sub.none
        

-- UPDATE --                    

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = ( updFunc msg model, Cmd.none )

updFunc : Msg -> Model -> Model
updFunc msg model =
 let
     flip2ndFuncVal = if model.state == State_Off
                        then False
                        else not model.on2ndFunc
 in case msg of
        NoOp
            -> model
               
        OnButton BO_2nd BO_2nd
            -> { model | on2ndFunc = flip2ndFuncVal }
               
        OnButton firstOp secondOp
            -> let
                firstOp1  = if model.onHypFunc then changeToHyp firstOp  else firstOp
                secondOp1 = if model.onHypFunc then changeToHyp secondOp else secondOp
                newModel  =
                    if model.onMemFunc && firstOp1 /= BO_Off
                      then execMemOp firstOp1 model
                      else
                        if model.on2ndFunc 
                          then updByButtonOp secondOp1 msg model
                          else updByButtonOp firstOp1  msg model
               in { newModel | on2ndFunc = False }

               
updByButtonOp : ButtonOperation -> Msg -> Model -> Model
updByButtonOp op msg model =
    case op of
        BO_NoOp          -> model
        BO_OnC           -> { model | state  = State_WaitOp, memory = initialMemory }
        BO_Off           -> initialModel
        BO_2nd           -> model
        BO_Num digit     -> addDigit model digit
        BO_Arc operation -> addArOp model operation
        BO_Eval          -> evalExpr model
        BO_Sign          -> changeSign model
        BO_BS            -> backspace model
        BO_AddP          -> addPoint model
        BO_Un unOp       -> applyUnOp model unOp
        BO_InsPi         -> insertNumber model pi
        BO_OpenPar       -> openPar model
        BO_ClosePar      -> closePar model
        BO_DRG           -> changeAUnit1 model
        BO_DRG2          -> changeAUnit2 model
        BO_Hyp           -> hypFuncMode model
        BO_ClearStat     -> clearStatData model
        BO_AddDataP      -> addDataPoint model
        BO_RemDataP      -> removeDataPoint model
        BO_Stat statOp   -> applyStatOp statOp model
        BO_Mem memOp     -> waitMemOpArg memOp model


waitMemOpArg : MemOp -> Model -> Model
waitMemOpArg memOp model =
    case model.state of
        State_Off   -> model
        State_Error -> model
        _           -> { model | onMemFunc = True, lastMemOp = memOp }
                       

hypFuncMode : Model -> Model
hypFuncMode model =
    case model.state of
        State_Off   -> model
        State_Error -> model
        _           -> { model | onHypFunc = not model.onHypFunc }
               

last : List a -> Maybe a
last list =
    case list of
        []      -> Nothing
        x :: [] -> Just x
        _ :: xs -> last xs

getExprVal : AngleUnit -> Expression -> (Bool, Float)
getExprVal au expr =
    case eval au expr of
       Nothing -> (True, 0)
       Just v  -> (False, v)
               
addArOp : Model -> ArOp -> Model
addArOp model op =
    let
        angleUnit     = model.angleUnit
        oldMemory     = model.memory
        oldExpr       = oldMemory.expression
        oldDisplayNum = oldMemory.curNum
        arLexem       = L_ArOp op
        newState      = if isError then State_Error else State_WaitOp
        newModel      = { model | memory = newMemory, state = newState }
                        
        newMemory = { oldMemory | curNum = newNum
                                , expression = newExpr
                                , lastArOp = Nothing }
                    
        (isError, newExpr, newNum)
            = case getLastLexem oldExpr of
                  Just (expr, lastLexem) ->
                      case lastLexem of
                          -- if last lexem was arithmetic operation
                          -- then we substitude it with new operation
                          L_ArOp _ -> (False, expr ++ [arLexem], oldDisplayNum)

                          -- otherwise we just add opeartion lexem
                          _ -> let (err, val) = getExprVal angleUnit oldExpr
                               in (err, oldExpr ++ [arLexem], EN.fromFloatNum val)
                  Nothing ->
                      (False, [L_Const (EN.toFloatNum oldDisplayNum), arLexem], oldDisplayNum)
                    
    in case model.state of
        State_Off     -> model
        State_Error   -> model
        State_WaitOp  -> newModel
        State_EditNum -> newModel


applyUnOp : Model -> UnOp -> Model
applyUnOp model op =
    let
        angleUnit     = model.angleUnit
        oldMemory     = model.memory
        oldExpr       = oldMemory.expression
        oldDisplayNum = oldMemory.curNum
        unLexem       = L_UnOp op
        curNumLexem   = L_Const (EN.toFloatNum oldDisplayNum)
        newState      = if isErr then State_Error else State_WaitOp
        newModel      = { model | memory = newMemory, state = newState }
                        
        newMemory = { oldMemory | curNum     = newNum
                                , expression = newExpr
                                , lastArOp   = Nothing }
                    
        (isErr, newExpr, newNum)
            = case getLastLexem oldExpr of
                  Just (expr, lastLexem) ->
                      case lastLexem of
                          -- if last lexem is arithmetic operation
                          -- then apply unary operator to current num
                          L_ArOp _
                              -> let exprToEval = [unLexem, curNumLexem]
                                     (err, val) = getExprVal angleUnit exprToEval
                                     newCurNum  = EN.fromFloatNum val
                                 in ( err, oldExpr ++ [ L_Const val ], newCurNum )
                                 
                          -- if last lexem is const lexem then apply unary operation
                          -- to this value and put the result as new const lexem
                          (L_Const _) as consLexem
                              -> let (err, val) = getExprVal angleUnit exprToEval
                                     exprToEval = [ unLexem, consLexem ]
                                     resExpr    = expr ++ [ L_Const val ]
                                 in ( err, resExpr, EN.fromFloatNum val )
                                          
                          -- otherwise do nothing
                          _ -> (False, oldExpr, oldDisplayNum)
                  Nothing ->
                      let exprToEval = [unLexem, L_Const (EN.toFloatNum oldDisplayNum)]
                          (err, val) = getExprVal angleUnit exprToEval
                      in ( err, [ L_Const val ], EN.fromFloatNum val )
                    
    in case model.state of
        State_Off     -> model
        State_Error   -> model
        State_WaitOp  -> newModel
        State_EditNum -> newModel


evalExpr : Model -> Model
evalExpr model =
    if List.length model.memory.exprStack > 0
      then evalExpr (closePar model)
      else evalCurExpr model
               

evalCurExpr : Model -> Model
evalCurExpr model =
    let
        angleUnit     = model.angleUnit
        oldMemory     = model.memory
        oldExpr       = oldMemory.expression
        oldDisplayNum = oldMemory.curNum
        dnumLexem     = L_Const (EN.toFloatNum oldDisplayNum)
        (isError, val)= getExprVal angleUnit exprToEval
        newNum        = EN.fromFloatNum val
        newExpr       = [ ]
                        
        newLastArOp2  = if List.length oldMemory.expression > 0
                        then newLastArOp
                        else oldMemory.lastArOp
                            
        newState      = if isError || isError2 then State_Error else State_WaitOp
        newModel      = { model | memory = newMemory, state = newState }
                            
        newMemory = { oldMemory | curNum     = newNum
                                , expression = newExpr
                                , lastArOp   = newLastArOp2 }

        (isError2, newLastArOp)
            = case getLastLexem exprToEval of
                  Nothing -> (False, oldMemory.lastArOp) -- Nothing
                  Just (expr, lastLexem) ->
                      case lastLexem of
                          (L_Const n) as constLexem ->
                              let
                                -- find unary operators chain
                                -- before the first arithmetic operator
                                findArOp l r =
                                  case getLastLexem l of
                                    Nothing -> ( False, oldMemory.lastArOp ) -- Nothing
                                    Just (l2, lastLexem2) ->
                                      case lastLexem2 of
                                        L_ArOp op
                                            -> let (err, v) = getExprVal angleUnit r
                                               in (err, Just (op, v))
                                        (L_UnOp _) as uop
                                            -> findArOp l2 (uop :: r)
                                        _ -- otherwise
                                            -> (False, Nothing)
                              in findArOp expr [constLexem]
                          _ -> (False, Nothing)
              
        exprToEval
            = case getLastLexem oldExpr of
                  Nothing
                      -> -- in this case try to apply last arithmetic operation
                         case model.memory.lastArOp of
                             Nothing      -> [ dnumLexem ]
                             Just (op, n) -> [ dnumLexem, L_ArOp op, L_Const n ] 
                  Just (expr, lastLexem) ->
                      case lastLexem of
                          -- if last lexem was arithmetic operation
                          -- then add current display number as cons lexem
                          L_ArOp AOP_Add -> oldExpr ++ [dnumLexem]
                          L_ArOp AOP_Sub -> oldExpr ++ [dnumLexem]
                          L_ArOp AOP_Mul -> oldExpr ++ [dnumLexem]
                          L_ArOp AOP_Div -> oldExpr ++ [dnumLexem]
                          -- otherwise return original expression
                          _ -> oldExpr
                    
    in case model.state of
        State_Off      -> model
        State_Error    -> model
        State_WaitOp   -> newModel
        State_EditNum  -> newModel


changeCurNum : (EN.EditableNum -> EN.EditableNum) -> Model -> Model
changeCurNum f model =
    let
        oldMemory     = model.memory
        oldExpr       = oldMemory.expression
        newNum        = f oldMemory.curNum
        newMemory = { oldMemory | curNum = newNum
                                , expression = newExpr }
        newExpr
            = case getLastLexem oldExpr of
                  Just (expr, lastLexem) ->
                      case lastLexem of
                          L_Const n
                              -> expr ++ [L_Const (EN.toFloatNum newNum)]
                          _
                              -> oldExpr ++ [L_Const (EN.toFloatNum newNum)]
                  Nothing -> [ ]
                    
    in case model.state of
        State_Off      -> model
        State_Error    -> model
        State_WaitOp   -> { model | memory = newMemory }
        State_EditNum  -> { model | memory = newMemory }                            
                          
               
changeSign : Model -> Model
changeSign model =
    if model.state == State_WaitOp
      then applyUnOp model UOP_Neg
      else changeCurNum EN.changeSign model
    

addPoint : Model -> Model
addPoint model = changeCurNum EN.addPoint model


addDigit : Model -> Int -> Model
addDigit model digit =
    let
        fdigit    = toFloat digit
        oldMemory = model.memory
        oldExpr   = oldMemory.expression
        newMemory = { oldMemory | curNum = newNum
                                , expression = newExpr
                                , lastArOp = Nothing }
        (newExpr, newNum)
            = case getLastLexem oldExpr of
                  Just (expr, lastLexem) ->
                      case lastLexem of
                          L_Const num ->
                              let newNumVal =
                                      if model.state == State_EditNum
                                       then EN.addDigit (oldMemory.curNum) digit
                                       else EN.fromFloatNum fdigit
                              in (expr ++ [L_Const (EN.toFloatNum newNumVal)], newNumVal)
                          _ ->
                              (oldExpr ++ [L_Const fdigit], EN.fromFloatNum fdigit)
                  Nothing -> ([L_Const fdigit], EN.fromFloatNum fdigit)
                                                       
    in case model.state of
        State_Off     -> model
        State_Error   -> model
        State_WaitOp  -> { model | memory = newMemory, state = State_EditNum }
        State_EditNum -> { model | memory = newMemory }


backspace : Model -> Model
backspace model =
    let
        oldMemory     = model.memory
        oldExpr       = oldMemory.expression
        newNum        = EN.removeLastGlyph oldMemory.curNum
        newMemory = { oldMemory | curNum = newNum
                                , expression = newExpr }
        newExpr
            = case getLastLexem oldExpr of
                  Just (expr, lastLexem) ->
                      case lastLexem of
                          L_Const n -> expr ++ [L_Const (EN.toFloatNum newNum)]
                          _         -> oldExpr
                  Nothing -> [ ]
                    
    in case model.state of
        State_Off     -> model
        State_Error   -> model
        State_WaitOp  -> model
        State_EditNum -> { model | memory = newMemory }
               

insertNumber : Model -> Float -> Model
insertNumber model num =
    let
        m1 = addDigit model 0
        chFunc _ = EN.fromFloatNum num
        m2 = changeCurNum chFunc m1
    in case model.state of
        State_Off       -> model
        State_Error     -> model
        State_WaitOp    -> { m2 | state = State_WaitOp }
        State_EditNum   -> { m2 | state = State_WaitOp }
    

openPar : Model -> Model
openPar model =
    let
        oldMemory     = model.memory
        oldExpr       = oldMemory.expression
        newNum        = EN.fromFloatNum newNumVal
        newExpr       = [ L_Const newNumVal ]
        newMemory = { oldMemory | curNum = newNum
                                , expression = newExpr
                                , exprStack = pushExpr :: oldMemory.exprStack }
        (newNumVal, pushExpr)
            = case getLastLexem oldExpr of
                  Just (expr, lastLexem) ->
                      case lastLexem of
                          L_Const n -> (n, expr)
                          _         -> (0, oldExpr)
                  Nothing -> (0, [ ])
                    
    in case model.state of
        State_Off      -> model
        State_Error    -> model
        State_WaitOp   -> { model | memory = newMemory }
        State_EditNum  -> { model | memory = newMemory, state = State_WaitOp }


closePar : Model -> Model
closePar model =
    let
        mod1          = evalCurExpr model
        oldMemory     = model.memory
        oldExpr       = oldMemory.expression
        newNum        = mod1.memory.curNum

        (newExpr, newStack) =
            case mod1.memory.exprStack of
                []      -> ( mod1.memory.expression, [] )
                e :: xs -> ( e ++ mod1.memory.expression, xs ) 
                        
        newMemory = { oldMemory | curNum     = newNum
                                , expression = newExpr
                                , exprStack  = newStack }

        newModel = if List.length model.memory.exprStack > 0
                    then { model | memory = newMemory, state = mod1.state }
                    else model
                    
    in case model.state of
        State_Off     -> model
        State_Error   -> model
        State_WaitOp  -> newModel
        State_EditNum -> newModel


changeAUnit1 : Model -> Model
changeAUnit1 model =
    let newAu = case model.angleUnit of
                    AU_Deg  -> AU_Rad
                    AU_Rad  -> AU_Grad
                    AU_Grad -> AU_Deg  
    in case model.state of
        State_Off     -> model
        State_Error   -> model
        State_WaitOp  -> { model | angleUnit = newAu }
        State_EditNum -> { model | angleUnit = newAu }


changeAUnit2 : Model -> Model
changeAUnit2 model =
    let op = case model.angleUnit of
                 AU_Deg  -> UOP_DegToRad
                 AU_Rad  -> UOP_RadToGrad
                 AU_Grad -> UOP_GradToDeg
    in changeAUnit1 (applyUnOp model op)
               

clearStatData : Model -> Model
clearStatData model =
    let oldMemory   = model.memory
        newStatData = SD.clearStatData model.statData
        newModel    = { model | statData = newStatData, state = newState }
        newState    = if SD.isEmpty model.statData
                       then State_Error
                       else State_WaitOp -- model.state
    in case model.state of
        State_Off     -> model
        State_Error   -> model
        State_WaitOp  -> newModel
        State_EditNum -> newModel

changeDataPoints : (SD.StatData -> SD.StatData) -> Model -> Model
changeDataPoints func model =
    let newStatData = func model.statData
    in case model.state of
        State_Off     -> model
        State_Error   -> model
        State_WaitOp  -> { model | statData = newStatData }
        State_EditNum -> { model | statData = newStatData, state = State_WaitOp }
               
addDataPointN : Float -> Int -> Model -> Model
addDataPointN val count model = changeDataPoints (SD.addDataPoint val count) model

addDataPoint : Model -> Model
addDataPoint model =
    let curNumVal = EN.toFloatNum model.memory.curNum
    in addDataPointN curNumVal 1 model

removeDataPointN : Float -> Int -> Model -> Model
removeDataPointN val count model = changeDataPoints (SD.removeDataPoint val count) model

removeDataPoint : Model -> Model
removeDataPoint model =
    let curNumVal = EN.toFloatNum model.memory.curNum
    in removeDataPointN curNumVal 1 model

applyStatOp : StatOp -> Model -> Model
applyStatOp op model =
    let statData = model.statData
        res = case op of
                SOP_Num  -> Just (toFloat (SD.numOfDataPoint statData))
                SOP_Sum  -> SD.calcSum         statData
                SOP_Sum2 -> SD.calcSum2        statData
                SOP_Mean -> SD.calcMean        statData
                SOP_PopD -> SD.populationStDev statData
                SOP_SamD -> SD.sampleStDev     statData
        newModel = insertNumber model (Maybe.withDefault 0 res) 
        newState = if res == Nothing then State_Error else newModel.state
    in { newModel | state = newState }


execMemOp : ButtonOperation -> Model -> Model
execMemOp op model =
 let newModel = { model | onMemFunc = False }
 in  case op of
        BO_Num d ->
            if d < 1 || d > 3
              then newModel
              else applyMemOp d newModel
        _ -> newModel
             
applyMemOp : Int -> Model -> Model
applyMemOp n model =
    let readMem x nums =
            case (x, nums) of
                (1, (v,_,_)) -> v
                (2, (_,v,_)) -> v
                (3, (_,_,v)) -> v
                _ -> 0
                     
        writeMem x y nums =
            case (x, nums) of
                (1, (_,b,c)) -> (y,b,c)
                (2, (a,_,c)) -> (a,y,c)
                (3, (a,b,_)) -> (a,b,y)
                _            -> (0,0,0)
                                
        curVal      = EN.toFloatNum model.memory.curNum
        curSVal     = readMem n model.storedNums
        writeCurVal = writeMem n curVal model.storedNums
                    
        storeFun    = { model | storedNums = writeCurVal }
        sumFun      = { model | storedNums = writeMem n (curVal + curSVal) model.storedNums }
        recallFun   = insertNumber model curSVal
        exchangeFun = { recallFun | storedNums = writeCurVal }
                      
        newModel = case model.lastMemOp of
                      MOP_Store  -> storeFun 
                      MOP_Sum    -> sumFun
                      MOP_Recall -> recallFun
                      MOP_Exc    -> exchangeFun
        newModel2 = { newModel | state = State_WaitOp, onMemFunc = False }
                    
    in case model.state of
           State_Off     -> model
           State_Error   -> model
           State_WaitOp  -> newModel2
           State_EditNum -> newModel2

-- VIEW --


view : Model -> Browser.Document Msg
view model = { title = "Elm Calculator"
             , body  = viewBody model }

viewBody : Model -> List (Html.Html Msg)
viewBody model = [
    toUnstyled
      (div [ css [bodyStyle defaultTheme] ]
           [ viewBlackBox        model
           , viewLogoAndOnButton model
           , viewButtons         model
           ])
    ]

viewBlackBox : Model -> Html Msg
viewBlackBox model =
    div [ css [blackBoxStyle defaultTheme] ]
        [ div [css [modelBoxStyle defaultTheme]]
              [ text "TI-"
              , span [css [fontFamilies ["Impact", "League Gothic", "sans-serif"]]]
                     [text "30X"]
              , text "a"
              ]
        , viewDisplayBox model
        ]

viewDisplayBox : Model -> Html Msg
viewDisplayBox model =
  if model.state == State_Off
   then
    div [ css [displayBoxStyle defaultTheme] ] [ ]
   else
    div [ css [displayBoxStyle defaultTheme] ]
        [ div [ css [curNumBoxStyle defaultTheme] ]
              [ viewDisplayNumBoxData model ]
        , div [ css [expNumBoxStyle defaultTheme] ]
              [ viewDisplayExpBoxData model ]
        , div [ css [flag2ndStyle defaultTheme] ]
              [ text (if model.on2ndFunc then "2nd" else "") ]
        , div [ css [flagHypStyle defaultTheme] ]
              [ text (if model.onHypFunc then "HYP" else "") ]
        , div [ css [flagParStyle defaultTheme] ]
              [ text (if List.length model.memory.exprStack == 0
                      then ""
                      else "( " ++ (String.fromInt (List.length model.memory.exprStack)) ++ " )")]
        , div [ css [flagAngUnitStyle defaultTheme] ]
              [ text (case model.angleUnit of
                          AU_Deg  -> "DEG"
                          AU_Grad -> "GRAD"
                          AU_Rad  -> "RAD" ) ]
        , div [ css [flagStatStyle defaultTheme] ]
              [ text (if SD.isEmpty model.statData then "" else "STAT") ]
                  
        , viewMemFlag 1 model
        , viewMemFlag 2 model
        , viewMemFlag 3 model
        ]

viewMemFlag : Int -> Model -> Html Msg
viewMemFlag i model =
    let (m1, m2, m3) = model.storedNums
        (mval, mtext) =
            case i of
               1 -> (m1, "M1")
               2 -> (m2, "M2")
               3 -> (m3, "M3")
               _ -> (0, "")
    in div [ css [memStyle i defaultTheme] ]
           [ text (if mval == 0 then "" else mtext) ]

viewDisplayNumBoxData : Model -> Html Msg
viewDisplayNumBoxData model =
    let (digits, _) = getDisplayNumberStr model.memory.curNum
    in case model.state of
        State_Off     -> text ""
        State_Error   -> text "ERROR"
        State_EditNum -> text (EN.editStrForm model.memory.curNum)
        _             -> text digits

viewDisplayExpBoxData : Model -> Html Msg
viewDisplayExpBoxData model =
    let (_, expVal) = getDisplayNumberStr model.memory.curNum
    in case model.state of
        State_Off     -> text ""
        State_Error   -> text ""
        State_EditNum -> text ""
        _             -> text expVal

-- The calculator can display up to 10 digits plus a minus
-- sign and a 2-digit exponent. Results with more than 10 digits
-- display in scientific notation.
-- For a given Float return (digits with point and sign, exponent)
getDisplayNumberStr : EN.EditableNum -> (String, String)
getDisplayNumberStr num
    = case EN.toStr num of
          Left str -> (str, "")
          Right x  -> x
                      

viewLogoAndOnButton : Model -> Html Msg
viewLogoAndOnButton model =
    div [ css [ marginTop (px 10) ] ]
        [ div [ css [ getCompanyStyle defaultTheme ] ]
              [ img [ src logoImgSrc ] []
              , text " Texas Instruments" ]
        , styled button
            [ genButtonStyle defaultTheme.bcolAdvOp defaultTheme.tcolAdvOp 30 15 ]
            [ onClick (OnButton BO_OnC BO_OnC) ]
            [ text "ON/C" ]
        ]


viewButtons : Model -> Html Msg
viewButtons model =
    let
       mapCol col = div [ css [ buttonsColStyle defaultTheme ] ]
                        (List.map viewButton col)
    in div [ css [ ] ] (List.map mapCol buttons)

viewButton : Button -> Html Msg
viewButton bt =
    div [ css [] ]
           [ div [ css [ getSecondTextBoxStyle defaultTheme ] ]
                 [ text bt.secondText ]
           , div [ css [ ] ]
                 [ styled button
                       [ getButtonStyle defaultTheme bt.buttonClass ]
                       [ onClick (OnButton bt.firstOp bt.secondOp) ]
                       [ text bt.firstText ] ]
           ]
