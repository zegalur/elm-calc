module EditableNum
    exposing ( EditableNum
             , toFloatNum
             , toStr
             , changeSign
             , addDigit
             , fromFloatNum
             , initialVal
             , removeLastGlyph
             , addPoint
             , editStrForm
             )

import Either exposing (..)
import Round  as R
         
type alias EditableNum =
    { str : String
    , val : Float
    }

initialVal : EditableNum
initialVal = EditableNum "0" 0
    
toFloatNum : EditableNum -> Float
toFloatNum n = n.val

maxLen : String -> Int
maxLen str = if String.startsWith "-" str
              then 11
              else 10

isWholeNum : Float -> Bool
isWholeNum n = (n - (toFloat (floor n))) < 10 ^ (-9)

removeLastZeros : String -> String
removeLastZeros str =
    if not (String.contains "." str)
        then str
        else
            let remNext s =
                    if String.endsWith "0" s
                        then remNext (String.dropRight 1 s)
                        else if String.endsWith "." s
                             then (String.dropRight 1 s)
                             else s
            in remNext str

-- Return either standard floating point representation
-- or (mantissa, exponent)
toStr : EditableNum -> Either String (String, String)
toStr n = --Left (String.fromFloat n.val) 
    if abs n.val < 10^10
      then
          let l10 = min 9 (max 0 (floor (logBase 10 (abs n.val))))
          in if isWholeNum n.val
              then Left (removeLastZeros (String.fromInt (round n.val)))
              else Left (removeLastZeros (R.round (9 - l10) n.val))
      else
          let expVal = ceiling (logBase 10 (abs n.val))
              manVal = (R.round 9 (n.val / (10 ^ (toFloat expVal))))
          in Right (manVal, String.fromInt expVal)

editStrForm : EditableNum -> String
editStrForm n = n.str

changeSign : EditableNum -> EditableNum
changeSign n =
    let
        newVal = -n.val
        newStr = if String.startsWith "-" n.str
                  then String.dropLeft 1 n.str
                  else if n.str == "0"
                        then "0"
                        else "-" ++ n.str
    in (EditableNum newStr newVal)

addDigit : EditableNum -> Int -> EditableNum
addDigit n d =
    let newStr = if n.str == "0"
                  then String.fromInt d
                  else n.str ++ (String.fromInt d)
        newVal = strToFloat newStr
    in if String.length n.str < (maxLen n.str)
        then { str = newStr
             , val = newVal }
        else n

havePoint : EditableNum -> Bool
havePoint n = String.contains "." n.str

strToFloat : String -> Float
strToFloat s = case String.toFloat s of
                     Just val -> val
                     Nothing  -> 0

fromFloatNum : Float -> EditableNum
fromFloatNum n = EditableNum (String.fromFloat n) n

removeLastGlyph : EditableNum -> EditableNum
removeLastGlyph n =
    let newStr  = String.dropRight 1 n.str
        newStr2 = if newVal == 0
                   then "0"
                   else newStr
        newVal  = strToFloat newStr
    in (EditableNum newStr2 newVal)

addPoint : EditableNum -> EditableNum
addPoint n =
    let newStr = if havePoint n
                  then n.str
                  else n.str ++ "."
        newVal = n.val
    in { str = newStr
       , val = newVal }
