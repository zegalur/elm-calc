module CalcTheme exposing (..)

import Css                    exposing (..)
import Html.Styled            exposing (..)

import Button                 exposing (..)

type alias CalcTheme =
    { bodyColor         : Color
    , blackBoxColor     : Color
    , displayBoxColor   : Color
    , displayBoxBorder  : Color
    , bboxModelColor    : Color
    , displayDigitsCol  : Color
    , bodyBorderColor   : Color
                          
    , bcol2ndFunc       : Color
    , bcolAdvOp         : Color
    , bcolMainOp        : Color
    , bcolNumPad        : Color

    , tcol2ndFunc       : Color
    , tcolAdvOp         : Color
    , tcolMainOp        : Color
    , tcolNumPad        : Color
                          
    , secondTextColor   : Color
    }

defaultTheme : CalcTheme
defaultTheme =
    { bodyColor        = rgb 119 125 129
    , blackBoxColor    = hex "1a1a1e"
    , displayBoxColor  = rgb 140 157 146
    , bboxModelColor   = rgb 214 219 222
    , displayBoxBorder = hex "d8dcdf"
    , displayDigitsCol = hex "191416"
    , bodyBorderColor  = hex "2a2a2a"
                         
    , bcol2ndFunc      = hex "a1d55f"
    , bcolAdvOp        = hex "e5e5e2"
    , bcolMainOp       = hex "a3a8a4"
    , bcolNumPad       = hex "2d292a"

    , tcol2ndFunc      = hex "e5f6e0"
    , tcolAdvOp        = hex "363b3c"
    , tcolMainOp       = hex "eff3f2"
    , tcolNumPad       = hex "f6f6fa"

    , secondTextColor  = hex "d0dc86"
    }

bodyStyle : CalcTheme -> Style
bodyStyle theme =
    Css.batch
        [ backgroundColor theme.bodyColor
        , width           (px 400)
        , textAlign       center
        , padding         (px 10)
        , borderRadius    (px 20)
        , paddingBottom   (px 40)

        , borderStyle       solid
        , borderWidth       (px 0)
        , borderBottomWidth (px 35)
        , borderColor       theme.bodyBorderColor
        ]

blackBoxStyle : CalcTheme -> Style
blackBoxStyle theme =
    Css.batch
        [ backgroundColor theme.blackBoxColor
        , width           (px 380)
        , borderRadius    (px 20)
        , marginTop       (px 35)
        , display         inlineBlock
        , paddingBottom   (px 30)
        ]

modelBoxStyle : CalcTheme -> Style
modelBoxStyle theme =
    Css.batch
        [ color           theme.bboxModelColor
        , fontSize        (px 30)
        , marginTop       (px 30)
        , marginBottom    (px 30)
        , display         inlineBlock
        , fontFamilies    ["Arial", "sans-serif"]
        , textShadow4     (px 0) (px 0) (px 2) theme.bboxModelColor
        ]
        
displayBoxStyle : CalcTheme -> Style
displayBoxStyle theme =
    Css.batch
        [ backgroundColor theme.displayBoxColor
        , display         inlineBlock
        , width           (px 320)
        , height          (px 80)
        , borderStyle     solid
        , borderColor     theme.displayBoxBorder
        , borderWidth     (px 10)
        , borderRadius    (px 15)
        , boxShadow5      inset (px 0) (px 10) (px 5) (rgba 0 0 0 0.5)
        , position        relative
        ]

numBoxStyle : CalcTheme
            -> Float
            -> Float
            -> Float
            -> Float
            -> Float
            -> Style
numBoxStyle theme w h t l fsize =
    Css.batch
        [ width           (px w)
        , height          (px h)
        , position        absolute
        , top             (px t)
        , left            (px l)
        , textAlign       right
        , display         tableCell
        , verticalAlign   bottom

        , fontFamilies    ["fantasy"] -- ["Impact", "League Gothic", "sans-serif"]
        , fontSize        (px fsize)
        , textShadow4     (px 1) (px 3) (px 2) (rgba 0 0 0 0.3)
        , color           theme.displayDigitsCol
        ]
        
curNumBoxStyle : CalcTheme -> Style
curNumBoxStyle theme = numBoxStyle theme 240 45 20 20 42
    
expNumBoxStyle : CalcTheme -> Style
expNumBoxStyle theme = numBoxStyle theme 35 25 20 260 20

memStyle : Int -> CalcTheme -> Style
memStyle n theme = numBoxStyle theme 35 15 8 (50 - 19 * (toFloat (4 - n))) 13
                       
flagHypStyle : CalcTheme -> Style
flagHypStyle theme = numBoxStyle theme 35 15 8 65 13
                       
flag2ndStyle : CalcTheme -> Style
flag2ndStyle theme = numBoxStyle theme 35 15 8 90 13

flagParStyle : CalcTheme -> Style
flagParStyle theme = numBoxStyle theme 35 15 8 115 13

flagAngUnitStyle : CalcTheme -> Style
flagAngUnitStyle theme = numBoxStyle theme 35 15 8 150 13

flagStatStyle : CalcTheme -> Style
flagStatStyle theme = numBoxStyle theme 35 15 8 185 13
                     
buttonsColStyle : CalcTheme -> Style
buttonsColStyle theme =
    Css.batch
        [ display         inlineBlock
        , width           (px 55)
        , verticalAlign   top
        , paddingLeft     (px 10)
        , paddingRight    (px 10)
        ]
        
getButtonStyle : CalcTheme -> ButtonClass -> Style
getButtonStyle theme buttonClass =
        case buttonClass of
            BC_2nd    -> genButtonStyle theme.bcol2ndFunc theme.tcol2ndFunc 40   15
            BC_MainOp -> genButtonStyle theme.bcolMainOp  theme.tcolMainOp  50.9 15
            BC_AdvOp  -> genButtonStyle theme.bcolAdvOp   theme.tcolAdvOp   40   15
            BC_NumPad -> genButtonStyle theme.bcolNumPad  theme.tcolNumPad  40   20

genButtonStyle : Color -> Color -> Float -> Float -> Style
genButtonStyle bcol tcol bheight theight =
    Css.batch
        [ width           (px 55)
        , height          (px bheight)
        , backgroundColor bcol
        , color           tcol
        , padding         (px 0)
        , border          (px 0)
        , borderRadius4   (px 10) (px 10) (px 15) (px 15)
        , fontFamilies    ["Arial", "sans-serif"]
        , fontSize        (px theight)
        , fontWeight      bold
        , boxShadow4      (px 0) (px 5) (px 2) (rgba 0 0 0 0.3)
        , textShadow4     (px 0) (px 0) (px 2) (rgba 255 255 255 0.3)
        ]

getSecondTextBoxStyle : CalcTheme -> Style
getSecondTextBoxStyle theme =
    Css.batch
        [ width           (px 55)
        , height          (px 15)
        , color           theme.secondTextColor
        , fontFamilies    ["Arial", "sans-serif"]
        , fontSize        (px 10)
        , paddingBottom   (px 0)
        , marginBottom    (px 0)
        , display         tableCell
        , verticalAlign   bottom
        ]

getCompanyStyle : CalcTheme -> Style
getCompanyStyle theme =
    Css.batch
        [ display         inlineBlock
        , width           (px 300)
        , height          (px 35)
        , color           theme.secondTextColor
        , fontFamilies    ["Arial", "sans-serif"]
        , fontSize        (px 17)
        , paddingBottom   (px 0)
        , marginBottom    (px 0)
        , verticalAlign   center
        , fontWeight      bold
        , textAlign       left
        ]

        
logoImgSrc : String
logoImgSrc = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABQAAAAUCAYAAACNiR0NAAAABmJLR0QAAAAAAAD5Q7t/AAAACXBIWXMAAC4jAAAuIwF4pT92AAAAB3RJTUUH4wIKEgAerNQ69QAAABl0RVh0Q29tbWVudABDcmVhdGVkIHdpdGggR0lNUFeBDhcAAAGySURBVDjLvdS9a1RBFAXwn/EjoOAHfiw2IoJpprEYNM1iECzEQmtTBAIiWAQsFHyFdgM2gp2NoCD+AxbGRkEFI74q8sAmG1AEESMaBCMuxsJRnutGN1lx4PJm7jucOXfOneEfj1XdkmVVHMHWWmo6hjTdC+GaJfIXcaC2voC+CC2hfAQnMBFDWuiGGeiRa6Ssir04iFE0yqrYUlbF2pUSHsI+TOFYJn6Hj2VVtMqq2L6ckhcwjwrPcB1v8AGbsBtflqPwcgypgZ0ZPxVDOo+ihln/U2FZFU3c7CBp1OYzZVVsq7n+pKyKPThca72zOPND4WO8wK5aDNYIr2EM+/E+9+cMjtcwE2VVbICBGFIbR/HwD2U/xXA2ZbjL/8Uc388whjSfVbS7gL/iVTZmEkP43IGZw6dfTIkhzeJWXrZwDi/zJrMxpKEY0hWM415N2R2MxZAWf7vLZVVszod7I4bUyoY9wGlcxbpc8m28xngM6dFfH4eOTe5jJJc8mOM5mjGktyu5KZfyd2PN/ZPdyHoijCFN4m4t1c6tpp/HYTS3DKzGjr4IY0hzaOZmPpXv9/8Z3wAHsnk9/KnSLQAAAABJRU5ErkJggg=="
