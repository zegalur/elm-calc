module Button exposing (..)

import Expression exposing (..)

type alias Button =
    { firstText   : String           -- Text on button itself
    , secondText  : String           -- Text above the button (can be empty)
    , buttonClass : ButtonClass
    , firstOp     : ButtonOperation
    , secondOp    : ButtonOperation
    }

type ButtonClass
    = BC_2nd        -- 2nd func button type (green button)
    | BC_AdvOp      -- Advanced function (white buttons)
    | BC_MainOp     -- Main operations (grey buttons)
    | BC_NumPad     -- NumPad buttons (black buttons)

type ButtonOperation
    = BO_NoOp          -- No operation
    | BO_OnC           -- On/C
    | BO_Off           -- Turn Off
    | BO_2nd           -- 2nd func
    | BO_Num Int       -- NumPad digit pressed
    | BO_Arc ArOp      -- Arithmetic operation
    | BO_Eval          -- Evaluate expression
    | BO_Sign          -- Change the sign of the current number
    | BO_BS            -- Back space
    | BO_AddP          -- Add point
    | BO_Un UnOp       -- Apply unary operator (function)
    | BO_InsPi         -- insert Pi
    | BO_OpenPar       --
    | BO_ClosePar      --
    | BO_DRG           -- Change angle-unit but dont convert current number
    | BO_DRG2          -- Change angle-unit and perform convertion
    | BO_Hyp           -- hyperbolic functions mode
    | BO_ClearStat     -- clear stat data
    | BO_AddDataP      -- enters a data point
    | BO_RemDataP      -- remove data point
    | BO_Stat StatOp   --
    | BO_Mem MemOp     -- memory opeartions
      

type StatOp
    = SOP_Num  -- number of data point
    | SOP_Sum  --
    | SOP_Sum2 --
    | SOP_Mean --
    | SOP_PopD -- population standard deviation
    | SOP_SamD -- sample -//-


type MemOp
    = MOP_Store  -- store current displayed number in memory
    | MOP_Recall -- recall value in memory
    | MOP_Sum    -- add displayed number to memory
    | MOP_Exc    -- exchange displayed number and number in memory


changeToHyp : ButtonOperation -> ButtonOperation
changeToHyp op =
  case op of
    BO_Un UOP_Sin  -> BO_Un UOP_HSin
    BO_Un UOP_Cos  -> BO_Un UOP_HCos
    BO_Un UOP_Tan  -> BO_Un UOP_HTan
    BO_Un UOP_Sin1 -> BO_Un UOP_HSin1
    BO_Un UOP_Cos1 -> BO_Un UOP_HCos1
    BO_Un UOP_Tan1 -> BO_Un UOP_HTan1
    _              -> op
