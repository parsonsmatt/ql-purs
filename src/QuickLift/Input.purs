module QuickLift.Input where

import Form.Types
import QuickLift.Model
import Types

data Input a
    = Goto Routes a
    | GetUser Int a
    | LoadSessions a
    | NewSession (FormInput Session) a
    | Register (FormInput UserReg) a
    | Authenticate (FormInput UserAuth) a
    | UserLogout a
