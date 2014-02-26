module ReWire.TransformTypes where

import ReWire.Core

-- A transformation command takes an argument (this can include spaces, so 
-- multiple arguments are possible but it's up to the command function to
-- parse them out) and a program, and returns maybe a transformed program and
-- maybe a message for the user.
type TransCommand = String -> RWCProg -> (Maybe RWCProg,Maybe String)
