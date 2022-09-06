module Nats where

data Nat = Zero
         | Succ Nat

-- Implementar o Show
-- Representar 3, por exemplo como SSSO

instance (Show Nat) where
    show Zero = "O"
    show (Succ m) = "S" ++ show m