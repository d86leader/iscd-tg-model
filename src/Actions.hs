module Actions
(
) where

-- Description: actions you can perform on entities in database


import Control.Monad.Trans.Class (lift)
import Database.Control          (SqlM)
import Database.Types            (User, Object, Rights)


-- | Actions should make sence for both users and objects
data Entity = EUser User | EObject Object


-- checkRights :: Entity -> Entity -> SqlM [Rights]
-- checkRights (EUser u1) (EUser u2) = do
