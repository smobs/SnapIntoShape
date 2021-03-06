{-# LANGUAGE TemplateHaskell #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import Control.Lens
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
import Snap.Snaplet.Session
import Snap.Snaplet.MongoDB.Core
------------------------------------------------------------------------------
data App = App
    { _heist :: Snaplet (Heist App)
    , _sess :: Snaplet SessionManager
    , _auth :: Snaplet (AuthManager App)
    , _mongo :: Snaplet MongoDB
    }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist

instance HasMongoDB App where
    getMongoDB app = view snapletValue $ view mongo app
------------------------------------------------------------------------------
type AppHandler = Handler App App


