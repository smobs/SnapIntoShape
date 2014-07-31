{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Data.ByteString (ByteString)
import           Data.ByteString.Char8 (readInt)
import qualified Data.Text as T
import           Database.MongoDB.Connection
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Snaplet.MongoDB
import           Snap.Util.FileServe
import           Heist
import           Control.Monad.IO.Class
import qualified Text.XmlHtml              as X
import qualified Heist.Interpreted as I
------------------------------------------------------------------------------
import           Application
import           RaceCountdown

------------------------------------------------------------------------------
-- | Render login form
handleLogin :: Maybe T.Text -> Handler App (AuthManager App) ()
handleLogin authError = heistLocal (I.bindSplices errs) $ render "login"
  where
    errs = maybe noSplices splice authError
    splice err = "loginError" ## I.textSplice err


------------------------------------------------------------------------------
-- | Handle login submit
handleLoginSubmit :: Handler App (AuthManager App) ()
handleLoginSubmit =
    loginUser "login" "password" Nothing
              (\_ -> handleLogin err) (redirect "/")
  where
    err = Just "Unknown user or password"


------------------------------------------------------------------------------
-- | Logs out and redirects the user to the site index.
handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> redirect "/"


------------------------------------------------------------------------------
-- | Handle new user form submit
handleNewUser :: Handler App (AuthManager App) ()
handleNewUser = method GET handleForm <|> method POST handleFormSubmit
  where
    handleForm = render "new_user"
    handleFormSubmit = registerUser "login" "password" >> redirect "/"

------------------------------------------------------------------------------

handleCountdown :: Handler App App ()
handleCountdown  = do 
  run <- unsafeWithDB findCurrentRun
  method GET (showPage run)  <|> method POST (handleAddRun)
  where showPage run = renderWithSplices "countdown" $ countdownSplices run
                  
handleAddRun :: Handler App App ()             
handleAddRun = do
  w <- f <$>  (getPostParam "week")
  r <- f <$>  (getPostParam "run")
  let wr = (,) <$> w <*> r 
  case wr of
    Nothing -> return ()
    Just wr' -> unsafeWithDB $ addNewRun wr'
  redirect "/countdown"

f :: Maybe ByteString -> Maybe Int 
f b = fst <$> (b >>= readInt)

countdownSplices :: Run -> Splices (SnapletISplice App)
countdownSplices currentRun  = do
  "remainingDays" ## daysTilRaceSplice
  "remainingRuns" ## remainingRunsSplice
  "slackDays" ## slackSplice
   where
         remainingRunsSplice = ioSplice (return $ runsRemaining currentRun)
         daysTilRaceSplice = ioSplice daysTilRace
         slackSplice = ioSplice $ numberOfSlackDays currentRun

ioSplice :: Show a => IO a -> SnapletISplice App
ioSplice io = do
  i <- liftIO io
  return [X.TextNode (T.pack . show $ i)]  
------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/login",    with auth handleLoginSubmit)
         , ("/logout",   with auth handleLogout)
         , ("/new_user", with auth handleNewUser)
         , ("/countdown",  handleCountdown)
         , ("",          serveDirectory "static")
         ]


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)
    m <- nestSnaplet "mongo" mongo $ mongoDBInit 10 (host "127.0.0.1") "HealthTrackerDB"
    -- NOTE: We're using initJsonFileAuthManager here because it's easy and
    -- doesn't require any kind of database server to run.  In practice,
    -- you'll probably want to change this to a more robust auth backend.
    a <- nestSnaplet "auth" auth $
           initJsonFileAuthManager defAuthSettings sess "users.json"
    addRoutes routes
    addAuthSplices h auth
    return $ App h s a m

