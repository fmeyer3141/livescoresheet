{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}

module Foundation where

import PackedHandler
import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)

import ClassyPrelude.Yesod (runDB)

import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import Yesod.Auth.Hardcoded
import qualified Yesod.Core.Unsafe as Unsafe
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE
import Yesod.Auth.Message
import qualified Data.Text as T

usernameAdmin :: Text
usernameAdmin = "admin"

passwordAdmin :: IO Text
passwordAdmin = T.init . decodeUtf8 <$> readFile "config/passAdmin" -- Zeilenumbruch entfernen

usernameKari :: Text
usernameKari = "kari"

passwordKari :: IO Text
passwordKari = (T.init . decodeUtf8) <$> readFile "config/passKari" -- Zeilenumbruch entfernen

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings            :: AppSettings
    , appStatic              :: Static -- ^ Settings for static file serving.
    , appConnPool            :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager         :: Manager
    , appLogger              :: Logger
    , appFrontendChannel     :: TChan FrontendMessage
    , appRefereeState        :: IORef RefereeResult
    , packedHandlerLock      :: PackedHandlerLock
    }

instance HasPackedHandlerLock App where
  getPackedHandlerLock = packedHandlerLock

data MenuItem = MenuItem
    { menuItemLabel :: Text
    , menuItemRoute :: Route App
    , menuItemAccessCallback :: Bool
    }

data MenuTypes
    = NavbarLeft MenuItem
    | NavbarRight MenuItem

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

-- | A convenient synonym for database access functions.
type DB a = ∀ (m :: * -> *).
    (MonadIO m, Functor m) => ReaderT SqlBackend m a

authorizedMinimal :: HandlerFor App AuthResult
authorizedMinimal = do
                      muid <- maybeAuthId
                      return $ case muid of
                        Nothing -> AuthenticationRequired
                        Just usern -> if (usern==usernameKari || usern==usernameAdmin)
                                      then Authorized else AuthenticationRequired

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot = ApprootRelative

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend foundation =
      Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        (clientSessionKey $ appSettings foundation)

    -- Yesod Middleware allows you to run code before and after each handler function.
    -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
    -- Some users may also want to add the defaultCsrfMiddleware, which:
    --   a) Sets a cookie with a CSRF token in it.
    --   b) Validates that incoming write requests include that token in either a header or POST parameter.
    -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
    -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
    yesodMiddleware = defaultYesodMiddleware

    defaultLayout widget = do
        master <- getYesod

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR css_bootstrap_css
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- Routes not requiring authentication.
    isAuthorized AdminR _ = do
                              muid <- maybeAuthId
                              return $ case muid of
                                Nothing -> AuthenticationRequired
                                Just usern -> if usern==usernameAdmin then Authorized
                                                                 else AuthenticationRequired
    isAuthorized CSVFormR b = isAuthorized AdminR b
    isAuthorized LifterFormR b = isAuthorized AdminR b
    isAuthorized MeetStateFormR b = isAuthorized AdminR b
    isAuthorized UndoR b = isAuthorized AdminR b
    isAuthorized ResetKariR b = isAuthorized AdminR b

    isAuthorized (JuryR _) _ = authorizedMinimal

    isAuthorized _ _ = return Authorized

    -- isAuthorized ProfileR _ = isAuthenticated


    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLogIO app _source level =
        pure $ appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger

-- Define breadcrumbs.
instance YesodBreadcrumbs App where
  breadcrumb  _ = return ("home", Nothing)

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = Text

    -- Where to send a user after successful login
    loginDest _ = OverviewR
    -- Where to send a user after logout
    logoutDest _ = FrontendR False

    authenticate (Creds credsPlug credsId _) = do
      return
        (case credsPlug of
           "hardcoded" ->
             case (credsId == usernameAdmin || credsId == usernameKari) of
               False  -> UserError InvalidLogin
               True   -> Authenticated credsId
           _ ->  error "Different Authplugin than hardcoded")

    authPlugins _ = [authHardcoded]

    -- authHttpManager = getHttpManager

instance YesodAuthHardcoded App where
  validatePassword u p = liftIO $ if u == "admin" then (== p) <$> passwordAdmin else (== p) <$> passwordKari

  doesUserNameExist    = pure . liftA2 (||) ("kari" ==) ("admin" ==)


instance YesodAuthPersist App where
  type AuthEntity App = Text

  getAuthEntity usern = pure $ if usern==usernameAdmin || usern==usernameKari then Just $ usern else Nothing


-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding
