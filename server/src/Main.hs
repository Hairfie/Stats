{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main (main) where

import           Control.Applicative    (Applicative, pure, (<$>), (<*>))
import           Control.Monad.Reader
import           Data.Aeson             (ToJSON, toJSON, object, (.=))
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.Lazy         as TL
import           Data.Time.Format       (parseTimeM, defaultTimeLocale)
import           Data.Time.Clock
import           Data.Maybe             (isJust, fromMaybe)
import           Data.Pool
import qualified Database.MongoDB       as Mongo
import           System.Environment
import           Network.Wai.Middleware.RequestLogger
import           Web.Scotty.Trans

instance Parsable UTCTime where parseParam = parseTimeM True defaultTimeLocale "%FT%T%QZ" . TL.unpack

data Status = Status
    { statusStarted :: UTCTime
    , statusUptime  :: Double
    }

instance ToJSON Status where
    toJSON s = object ["started" .= statusStarted s
                      ,"uptime" .= statusUptime s
                      ]

data Stats = Stats
    { statsNumHairfies       :: Int
    , statsNumHairfieLikes   :: Int
    , statsNumReviews        :: Int
    , statsNumReviewRequests :: Int
    }

instance ToJSON Stats where
    toJSON s = object ["numHairfies"       .= statsNumHairfies s
                      ,"numHairfieLikes"   .= statsNumHairfieLikes s
                      ,"numReviews"        .= statsNumReviews s
                      ,"numReviewRequests" .= statsNumReviewRequests s
                      ]

data AppState = AppState
    { config    :: AppConfig
    , startTime :: UTCTime
    , mongoPool :: Pool Mongo.Pipe
    }

newtype WebM a = WebM { unWebM :: ReaderT AppState IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppState)

runWebM :: AppState -> WebM a -> IO a
runWebM as w = runReaderT (unWebM w) as

webM :: MonadTrans t => WebM a -> t WebM a
webM = lift


data AppConfig = AppConfig
    { port      :: Int
    , mongoHost :: Text
    , mongoDb   :: Text
    , mongoUser :: Maybe Text
    , mongoPass :: Maybe Text
    } deriving (Show, Eq)

getAppConfig :: IO AppConfig
getAppConfig = do
    env <- getEnvironment
    return AppConfig
        { port = maybe 3000 read $ lookup "PORT" env
        , mongoHost = maybe "127.0.0.1" T.pack $ lookup "MONGO_HOST" env
        , mongoDb   = maybe "hairfie" T.pack $ lookup "MONGO_DB" env
        , mongoUser = T.pack <$> lookup "MONGO_USER" env
        , mongoPass = T.pack <$> lookup "MONGO_PASS" env
        }

getAppState :: AppConfig -> IO AppState
getAppState cfg =
    AppState cfg <$> getCurrentTime
                 <*> createPool newMongoPipe Mongo.close 1 300 5
  where
    newMongoPipe = Mongo.connect (Mongo.readHostPort (T.unpack (mongoHost cfg)))


main :: IO ()
main = do
    config <- getAppConfig
    state  <- getAppState config
    scottyT (port config) (runWebM state) (runWebM state) app

getStatus :: WebM Status
getStatus = do
    st <- asks startTime
    nt <- liftIO getCurrentTime
    let dt = diffUTCTime nt st
    return $ Status st (realToFrac dt)

mongo :: Mongo.Action IO a -> WebM a
mongo action = do
    pool <- asks mongoPool
    user <- asks (mongoUser . config)
    pass <- asks (mongoPass . config)
    db   <- asks (mongoDb . config)
    liftIO $ withResource pool $ \pipe -> Mongo.access pipe Mongo.slaveOk db $ do
        -- authenticate action when necessary
        -- NOTE: the auth should be moved into the pipe creator of a Pool
        when (isJust user) $ void $ Mongo.auth (fromMaybe "" user) (fromMaybe "" pass)
        action

maybeParam :: (Parsable a, ScottyError e, Monad m) => TL.Text -> ActionT e m (Maybe a)
maybeParam n = do
    val <- lookup n `liftM` params
    case val of
        Nothing -> return Nothing
        Just v  ->
            case parseParam v of
                Left e  -> next
                Right p -> return $ Just p

app :: ScottyT TL.Text WebM ()
app = do
    middleware logStdoutDev
    get "/" $ do
        st <- webM getStatus
        json st
    get "/businesses/:businessId" $ do
        businessId <- param "businessId"
        since      <- maybeParam "since"
        until      <- maybeParam "until"
        bs         <- webM $ getBusinessStats businessId since until
        json bs

getBusinessStats :: Text -> Maybe UTCTime -> Maybe UTCTime -> WebM Stats
getBusinessStats businessId since until =
    mongo $ Stats <$> numHairfies
                  <*> numHairfieLikes
                  <*> numReviews
                  <*> numReviewRequests
  where
    crit =
        let
            date = ("$gte" Mongo.=? since) `Mongo.merge` ("$lte" Mongo.=? until)
        in
            ["businessId" Mongo.=: T.unpack businessId]
                `Mongo.merge` if null date then []
                                           else ["createdAt" Mongo.=: date]

    numHairfies = Mongo.count $ Mongo.select crit "hairfies"
    numHairfieLikes = Mongo.count $ Mongo.select crit "hairfieLikes"
    numReviews = Mongo.count $ Mongo.select crit "businessReviews"
    numReviewRequests = Mongo.count $ Mongo.select crit "businessReviewRequests"
