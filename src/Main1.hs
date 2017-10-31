{-# LANGUAGE Arrows          #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}


-- base
import Control.Concurrent -- (threadDelay)
import Data.Either (rights)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

import Control.Monad.Trans.Class
import Control.Monad.IO.Class

import Control.Monad (void)

-- dunai
import Control.Monad.Trans.MSF.Maybe (runMaybeT, MaybeT, exit)

import System.Random

import Text.Printf
import qualified Data.Map as Map
import Data.Map (Map)

import React.Flux

-- rhine
import FRP.Rhine
import FRP.Rhine.SyncSF.Except
import FRP.Rhine.Clock.Realtime.Millisecond
import FRP.Rhine.Clock.Realtime.Stdin
import FRP.Rhine.Clock.Select
import FRP.Rhine.Schedule.Concurrently
import FRP.Rhine.Schedule.Trans
import FRP.Rhine.ResamplingBuffer.KeepLast
import FRP.Rhine.ResamplingBuffer.Collect


type StockName = String

data Stock =
  Stock {
  stock :: StockName
  , value :: Double
  } deriving (Show)


data StockExchange = StockExchange {
  stocks :: [Stock]
  , balance :: Double
  , portfolio :: Map StockName Int
  }

data Action = Action

instance StoreData StockExchange where
  type StoreAction StockExchange = Action

  transform Action state = do
    return $ state


stockLenses :: Stock
stockLenses = Stock "lenses" 2.7

stockRB :: Stock
stockRB = Stock "reactive-banana" 3.2

stockMedModule :: Stock
stockMedModule = Stock "med-module" 2.1

stockReactHS :: Stock
stockReactHS = Stock "react-hs" 2.4

initialStore :: StockExchange
initialStore = StockExchange {
  stocks = [stockLenses, stockRB, stockMedModule, stockReactHS]
  , balance = 10.0
  , portfolio = Map.empty
  }




dispatch :: Action -> [SomeStoreAction]
dispatch a = [action @StockExchange a]

sDispatch :: Action -> ([SomeStoreAction], [EventModification])
sDispatch = simpleHandler . dispatch

webApp_ :: ReactElementM_ [SomeStoreAction] ()
webApp_ =
  p_ [] "hi"

  
webApp :: View '[]
webApp = mkView "webApp" webApp_

main :: IO ()
main = do
  registerInitialStore initialStore
  reactRenderView "app" webApp


{-

data Stock =
  Stock {
  stock :: String
  , value :: Double
  } deriving (Show)

type StockClock n m = HoistClock IO m (Millisecond n)

monadIOClock :: (MonadIO m) => cl -> HoistClock IO m cl
monadIOClock cl = HoistClock cl liftIO


myClock0 :: (MonadIO m) => StockClock 1200 m
myClock0 = monadIOClock waitClock

myClock1 :: (MonadIO m) => StockClock 1000 m
myClock1 = monadIOClock waitClock

myClock2 :: (MonadIO m) => StockClock 1600 m
myClock2 = monadIOClock waitClock

displayStock :: Stock -> IO ()
displayStock v = print v

modifyStock :: Integer -> Stock -> SyncSF (ExceptT () IO) (StockClock n m) () ()
modifyStock i Stock{..} = proc () -> do
  v <- arrMSync (\x -> fmap (+x) (liftIO (randomRIO (-1, 1)))) -< value 
  _ <- arrMSync (\v -> lift (displayStock (Stock stock v))) -< v
  returnA -< ()

syncExcept :: Integer -> Stock -> SyncExcept IO (StockClock n m) () () Empty
syncExcept i stock = do
  try (modifyStock i stock)
  syncExcept i stock

  

conc :: (TimeDomainOf cl1 ~ TimeDomainOf cl2, Clock IO cl1, Clock IO cl2) => Schedule IO (HoistClock IO IO cl1) (HoistClock IO IO cl2)
conc = hoistClockSchedule liftIO concurrently

conc2 :: (TimeDomainOf cl1 ~ TimeDomainOf cl2, MonadIO m, Clock IO cl1, Clock IO cl2) => Schedule m cl1 cl2
conc2 = hoistSchedule liftIO concurrently

main :: IO ()
main = do
  let a = safely (syncExcept 0 stockLenses) @@ (myClock0 :: StockClock 1200 IO)
      b = safely (syncExcept 1 stockRB) @@ (myClock1 :: StockClock 1000 IO)
      c = safely (syncExcept 2 stockMedModule) @@ (myClock2 :: StockClock 1600 IO)

     
  flow (a **@ conc @** b)

    -- Unfortunatly, this does not work:
    -- flow ((a **@ conc @** b) **@ conc @** c)

    -- Here is a simple type error which I do not know how to fix!
    -- flow input

    -- Ideally this should work:
    -- flow (((a **@ conc @** b) **@ conc @** c) **@ conc @** input)



-}
