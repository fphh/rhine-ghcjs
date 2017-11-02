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

import Control.Monad

import Data.Time.Clock (getCurrentTime)

-- dunai
import Control.Monad.Trans.MSF.Maybe (runMaybeT, MaybeT, exit)

import System.Random

import Text.Printf
import qualified Data.Map as Map

import qualified Data.List as List

import Data.Map (Map)

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.JSString as JSS

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
  } deriving (Eq, Show)

   
stockLenses :: Stock
stockLenses = Stock "lenses" 2.7

stockRB :: Stock
stockRB = Stock "reactive-banana" 3.2

stockMedModule :: Stock
stockMedModule = Stock "med-module" 2.1

stockReactHS :: Stock
stockReactHS = Stock "react-hs" 2.4


data StockExchange = StockExchange {
  stocks :: Map StockName [(UTCTime, Double)]
  , balance :: Double
  , portfolio :: Map StockName Int
  , portfolioValue :: [(UTCTime, Double)]
  } deriving (Eq)

data Action =
  Buy StockName
  | Sell StockName
  | Modify UTCTime StockName
  | EvaluatePortfolio UTCTime

numOfPts :: Int
numOfPts = 30

instance StoreData StockExchange where
  type StoreAction StockExchange = Action

  transform (Buy name) StockExchange{..} =
    let (_, price):_ = stocks Map.! name
        newBal = balance - price
        newPort = Map.insertWith (+) name 1 portfolio
        
    in return (StockExchange stocks newBal newPort portfolioValue)
    
  transform (Sell name) StockExchange{..} =
    let price = current (stocks Map.! name)
        newBal = balance + price
        newPort = Map.filter (/=0) $ Map.adjust (+ negate 1) name portfolio
    in return (StockExchange stocks newBal newPort portfolioValue)

  transform (Modify utc name) StockExchange{..} = do
    val <- randomRIO (0.9, 1.1)
    let f xs = (utc, current xs * val) : take (numOfPts-1) xs
        newStocks = Map.adjust f name stocks
    return (StockExchange newStocks balance portfolio portfolioValue)
    
  transform (EvaluatePortfolio utc) StockExchange{..} = do
    let newPValue = (utc, evaluatePortfolio portfolio stocks) : take (numOfPts-1) portfolioValue
    return (StockExchange stocks balance portfolio newPValue)

initialStore :: UTCTime -> StockExchange
initialStore utc = StockExchange {
  stocks = foldl f Map.empty [stockLenses, stockRB, stockMedModule, stockReactHS]
  , balance = 100.0
  , portfolio = Map.empty
  , portfolioValue = [(utc, 0)]
  }
  where f acc (Stock n v) = Map.insert n [(utc, v)] acc


evaluatePortfolio :: Map StockName Int -> Map StockName [(UTCTime, Double)] -> Double
evaluatePortfolio portfolio stocks =
  let evaluate name n acc = acc + current (stocks Map.! name) * fromIntegral n
  in Map.foldWithKey evaluate 0.0 portfolio
  
current :: [(UTCTime, Double)] -> Double
current ((_, v):_) = v

a .= b = (Text.pack a) Aeson..= (Text.pack b)

dispatch :: Action -> [SomeStoreAction]
dispatch a = [action @StockExchange a]

sDispatch :: Action -> ([SomeStoreAction], [EventModification])
sDispatch = simpleHandler . dispatch

minMax :: [(a, Double)] -> (Double, Double)
minMax vs =
  let ys = map snd vs
  in (minimum ys, maximum ys)

scale :: (Double, Double) -> Int -> (Double -> Double)
scale (mi, ma) h =
  let d = fromIntegral h / (if ma-mi ==  0 then 10 else ma - mi)
  in \x -> fromIntegral h - (x-mi)*d

sparkLine :: [(a, Double)] -> ReactElementM_ [SomeStoreAction] ()
sparkLine xs = do
  let w, h :: Int
      w = 120
      h = 20
      sc = scale (minMax xs) h
      rectSty = Aeson.object [ "fill" .= "none", "stroke" .= "black" ]
      d = fromIntegral w / fromIntegral (numOfPts-1)
      f i (_, v) = show (fromIntegral w - fromIntegral i * d) ++ "," ++ show (sc v)

      l = fromIntegral w - (fromIntegral (length xs) * d)
      
      pts = List.intercalate " " $ (show w ++ ",20") : zipWith f [0..] xs ++ [(show l ++ ",20")]
  svg_ [ "version" $= "1.1"
       , "xmlns" $= "http://www.w3.org/2000/svg"
       , "width" @= w
       , "height" @= h
       , "viewBox" $= (JSS.pack $ "0 0 " ++ show w ++ " " ++ show h) ] $ do
    polyline_ [ "points" $= JSS.pack pts, "fill" $= "#dddddd", "stroke" $= "#aa4444" ] mempty
    rect_ [ "x" $= "0", "y" $= "0", "width" @= w, "height" @= h, "style" @= rectSty ] mempty

renderStocks_ :: StockExchange -> ReactElementM_ [SomeStoreAction] ()
renderStocks_ (StockExchange{..}) = do
  let sty = Aeson.object [ "padding" .= "6px 12px" ]

      buyCB name = onClick $ \_ _ -> sDispatch (Buy name)
      sellCB name = onClick $ \_ _ -> sDispatch (Sell name)

      format = elemString . printf "%.2f"
      
      renderStocks :: StockName -> [(UTCTime, Double)] -> ReactElementM_ [SomeStoreAction] ()-> ReactElementM_ [SomeStoreAction] ()
      renderStocks name xs acc = do
        let value = current xs
        acc
        div_ [ "className" $= "row" ] $ do
          div_ [ "className" $= "col-xs-4", "style" @= sty ] (elemString name)
          div_ [ "className" $= "col-xs-4", "style" @= sty ] (sparkLine xs)
          div_ [ "className" $= "col-xs-2", "style" @= sty ]
            $ b_ [ "className" $= "redToBlack" ] (format value)
          if (balance > value)
            then div_ [ "className" $= "col-xs-2 btn btn-link", buyCB name ] "Buy"
            else div_ [ "className" $= "col-xs-2 btn btn-link disabled" ] "Buy"

      renderPortfolio :: StockName -> Int -> ReactElementM_ [SomeStoreAction] () -> ReactElementM_ [SomeStoreAction] ()
      renderPortfolio name n acc = do
        acc
        div_ [ "className" $= "row" ] $ do
          div_ [ "className" $= "col-xs-6", "style" @= sty ] (elemString name)
          div_ [ "className" $= "col-xs-2", "style" @= sty ] (elemShow n)
          div_ [ "className" $= "col-xs-2", "style" @= sty ] (format (snd (head (stocks Map.! name)) * fromIntegral n))
          div_ [ "className" $= "col-xs-2 btn btn-link", sellCB name ] "Sell"

  div_ [ "className" $= "container-fluid" ] $ do
    Map.foldWithKey renderStocks mempty stocks
    
    hr_ []
    div_ [ "className" $= "row" ] $ do
      div_ [ "className" $= "col-xs-8", "style" @= sty ] "Balance:"
      div_ [ "className" $= "col-xs-2", "style" @= sty ] $ b_ [] (format balance)
      div_ [ "className" $= "col-xs-2", "style" @= sty ] mempty

    when (not (Map.null portfolio)) $ do
      hr_ []
      Map.foldWithKey renderPortfolio mempty portfolio
      
      hr_ []
    
      div_ [ "className" $= "row" ] $ do
        div_ [ "className" $= "col-xs-4", "style" @= sty ] "Portfolio value:"
        div_ [ "className" $= "col-xs-4", "style" @= sty ] (sparkLine portfolioValue)
        div_ [ "className" $= "col-xs-2", "style" @= sty ] $
          b_ [] $ format $ (evaluatePortfolio portfolio stocks)
        div_ [ "className" $= "col-xs-2", "style" @= sty ] mempty
 
      hr_ []
      div_ [ "className" $= "row" ] $ do
        div_ [ "className" $= "col-xs-8", "style" @= sty ] "Balance+Portfolio:"
        div_ [ "className" $= "col-xs-2", "style" @= sty ] $ b_ [] (format $ balance + current portfolioValue)
        div_ [ "className" $= "col-xs-2", "style" @= sty ] mempty


webApp :: View '[]
webApp = mkControllerView @'[StoreArg StockExchange] "render-stocks" renderStocks_

modifyStock :: StockName -> SyncSF IO (Millisecond n) () ()
modifyStock name = proc () -> do
  tinfo <- timeInfo -< ()
  time <- arrMSync (return . absolute) -< tinfo
  _ <- arrMSync (\t -> liftIO (executeAction (action @StockExchange (Modify t name)))) -< time
  returnA -< ()

updatePortfolioValue :: SyncSF IO (Millisecond n) () ()
updatePortfolioValue = proc () -> do
  tinfo <- timeInfo -< ()
  time <- arrMSync (return . absolute) -< tinfo
  _ <- arrMSync (\t -> liftIO (executeAction (action @StockExchange (EvaluatePortfolio t)))) -< time
  returnA -< ()                    

main :: IO ()
main = do
  now <- getCurrentTime
  registerInitialStore (initialStore now)
  reactRenderView "app" webApp
  let a = modifyStock "lenses" @@ (waitClock :: Millisecond 1200)
      b = modifyStock "reactive-banana" @@ (waitClock :: Millisecond 2100)
      c = modifyStock "med-module" @@ (waitClock :: Millisecond 1900)
      d = modifyStock "react-hs" @@ (waitClock :: Millisecond 1300)
      p = updatePortfolioValue @@ (waitClock :: Millisecond 600)
  flow ((((a **@ concurrently @** b) **@ concurrently @** c) **@ concurrently @** d) **@ concurrently @** p)

