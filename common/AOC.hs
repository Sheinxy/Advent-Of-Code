{-# LANGUAGE OverloadedStrings #-}
module AOC (submit) where

import qualified Data.ByteString.Char8     as B8
import           Network.HTTP.Client       (RequestBody (RequestBodyBS))
import           Network.HTTP.Simple
import           Network.HTTP.Types.Header (hContentType, hCookie)
import           System.Environment        (getEnv)
import           Text.Regex.TDFA

submit :: Int -> Int -> Int -> String -> IO ()
submit year day level answer = do
        sessionCookie <- getEnv "AOC_SESSION"
        request' <- parseRequest $ "POST https://adventofcode.com/" ++ show year ++ "/day/" ++ show day ++ "/answer"
        let formData = "level=" ++ show level ++ "&answer=" ++ answer
            request = setRequestHeader hContentType ["application/x-www-form-urlencoded"]
                    $ setRequestBody (RequestBodyBS $ B8.pack formData)
                    $ setRequestHeader hCookie [B8.pack $ "session=" ++ sessionCookie] request'
        res <- httpBS request
        let body  = getResponseBody res
            regex = "<p>(.+)</p>" :: String
            [[_, msg]] = B8.unpack body =~ regex :: [[String]]
        putStrLn ("Sent: " ++ answer)
        putStrLn ("     "  ++ msg)
