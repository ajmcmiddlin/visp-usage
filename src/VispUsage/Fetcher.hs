{-# LANGUAGE OverloadedStrings #-}

module VispUsage.Fetcher
  ( fetch
  ) where

import Prelude hiding (lines, readFile)
import Data.Text
import Data.Text.IO
import Test.WebDriver

data Auth = Auth Text Text deriving Show

data Error = NoAuth deriving Show

mkAuth :: FilePath -> IO (Maybe Auth)
mkAuth fp = do
  fileContents <- readFile fp
  let auth = lines fileContents
  case f auth of
    Just (u, p) -> return . Just $ Auth u p
    _           -> return Nothing
  where
    f (u:p:ss) = Just (u,p)
    f _        = Nothing

authFile :: FilePath
authFile = "/Users/andrew/.visp"

fetchLength :: IO (Either Error Int)
fetchLength = do
  page <- fetch
  case page of
    Right html -> return . Right $ Data.Text.length html
    Left err -> return (Left err)


fetch :: IO (Either Error Text)
fetch = do
  mAuth <- mkAuth authFile
  case mAuth of
    Just auth -> getUsagePage auth >>= return . Right
    _         -> return $ Left NoAuth

getUsagePage :: Auth -> IO Text
getUsagePage (Auth userName password) = runSession defaultConfig $ do
  openPage "https://mybroadbandusage.virginbroadband.com.au/#login"
  userInput <- findElem (ByCSS "input.inputField[type='text']")
  sendKeys userName userInput
  passInput <- findElem (ByCSS "input[type='password']")
  sendKeys password passInput
  loginBtn  <- findElem (ByCSS "button.btn-primary[type='button']")
  click loginBtn
  getSource
