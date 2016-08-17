{-# LANGUAGE OverloadedStrings #-}

module VispUsage.Fetcher
  ( fetch
  ) where

import Prelude hiding (lines, readFile)
import Data.Text
import Data.Text.IO
import Test.WebDriver
import Test.WebDriver.Commands.Wait
import Test.WebDriver.JSON

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
  pages <- fetch
  case pages of
    Right (html,_) -> return . Right $ Data.Text.length html
    Left err -> return (Left err)


fetch :: IO (Either Error (Text, Text))
fetch = do
  mAuth <- mkAuth authFile
  case mAuth of
    Just auth -> getTwoMonthsUsage auth >>= return . Right
    _         -> return $ Left NoAuth

getTwoMonthsUsage :: Auth -> IO (Text, Text)
getTwoMonthsUsage auth = runSession defaultConfig $ do
  login auth
  month1 <- getSource
  loadPreviousMonthsUsage
  month2 <- getSource
  return (month1, month2)

login :: Auth -> WD ()
login (Auth userName password) = do
  openPage "https://mybroadbandusage.virginbroadband.com.au/#login"
  userInput <- findElem (ByCSS "input.inputField[type='text']")
  sendKeys userName userInput
  passInput <- findElem (ByCSS "input[type='password']")
  sendKeys password passInput
  loginBtn <- findElem (ByCSS "button.btn-primary[type='button']")
  click loginBtn
  waitUntil 10 $ findElem (ByCSS "h1.myBroadbandHeader")
  return ()

loadPreviousMonthsUsage :: WD ()
loadPreviousMonthsUsage = do
  select <- findElem (ByTag "select")
  noReturn (executeJS (fmap JSArg [select]) "arguments[0].options[1].selected = true")
  histUsageBtn <- findElem (ByXPath "//button[text()='Get Historical Usage']")
  click histUsageBtn
