{- stack script
    --resolver lts-17.9
    --install-ghc
    --ghc-options -Wall
    --package blaze-html
    --package shakespeare
    --package time
    --package yaml
-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Time.Clock
import Data.Time.Format
import Data.Yaml (FromJSON (..))
import qualified Data.Yaml as Y
import GHC.Generics (Generic)
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Pretty
import Text.Hamlet

data SectionHeader = SectionHeader
  { title :: String
  , subtitle :: String
  , extraHeader :: Maybe String
  , extraSubheader :: Maybe String
  }
  deriving (Show, Generic)

newtype SectionBody
  = SectionBody [String]
  deriving (Show, Generic)

data SectionItem = SectionItem
  { header :: Maybe SectionHeader
  , body :: Maybe SectionBody
  }
  deriving (Show, Generic)

data Section = Section
  { name :: String
  , children :: [SectionItem]
  }
  deriving (Show, Generic)

data Resume = Resume
  { name :: String
  , contact :: [String]
  , sections :: [Section]
  }
  deriving (Show, Generic)

instance FromJSON SectionHeader
instance FromJSON SectionBody
instance FromJSON SectionItem
instance FromJSON Section
instance FromJSON Resume

sectionTemplate :: Section -> Html
sectionTemplate (Section{..}) =
  [shamlet|
<section>
  <h2>#{name}
  $forall (SectionItem header body) <- children
    <div .section-object>
      $maybe SectionHeader title subtitle extraHeader extraSubheader <- header
        <div .section-header>
          <div .flex>
            <h3 .section-title>#{preEscapedToHtml $ title}
            $maybe extraHeader' <- extraHeader
              <p .section-extra-header>#{extraHeader'}
          
          <div .flex>
            <p .section-subtitle>#{subtitle}
            $maybe extraSubheader' <- extraSubheader
              <p .section-extra-subheader>#{extraSubheader'}
          
      $maybe SectionBody body' <- body
        <ul .section-body>
          $forall b <- body'
            <li .section-body-list>#{preEscapedToHtml b}
|]

template :: String -> Resume -> String -> Html
template css (Resume{..}) d =
  [shamlet|
$doctype 5
<html>
  <head>
    <title>Cheah Jer Liang
    <meta charset="utf-8">
    <style>#{preEscapedToHtml css}

  <body>
    <div .wrapper>
      <div .top>
        <h1>#{name}
        <ul .contact-details>
          $forall c <- contact
            <li>#{preEscapedToHtml c}
          <li>#{d}
      $forall section <- sections
        #{preEscapedToHtml $ sectionTemplate section}
|]

inputFile :: String
inputFile = "input.yaml"

cssFile :: String
cssFile = "style.css"

dateFormat :: String
dateFormat = "%d %B %Y"

date :: IO String
date = getCurrentTime >>= return . formatTime defaultTimeLocale dateFormat

main :: IO ()
main = do
  css <- readFile cssFile
  config <- Y.decodeFileThrow inputFile
  d <- date

  putStrLn $ renderHtml $ template css config d
