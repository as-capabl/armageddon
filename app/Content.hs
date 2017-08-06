{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict, StrictData #-}

module
    Content
where

import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextL
import qualified Data.Text.Lazy.Builder as TextL

import qualified Graphics.UI.Gtk.WebKit.DOM.Document as DOM
import qualified Graphics.UI.Gtk.WebKit.DOM.Element as DOM
import qualified Graphics.UI.Gtk.WebKit.DOM.Node as DOM
import qualified Graphics.UI.Gtk.WebKit.DOM.CSSStyleDeclaration as DOM
import qualified Web.Hastodon as Hdon

classStatus = "hdon_status" :: Text.Text
classUsername = "hdon_username" :: Text.Text
classContent = "hdon_content" :: Text.Text


initialHtml :: Text.Text
initialHtml = TextL.toStrict $ TextL.toLazyText html
  where
    html = mconcat
      [
        "<html>",
        "<head>", head, "</head>\n",
        "<body>", "<div id=\"timeline\"></div>", "</body>\n",
        "</html>"
      ]
    head = mconcat
      [
        "<style type=\"text/css\">\n", style, "</style>\n"
      ]
    style = mconcat
      [
        "p { margin: 0pt 0pt 0pt 0pt; padding: 0pt 0pt 0pt 0pt; }\n",
        "div.hdon_status { ", styleStatus, " }\n",
        "div.hdon_username { font-weight: bold; }\n",
        "div.hdon_content { margin-left: 5pt }\n"
      ]
    styleStatus = mconcat
      [
        "margin: 2pt 2pt 2pt 2pt;",
        "padding: 10pt 10pt 10pt 10pt;",
        "border: solid thin blue"
      ]

getTimelineParent ::
    MonadIO m =>
    DOM.Document -> m (Maybe DOM.Element)
getTimelineParent doc = DOM.getElementById doc ("timeline" :: Text.Text)

domifyStatus ::
    MonadIO m =>
    DOM.Document -> Hdon.Status -> m (Maybe DOM.Element)
domifyStatus doc st = runMaybeT $
  do
    ch <- MaybeT $ DOM.createElement doc (Just "div" :: Maybe Text.Text)
    DOM.setClassName ch classStatus

    nameEl <- domifyUsername
    DOM.appendChild ch $ Just nameEl

    contEl <- domifyContent
    DOM.appendChild ch $ Just contEl

    return ch
  where
    domifyUsername =
      do
        ch <- MaybeT $ DOM.createElement doc (Just "div" :: Maybe Text.Text)
        DOM.setClassName ch classUsername

        let name = Hdon.accountUsername (Hdon.statusAccount st)
        txt <- MaybeT $ DOM.createTextNode doc name
        DOM.appendChild ch $ Just txt

        return ch

    domifyContent =
      do
        ch <- MaybeT $ DOM.createElement doc (Just "div" :: Maybe Text.Text)
        DOM.setClassName ch classContent

        let content = Hdon.statusContent st
        -- txt <- MaybeT $ DOM.createTextNode doc content
        -- DOM.appendChild ch $ Just txt
        DOM.setInnerHTML ch $ Just content

        return ch
