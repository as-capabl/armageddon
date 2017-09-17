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
import qualified Graphics.UI.Gtk.WebKit.DOM.HTMLImageElement as DOM
import qualified Web.Hastodon as Hdon

classStatus = "hdon_status" :: Text.Text
classAvatar = "hdon_avatar" :: Text.Text
classStatusMain = "hdon_status_main" :: Text.Text
classStatusClear = "hdon_status_clear" :: Text.Text
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
        "div.hdon_status_clear { clear: both; }\n",
        "div.hdon_avatar { float: left; width: 40pt; }\n",
        "div.hdon_avatar img { width: 40pt; height: 40pt; }",
        "div.hdon_status_main { float: left; width: calc(100% - 40pt); }\n",
        "div.hdon_username { font-weight: bold; }\n",
        "div.hdon_content { margin-left: 5pt }\n"
      ]
    styleStatus = mconcat
      [
        "margin: 1pt 1pt 1pt 1pt;",
        "padding: 5pt 5pt 5pt 5pt;",
        "border: solid thin lightgray"
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

    avatarEl <- domifyAvatar
    DOM.appendChild ch $ Just avatarEl

    mainEl <- MaybeT $ DOM.createElement doc (Just "div" :: Maybe Text.Text)
    DOM.setClassName mainEl classStatusMain

    nameEl <- domifyUsername
    DOM.appendChild mainEl $ Just nameEl

    contEl <- domifyContent
    DOM.appendChild mainEl $ Just contEl

    DOM.appendChild ch $ Just mainEl

    clearEl <- MaybeT $ DOM.createElement doc (Just "div" :: Maybe Text.Text)
    DOM.setClassName clearEl classStatusClear
    DOM.appendChild ch $ Just clearEl

    return ch
  where
    account = Hdon.statusAccount st

    domifyAvatar =
      do
        ch <- MaybeT $ DOM.createElement doc (Just "img" :: Maybe Text.Text)
        let imgCh = DOM.castToHTMLImageElement ch

        let avatar = Hdon.accountAvatar account
        DOM.setSrc imgCh avatar

        chDiv <- MaybeT $ DOM.createElement doc (Just "div" :: Maybe Text.Text)
        DOM.setClassName chDiv classAvatar
        DOM.appendChild chDiv $ Just ch

        return chDiv

    domifyUsername =
      do
        ch <- MaybeT $ DOM.createElement doc (Just "div" :: Maybe Text.Text)
        DOM.setClassName ch classUsername

        let name = Hdon.accountUsername account
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
