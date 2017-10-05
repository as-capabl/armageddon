{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Strict, StrictData #-}

module
    Content
where

import Control.Monad (forM_, mzero)
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Lens
import qualified Data.Tree as Tree
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextL
import qualified Data.Text.Lazy.Builder as TextL

import qualified Graphics.UI.Gtk.WebKit.DOM.Document as DOM
import qualified Graphics.UI.Gtk.WebKit.DOM.Element as DOM
import qualified Graphics.UI.Gtk.WebKit.DOM.Node as DOM
import qualified Graphics.UI.Gtk.WebKit.DOM.CSSStyleDeclaration as DOM
import qualified Graphics.UI.Gtk.WebKit.DOM.HTMLImageElement as DOM
import qualified Graphics.UI.Gtk.WebKit.DOM.HTMLAnchorElement as DOM
import qualified Web.Hastodon as Hdon

import BasicModel

--
-- Utility
--
data SomeNode = forall a. DOM.NodeClass a => SomeNode a

nestElement ::
    (Monad m, MonadIO m) =>
    Tree.Tree SomeNode -> m ()
nestElement (Tree.Node pr lCh) =
  do
    forM_ lCh $ \trCh@(Tree.Node ch _) ->
      do
        nestElement trCh
        doAppend pr ch
  where
    doAppend (SomeNode x) (SomeNode y) = DOM.appendChild x $ Just y

elemTree x l = Tree.Node (SomeNode x) l

--
-- Manipulation of armageddon specific DOM
--
classStatus = "hdon_status" :: Text.Text
classAvatar = "hdon_avatar" :: Text.Text
classStatusMain = "hdon_status_main" :: Text.Text
classStatusClear = "hdon_status_clear" :: Text.Text
classUsername = "hdon_username" :: Text.Text
classContent = "hdon_content" :: Text.Text
classRPH = "hdon_rph" :: Text.Text
classRPHWaiting = "waiting" :: Text.Text
classRPHLoading = "loading" :: Text.Text

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
        "div.hdon_content { margin-left: 5pt }\n",
        "div.hdon_rph { margin: 6pt 6pt 6pt 6pt; }\n",
        "div.hdon_rph a.waiting {}\n",
        "div.hdon_rph a.loading {}\n"
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
    DOM.setId ch (statusIdToDomId $ Hdon.statusId st)

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

pushRPH ::
    MonadIO m =>
    DOM.Document -> m (Maybe BMText)
pushRPH doc = runMaybeT $
  do
    pr <- MaybeT $ getTimelineParent doc

    tId <- makeUniqueId pr

    ch <- MaybeT $ DOM.createElement doc (Just "div" :: Maybe Text.Text)
    DOM.setClassName ch classRPH
    DOM.setId ch tId

    ach <-
        fmap DOM.castToHTMLAnchorElement $
        MaybeT $
        DOM.createElement doc (Just "a" :: Maybe Text.Text)
    DOM.setHref ach ("about:armageddon" :: Text.Text)
    DOM.setClassName ach classRPHWaiting

    txt <- MaybeT $ DOM.createTextNode doc ("Read more..." :: Text.Text)

    nestElement $ elemTree ch
      [
        elemTree ach
          [
            elemTree txt []
          ]
      ]

    mfc <- lift $ DOM.getFirstChild pr
    DOM.insertBefore pr (Just ch) mfc

    return tId

  where
    -- Any unique id is OK.
    -- Here `makeUniqueId` returns a modification of top element
    makeUniqueId pr =
      do
        mfch <- lift $ DOM.getFirstElementChild pr
        case mfch
          of
            Just fch ->
              do
                fId <- domIdToStatusId <$> DOM.getId fch
                if fId == statusIdInvalid then mzero else return ()
                return $ (Text.pack "rph_") `mappend` Text.pack (show fId)
            Nothing ->
                return $ (Text.pack "rph_blank")

extractRPH ::
    MonadIO m =>
    BMText -> DOM.Document -> m (Maybe RPH)
extractRPH tId doc = runMaybeT $
  do
    rphElem <- MaybeT $ DOM.getElementById doc tId

    nxElem <- lift $ DOM.getNextElementSibling rphElem
    nxId <- traverse DOM.getId nxElem

    pvElem <- lift $ DOM.getPreviousElementSibling rphElem
    pvId <- traverse DOM.getId pvElem

    return $ RPH
      {
        _rphId = tId,
        _rphUpper = domIdToStatusId <$> pvId,
        _rphLower = domIdToStatusId <$> nxId
      }

