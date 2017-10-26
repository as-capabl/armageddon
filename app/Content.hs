{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Strict, StrictData #-}

module
    Content
where

import Control.Monad (forM_, mzero, join)
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Lens
import qualified Data.Tree as Tree
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextL
import qualified Data.Text.Lazy.Builder as TextL
import Data.Maybe (catMaybes)

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
classCreateAt = "hdon_create_at" :: Text.Text
classContent = "hdon_content" :: Text.Text
classNotification = "hdon_notification" :: Text.Text
classRPH = "hdon_rph" :: Text.Text
classQuotation = "quotation" :: Text.Text
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
        "div.hdon_status { ", styleListItem, " }\n",
        "div.hdon_status_clear { clear: both; }\n",
        "div.hdon_avatar { float: left; width: 40pt }\n",
        "div.hdon_avatar img { width: 40pt; height: 40pt }",
        "div.hdon_status_main { float: left; width: calc(100% - 40pt); }\n",
        "div.hdon_username a { margin-left: 1pt; font-weight: bold; text-decoration: none; color: black; }\n",
        "div.hdon_username a:hover { color: #303030; }\n",
        "div.hdon_content { margin-left: 8pt }\n",
        "div.hdon_create_at { margin-left: 8pt; font-size: 75%; color: #505050; }\n",
        "div.hdon_notification { ", styleListItem, " }\n",
        "div.hdon_rph { margin: 12pt 6pt; }\n",
        "div.hdon_rph a.waiting {}\n",
        "div.hdon_rph a.loading {}\n",
        "div.quotation {margin-left: 20pt}"
      ]
    styleListItem = mconcat
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

    mainEl <- MaybeT $ DOM.createElement doc (Just "div" :: Maybe Text.Text)
    DOM.setClassName mainEl classStatusMain

    nameEl <- domifyUsername

    contEl <- domifyContent

    dateEl <- domifyCreateAt

    clearEl <- MaybeT $ DOM.createElement doc (Just "div" :: Maybe Text.Text)
    DOM.setClassName clearEl classStatusClear

    nestElement $
        elemTree ch [
            elemTree avatarEl [],
            elemTree mainEl [
                elemTree nameEl [],
                elemTree dateEl [],
                elemTree contEl []
              ],
            elemTree clearEl []
          ]

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

        ach <- MaybeT $ DOM.createElement doc (Just "a" :: Maybe Text.Text)
        let aach = DOM.castToHTMLAnchorElement ach
        DOM.setHref aach $ Text.pack $ "about://armageddon"

        let name = Hdon.accountUsername account
        txt <- MaybeT $ DOM.createTextNode doc name

        nestElement $ elemTree ch
          [
            elemTree ach
              [
                elemTree txt []
              ]
          ]

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

    domifyCreateAt =
      do
        ch <- MaybeT $ DOM.createElement doc (Just "div" :: Maybe Text.Text)
        DOM.setClassName ch classCreateAt

        let str = Hdon.statusCreatedAt st
        txt <- MaybeT $ DOM.createTextNode doc str
        DOM.appendChild ch $ Just txt

        return ch

domifyNotification ::
    MonadIO m =>
    DOM.Document -> Hdon.Notification -> m (Maybe DOM.Element)
domifyNotification doc ntf = runMaybeT $
  do
    ch <- MaybeT $ DOM.createElement doc (Just "div" :: Maybe Text.Text)
    DOM.setId ch $ notificationIdToDomId $ Hdon.notificationId ntf
    DOM.setClassName ch classNotification

    divType <- MaybeT $ DOM.createElement doc (Just "div" :: Maybe Text.Text)
    txtType <- MaybeT $ DOM.createTextNode doc $ Hdon.notificationType ntf

    let account = Hdon.notificationAccount ntf
    divAccount <- MaybeT $ DOM.createElement doc (Just "div" :: Maybe Text.Text)
    DOM.setClassName divAccount classUsername

    txtAccount <- MaybeT $ DOM.createTextNode doc $ Hdon.accountUsername account

    -- Make quotation part if notificationStatus is Just ...
    mDivQuot <- liftIO $ runMaybeT $
      do
        st <- MaybeT $ return $ Hdon.notificationStatus ntf
        stDom <- MaybeT $ domifyStatus doc st

        divQuot <- MaybeT $ DOM.createElement doc (Just "div" :: Maybe Text.Text)
        DOM.setClassName divQuot classQuotation

        nestElement $ elemTree divQuot
          [
            elemTree stDom []
          ]

        return divQuot
    -- notificationId :: Int,
    -- notificationType :: String,
    -- notificationCreatedAt :: String,
    -- notificationAccount :: Account,
    -- notificationStatus :: Maybe Status

    let subtree = catMaybes [
            Just $ elemTree divType [elemTree txtType []],
            Just $ elemTree divAccount [elemTree txtAccount []],
            elemTree <$> mDivQuot <*> pure []
          ]
    nestElement $ elemTree ch subtree

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
    DOM.setClassName ach classRPHWaiting
    DOM.setHref ach ("about:armageddon" :: Text.Text)

    txt <- MaybeT $ DOM.createTextNode doc ("Read more..." :: Text.Text)

    nestElement $ elemTree ch
      [
        elemTree ach [elemTree txt []]
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
                fId <- MaybeT $ domIdToStatusId <$> DOM.getId fch
                return $ (Text.pack "rph_") `mappend` Text.pack (show fId)
            Nothing ->
                return $ (Text.pack "rph_blank")

extractRPH ::
    MonadIO m =>
    DOM.Document -> BMText -> m (Maybe RPH)
extractRPH doc tId = runMaybeT $
  do
    rphElem <- MaybeT $ DOM.getElementById doc tId

    nxElem <- lift $ DOM.getNextElementSibling rphElem
    nxId <- traverse DOM.getId nxElem

    pvElem <- lift $ DOM.getPreviousElementSibling rphElem
    pvId <- traverse DOM.getId pvElem

    return $ RPH
      {
        _rphId = tId,
        _rphUpper = join $ domIdToStatusId <$> pvId,
        _rphLower = join $ domIdToStatusId <$> nxId
      }

