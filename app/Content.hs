{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Strict, StrictData #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

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
import qualified Data.Text.Read as Text
import qualified Data.Text.Lazy.Builder as TextL
import Data.Maybe (catMaybes, fromMaybe)
import Data.Char (chr)

import qualified Graphics.UI.Gtk.WebKit.DOM.Document as DOM
import qualified Graphics.UI.Gtk.WebKit.DOM.Element as DOM
import qualified Graphics.UI.Gtk.WebKit.DOM.Node as DOM
import qualified Graphics.UI.Gtk.WebKit.DOM.CSSStyleDeclaration as DOM
import qualified Graphics.UI.Gtk.WebKit.DOM.HTMLImageElement as DOM
import qualified Graphics.UI.Gtk.WebKit.DOM.HTMLAnchorElement as DOM
import qualified Web.Hastodon as Hdon

import BasicModel
import qualified ClassyDOM as Tmpl

instance Tmpl.Template "hdon_status"
  where
    type Structure "hdon_status" =
        'Tmpl.NodeT "div" "hdon_status"
          '[
            'Tmpl.NodeT "a" "name" '[]
           ]


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
classStInfo = "hdon_stinfo" :: Text.Text
classFavBox = "hdon_favbox" :: Text.Text
classRebBox = "hdon_rebbox" :: Text.Text
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
        "div.hdon_status_clear { float:none !important; clear: both; }\n",
        "div.hdon_avatar { float: left; width: 40pt }\n",
        "div.hdon_avatar img { width: 40pt; height: 40pt }",
        "div.hdon_status_main { float: left; width: calc(100% - 40pt); }\n",
        "div.hdon_username a { margin-left: 1pt; font-weight: bold; text-decoration: none; color: black; }\n",
        "div.hdon_username a:hover { color: #303030; }\n",
        "div.hdon_content { margin-left: 8pt }\n",
        "div.hdon_stinfo { margin-left: 0pt; margin-top: 4pt; font-size: 70%; }\n",
        "div.hdon_stinfo div { float: left; }\n",
        "div.hdon_create_at { margin-left: 8pt; color: #505050; }\n",
        "div.hdon_favbox {", favImage, infoBoxItem, "color: gray; ", "}\n",
        "div.hdon_rebbox {", rebImage ,infoBoxItem, "color: gray; ", "}\n",
        "div.hdon_favbox:hover {border-color: darkgray}\n",
        "div.hdon_rebbox:hover {border-color: darkgray}\n",
        "div.hdon_notification { ", styleListItem, " }\n",
        "div.hdon_rph { margin: 12pt 6pt; }\n",
        "div.hdon_rph a.waiting {}\n",
        "div.hdon_rph a.loading {}\n",
        "div.quotation {margin-left: 20pt}"
      ]
    styleListItem = mconcat
      [
        "margin: 1pt 1pt 1pt 1pt;",
        "padding: 5pt 5pt 2pt 5pt;",
        "border: solid thin lightgray"
      ]
    infoBoxItem = mconcat
      [
        "margin-left: 4pt;",
        "padding-left: 4pt;",
        "padding-right: 12pt;",
        "position: relative;",
        "bottom: 2pt;",
        "border: solid thin lightgray;",
        "-webkit-background-size: auto 1.3ex;",
        "background-position: 90% center;",
        "background-repeat: no-repeat;"
      ]
    favImage = "background-image: url(data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAABmJLR0QA/wD/AP+gvaeTAAAACXBIWXMAAAsTAAALEwEAmpwYAAAAB3RJTUUH4QobEwM2JpNxywAAAB1pVFh0Q29tbWVudAAAAAAAQ3JlYXRlZCB3aXRoIEdJTVBkLmUHAAAA/0lEQVRYw8WXTQrDIBCF9VXomXqQntn7dOdC7CYJIlHnrx0hBAITv3njPDW2zzt4DhzvpnlKrtLYC0A8Sq4mCgQvEHhmb6LA8/XwAeiz10DAsqUkJYGn/GKAu0ylMKYlkJQBnvKHEEI89oJm0dOCJCKsM+IqmMaPpxIzoJLrEnYWP/MNzMgsS9JDjWBYyTNC7LLnKLLtgh6Co8Zd3Aoa1FajQoy13imGf/W7yoioSoySU1RLVFlXtdVszUliIOeEFmcCaFxsNTkVCBz5Z+tAY1rQutrOL3ZwsHC1EYRTBnBW/+6nkn3kOg9QTjeclU6MjbCy1J90gWRiLkT0vp5/AUc8uxJf0oslAAAAAElFTkSuQmCC);"
    rebImage = "background-image: url(data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAABmJLR0QA/wD/AP+gvaeTAAAACXBIWXMAAAsTAAALEwEAmpwYAAAAB3RJTUUH4QobEBcph3MVMgAAAB1pVFh0Q29tbWVudAAAAAAAQ3JlYXRlZCB3aXRoIEdJTVBkLmUHAAAAf0lEQVRYw+3XwQ3AIAiF4QdhWWfw3Bk6rj2Z2KamN/5DcQG+PNCgjTFEnpCk8xCiaF3mgk9MSWbRNXE8ASdn4JYAhfBdbxAAgfCvKU25huvDgLegAAUoQAEKUIDfAYLYhJEEdrtGagveEOkz8ERE1i64Fm5dNms5NYQTZPT3/AJMjyV5V5bxWwAAAABJRU5ErkJggg==);"

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

    infoEl <- domifyStInfo

    clearEl <- MaybeT $ DOM.createElement doc (Just "div" :: Maybe Text.Text)
    DOM.setClassName clearEl classStatusClear

    nestElement $
        elemTree ch [
            elemTree avatarEl [],
            elemTree mainEl [
                elemTree nameEl [],
                elemTree contEl [],
                elemTree infoEl []
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

    domifyStInfo =
      do
        ch <- MaybeT $ DOM.createElement doc (Just "div" :: Maybe Text.Text)
        DOM.setClassName ch classStInfo

        dateDiv <- textDiv classCreateAt $ Hdon.statusCreatedAt st
        favDiv <- textDiv classFavBox $ Text.unpack $ nonzeroText $ Hdon.statusFavouritesCount st
        rebDiv <- textDiv classRebBox $ Text.unpack $ nonzeroText $ Hdon.statusReblogsCount st

        clearEl <- MaybeT $ DOM.createElement doc (Just "div" :: Maybe Text.Text)
        DOM.setClassName clearEl classStatusClear

        nestElement $ elemTree ch $
          [
            elemTree favDiv [],
            elemTree rebDiv [],
            elemTree dateDiv [],
            elemTree clearEl []
          ]

        return ch

    textDiv cls str =
      do
        divEl <- MaybeT $ DOM.createElement doc (Just "div" :: Maybe Text.Text)
        DOM.setClassName divEl cls

        txt <- MaybeT $ DOM.createTextNode doc str

        DOM.appendChild divEl $ Just txt

        return divEl

    nonzeroText i
        | i == 0 = Text.pack [chr 0x00a0] -- nbsp
        | otherwise = Text.pack $ show i

replaceStatus ::
    MonadIO m =>
    DOM.Document -> Hdon.Status -> m Bool
replaceStatus doc st = fmap (fromMaybe False) $ runMaybeT $
  do
    liftIO $ print st
    let domId = statusIdToDomId $ Hdon.statusId st
    oldElem <- MaybeT $ DOM.getElementById doc domId

    parent <- MaybeT $ DOM.getParentNode oldElem
    newElem <- MaybeT $ domifyStatus doc st
    MaybeT $ DOM.replaceChild parent (Just newElem) (Just oldElem)
    liftIO $ putStrLn "replaced!"

    return True



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
        _rphUpper = pvId >>= domIdToStatusId >>= return . read, -- stab
        _rphLower = nxId >>= domIdToStatusId >>= return . read -- stab
      }

