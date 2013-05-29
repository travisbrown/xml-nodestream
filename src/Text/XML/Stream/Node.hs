module Text.XML.Stream.Node
  ( buildElement
  , buildElementCursor
  , buildNode
  , many_
  ) where
import Control.Applicative ((<$>))
import Control.Arrow (second)
import Control.Exception (throw)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Resource (MonadThrow, monadThrow)
import Data.Conduit (Consumer)
import qualified Data.Conduit.List as CL
import qualified Data.Map.Lazy as M
import qualified Data.Text as T
import Data.XML.Types (Content (..), Event (..))
import Text.XML (Element (..), Name, Node (..))
import Text.XML.Cursor (Cursor, fromNode)
import Text.XML.Stream.Parse (XmlException (InvalidEntity), many)
import Text.XML.Unresolved (InvalidEventStream (MissingEndElement))

-- | A version of 'many' that runs a computation for its side effects until it
-- hits a 'Nothing'.
many_ :: Monad m => m (Maybe a) -> m ()
many_ c = c >>= maybe (return ()) (const $ many_ c)

-- | Grabs events until it has a complete XML node.
buildNode :: MonadThrow m => Consumer Event m (Maybe Node)
buildNode = do
  x <- CL.peek
  case x of
    Just (EventBeginElement n as) -> Just <$> goE n (toAttrMap as)
    Just (EventInstruction i) -> ok $ NodeInstruction i
    Just (EventComment t) -> ok $ NodeComment t
    Just (EventCDATA t) -> ok $ NodeContent t
    Just (EventContent (ContentText t)) -> ok $ NodeContent t
    Just (EventContent (ContentEntity e)) -> lift . monadThrow $ InvalidEntity e
    _ -> return Nothing
  where
    goE n as = do
      CL.drop 1
      ns <- many buildNode
      end <- CL.head
      if end == Just (EventEndElement n)
        then return . NodeElement . Element n as $ compressNodes ns
        else lift . monadThrow $ MissingEndElement n ((,) Nothing <$> end)

    ok x = CL.drop 1 >> return (Just x)

-- | A convenience function that ignores non-element nodes.
buildElement :: MonadThrow m => Consumer Event m (Maybe Element)
buildElement = do
  n <- buildNode
  case n of
    Just (NodeElement e) -> return $ Just e
    Just _ -> buildElement
    _ -> return Nothing

-- | A convenience function that returns cursors for element nodes.
buildElementCursor :: MonadThrow m => Consumer Event m (Maybe Cursor)
buildElementCursor = do
  n <- buildNode
  case n of
    Just e @ (NodeElement _) -> return . Just $ fromNode e
    Just _ -> buildElementCursor
    _ -> return Nothing

-- | You may recognize this fellow from 'Text.XML.Unresolved'.
compressNodes :: [Node] -> [Node]
compressNodes [] = []
compressNodes [x] = [x]
compressNodes (NodeContent x : NodeContent y : z) =
  compressNodes $ NodeContent (x `T.append` y) : z
compressNodes (x : xs) = x : compressNodes xs

-- | Utility function for converting the 'Data.XML.Types' representation of
-- element attributes to the representation expected by 'Text.XML'.
toAttrMap :: [(Name, [Content])] -> M.Map Name T.Text
toAttrMap = M.fromList . map (second $ T.concat . map toText)
  where
    toText :: Content -> T.Text
    toText (ContentText t) = t
    toText (ContentEntity e) = throw $ InvalidEntity e

