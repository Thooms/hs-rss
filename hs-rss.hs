import Control.Applicative
import System.IO
import Network.Curl.Download
import Text.Feed.Types hiding ( RSSItem )
import Text.RSS.Syntax

-- Simple HTML5 generator

doctype = "<!DOCTYPE HTML>"

stylesheet = "style.css"

--

data Html = Html Head Body

data Head = Head Title Link

type Title = String

data Link = Link Rel TypeLink Url

data Body = Body [Block]

data Block = 
  HTitle Size String
  | Paragraph [Inline]
  | BDiv Div    
    
data Inline = 
  Raw String
  | InlineLink Url String
  | IDiv Div

data Div = Div Class [Block]

type Size = Int

type Url = String

type Rel = String

type TypeLink = String

type Class = String

nullHTML = 
  Html 
  (Head "Foo" (Link "bar" "text/css" stylesheet)) 
  (Body [HTitle 1 "Hello, World!"])
  
errorParagraph = Paragraph [Raw "Sorry, this feed couldn't have been loaded :("]

class (Gen a) where
  gen :: a -> String
  
instance Gen Html where
  gen (Html h b) = doctype ++ "\n<html>\n" ++ gen h ++ gen b ++ "</html>\n"
    
instance Gen Head where
  gen (Head t (Link rel typ url)) = 
    "<head>\n" ++ 
    "<title>" ++ t ++ "</title>\n" ++
    "<link rel=\"" ++ rel ++ "\" type=\"" ++ typ ++ "\" href=\"" ++ url ++ "\">\n" ++
    "</head>\n"
    
instance Gen Body where
  gen (Body blocks) = "<body>\n" ++ concatMap gen blocks ++ "</body>\n"
  
instance Gen Block where
  gen (HTitle size s) = "<h" ++ siz ++ "> " ++ s ++ "</h" ++ siz ++ ">\n"
    where siz = show size
  gen (Paragraph l) = "<p>" ++ concatMap gen l ++ "</p>\n"    
  gen (BDiv d) = gen d

instance Gen Inline where
  gen (Raw s) = s
  gen (InlineLink url s) = "<a href=\"" ++ url ++ "\">" ++ s ++ "</a>"
  gen (IDiv d) = gen d
  
instance Gen Div where
  gen (Div cl l) = "<div class=\"" ++ cl ++ "\">" ++ concatMap gen l ++ "</div>\n" 

-- Feeds to HTML
-- Completely arbitrary and ugly.
  
  
strFromMaybe Nothing = ""
strFromMaybe (Just str) = str
  
convertFeed :: Either String Feed -> Block
convertFeed (Left e) = errorParagraph
convertFeed (Right (RSSFeed (RSS version attr chan _))) = 
  BDiv $ Div "feed" (convertFeedContent chan)

convertFeedContent :: RSSChannel -> [Block] 
convertFeedContent (RSSChannel title link desc items lang copyright editor webmaster 
                    pubDate lastUpdate categories generator docs cloud ttl image rating 
                    textInput skipHours skipDays _) =
  [
    HTitle 1 $ "RSS Channel : " ++ title
  ] ++ map convertItem items 
  
convertItem :: RSSItem -> Block
convertItem (RSSItem title link desc author categories comments enclosure guid pubDate
            source attr other) =
  BDiv . Div "item" $ 
  [
    HTitle 2 $ "Item : " ++ strFromMaybe title
  , Paragraph [ InlineLink (strFromMaybe link) "[ LINK ]" ]
  , Paragraph [Raw . strFromMaybe $ desc]
  , Paragraph [Raw . strFromMaybe $ author]
  ]
    

build :: [Block] -> Html
build blocks = Html 
          (Head "RSS Reader." (Link "stylesheet" "text/css" stylesheet))
          (Body blocks)

-- Main feeds handling

feedsFile = "feeds"

outputFile = "output.html"

fetch url = do
  feed <- openAsFeed url
  putStrLn $ case feed of
    Left err -> err
    Right _ -> "RSS flux located at " ++ url ++ " correctly loaded."
  return feed

main :: IO ()
main = do
  feedsUrls <- lines <$> readFile feedsFile
  feeds <- mapM fetch feedsUrls
  writeFile outputFile . gen . build . map convertFeed $ feeds
  return ()