{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as E
import Data.Maybe
import Books
import Marc


main :: IO ()
main = do
    marcData <- B.readFile "sample.mrc"
    let processed = processRecords 20 marcData
    TIO.writeFile "books.html" processed

marcToPairs :: B.ByteString -> [(Maybe Title, Maybe Author)]
marcToPairs marcStream = zip titles authors where
    records = getRecords marcStream
    titles = map lookupTitle records
    authors = map lookupAuthor records

pairsToBooks :: [(Maybe Title, Maybe Author)] -> [Book]
pairsToBooks pairs = map (\(title, author) -> Book {
    title = fromJust title,
    author = fromJust author
}) justPairs where
    justPairs = filter(\(title, author) -> isJust title && isJust author) pairs

processRecords :: Int -> B.ByteString -> Html
processRecords n = booksToHtml . pairsToBooks . take n . marcToPairs