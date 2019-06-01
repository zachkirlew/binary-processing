{-# LANGUAGE OverloadedStrings #-}

module Books where

import qualified Data.Text as T

-- Start off by creating data type for books
type Author = T.Text
type Title = T.Text

data Book = Book {
    author :: Author,
    title :: Title
} deriving Show

type Html = T.Text

-- Some test books
book1 :: Book
book1 = Book { author = "Miran Lipovaca", title = "Learn You a Haskell for Great Good!" }

book2 :: Book
book2 = Book { author = "Bryan O'Sullivan", title = "Real World Haskell" }

book3 :: Book
book3 = Book { author = "Graham Hutton", title = "Programming in Haskell" }

myBooks :: [Book]
myBooks = [book1, book2, book3]

-- Methods that turn books to html
bookToHtml :: Book -> Html
bookToHtml book = mconcat ["<p>\n", titleInTags, authorInTags, "</p>\n"] where
    titleInTags = mconcat ["<strong>", title book, "</strong>\n" ]
    authorInTags = mconcat ["<em>", author book, "</em>\n" ]

booksToHtml :: [Book] -> Html
booksToHtml books = mconcat ["<html>\n",
                            "<head><title>books</title>\n",
                            "<meta charset='utf-8'/>",
                            "</head>\n",
                            "<body>\n",
                            booksHtml,
                            "\n</body>\n",
                            "</html>\n"] where
                                booksHtml = mconcat $ map bookToHtml books