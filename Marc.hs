{-# LANGUAGE OverloadedStrings #-}
module Marc where

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Books


-- Marc records are split into three parts
-- - the leader (contains length of record and where to find the base record)
-- - the directory (tells you about info on the record and where to find it)
-- - the base record (where all the info is stored)

-- As there is no delimiter for splitting lists of records, we need to look at the 
-- leader to see how long each record is. Once we do this we can iteratate through
-- the list and collect records.

type MarcRecordRaw = B.ByteString
type MarcLeaderRaw = B.ByteString

-- the leader is the first 24 bytes of the record
leaderLength :: Int
leaderLength = 24

getLeader :: MarcRecordRaw -> MarcLeaderRaw
getLeader = B.take leaderLength

-- The first 5 bytes of the leader says the length of the record
-- So to get the length, we have to convert these 5 bytes to Int

rawToInt :: B.ByteString -> Int
rawToInt = read . T.unpack . E.decodeUtf8

getRecordLength :: MarcLeaderRaw -> Int
getRecordLength = rawToInt . B.take 5

-- In order to split raw data into records of variable bytes:
-- We must create a function to split the raw data into the 
-- first record and then the rest. We will then use this 
-- function recursively.

nextAndRest :: B.ByteString -> (MarcRecordRaw, B.ByteString)
nextAndRest marcStream = B.splitAt recordLength marcStream where
    recordLength = getRecordLength marcStream

getRecords :: B.ByteString -> [MarcRecordRaw]
getRecords marcStream
    | marcStream == B.empty = []
    | otherwise = next : getRecords rest where
        (next, rest) = nextAndRest marcStream

-- now time to get the directory
type MarcDirectoryRaw = B.ByteString

-- the leader tells you where the base record begins.
-- in order to get the directory we have to get the base address
-- from the leader. It's located in the 12th - 16th byte (5 bytes)

-- To access this we will take the leader, drop 12 and then take 5
-- bytes from the remaining 12 of the leader.

getBaseAddress :: MarcLeaderRaw -> Int
getBaseAddress leader = rawToInt (B.take 5 remainder) where
    remainder = B.drop 12 leader

getDirLength :: MarcLeaderRaw -> Int
getDirLength leader = getBaseAddress leader - (leaderLength + 1)

-- Now we have the starting byte and length of the directory we
-- can get it!

getDirectory :: MarcRecordRaw -> MarcDirectoryRaw
getDirectory record = B.take directoryLength afterLeader where
    directoryLength = getDirLength record
    afterLeader = B.drop leaderLength record

-- Now it's time to look up fields using the directory
-- each instance of field metadata is 12 bytes

type MarcDirEntryRaw = B.ByteString

dirEntryLength :: Int
dirEntryLength = 12

splitDirectory :: MarcDirectoryRaw -> [MarcDirEntryRaw]
splitDirectory directory
    | directory == B.empty = []
    | otherwise = nextEntry : splitDirectory restEntries where
        (nextEntry, restEntries) = B.splitAt dirEntryLength directory

-- Processing entries and looking up fields

-- Each entry in the directory has 
-- -- Tag of the field (first 3 characters)
-- -- Length of the field (next 4 chars)
-- -- Where the field starts relative to the base address (rest of chars)

-- We need all this info so we make a type for it

data FieldMetaData = FieldMetaData { 
    tag :: T.Text, 
    fieldLength :: Int, 
    fieldStart :: Int
} deriving Show

-- Next we have to process each raw directory entry into field meta data
-- so we can then process a list of entries.

buildFieldMetaData :: MarcDirEntryRaw -> FieldMetaData
buildFieldMetaData entry = FieldMetaData textTag fLength fStart where
    (theTag, rest) = B.splitAt 3 entry
    (rawLength, rawStart) = B.splitAt 4 rest
    textTag =  E.decodeUtf8 theTag
    fLength = rawToInt rawLength
    fStart = rawToInt rawStart

getFieldMetaData :: [MarcDirEntryRaw] -> [FieldMetaData]
getFieldMetaData = map buildFieldMetaData

type FieldText = T.Text

-- Now we can get the meta data field text! We have to drop both 
-- the leader and the directory to get the base. Then we drop the
-- fieldStart and take the fieldLength from the remaining bit.

getTextField :: MarcRecordRaw -> FieldMetaData -> FieldText
getTextField record fieldMetaData = E.decodeUtf8 byteStringVal where
    baseAddress = getBaseAddress $ getLeader record
    baseRecord = B.drop baseAddress record
    baseAtEntry = B.drop (fieldStart fieldMetaData) baseRecord
    byteStringVal = B.take (fieldLength fieldMetaData) baseAtEntry


-- In MARC records each value is associated with a tag. Each field
-- is also made up of sub fields separated by the 31st ASCII char.

fieldDelimiter :: Char
fieldDelimiter = toEnum 31

-- Each subfield is represented by a single char. Each subfield has 
-- a value. Before the value is a letter representing a subfield.

titleTag :: T.Text
titleTag = "245"

titleSubfield :: Char
titleSubfield = 'a'

authorTag :: T.Text
authorTag = "100"

authorSubfield :: Char
authorSubfield = 'a'

-- To get a value of a field we need to look up location in the record
-- by splitting te metadata into subfield, and looking at the first
-- char of the subfield to see whether the subfield we want is there.

lookupFieldMetaData :: T.Text -> MarcRecordRaw -> Maybe FieldMetaData
lookupFieldMetaData aTag record
    | length results < 1 = Nothing
    | otherwise =  Just (head results) where
        metaData = (getFieldMetaData . splitDirectory . getDirectory) record
        results = filter ((== aTag). tag) metaData

-- now we can look up a subfield

lookupSubfield :: Maybe FieldMetaData -> Char -> MarcRecordRaw -> Maybe T.Text
lookupSubfield Nothing _ _ = Nothing
lookupSubfield (Just fieldMetaData) subfield record
    | null results = Nothing
    | otherwise = Just ((T.drop 1 . head) results) where
        rawField = getTextField record fieldMetaData
        subfields = T.split (== fieldDelimiter) rawField
        results = filter ((== subfield). T.head) subfields

lookupValue :: T.Text -> Char -> MarcRecordRaw -> Maybe T.Text
lookupValue aTag subfield record = lookupSubfield entryMetaData subfield record where
    entryMetaData = lookupFieldMetaData aTag record

-- Now we can get the title and author!

lookupTitle :: MarcRecordRaw -> Maybe Title
lookupTitle = lookupValue titleTag titleSubfield

lookupAuthor :: MarcRecordRaw -> Maybe Title
lookupAuthor = lookupValue authorTag authorSubfield