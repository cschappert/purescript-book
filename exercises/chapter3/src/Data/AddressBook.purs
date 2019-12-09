module Data.AddressBook where

import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head)
import Data.Maybe (Maybe)

type Address =
  -- from { to } is a record. similar to JS Objects, Structs, etc.
  -- a record can contain another record
  { street :: String
  , city   :: String
  , state  :: String
  }

-- this is a type synonym
-- a more simple example - type Name = String
type Entry =
  { firstName :: String
  , lastName  :: String
  , address   :: Address
  }

-- a linked list of Entry records. List is distinct from Array
-- apply the type constructor List to Entry to get a list of entries
type AddressBook = List Entry

showAddress :: Address -> String
showAddress addr = addr.street <> ", " <> addr.city <> ", " <> addr.state

showEntry :: Entry -> String
showEntry entry = entry.lastName <> ", " <> entry.firstName <> ": " <> showAddress entry.address

printEntry :: String -> String -> AddressBook -> Maybe String
printEntry firstName lastName book = map showEntry (findEntry firstName lastName book)

emptyBook :: AddressBook
emptyBook = empty

-- using eta conversion to write function in point-free form.
-- other than the restrictions on type, insertEntry is the same function as Cons
insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName = head <<< filter filterEntry
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.firstName == firstName && entry.lastName == lastName
