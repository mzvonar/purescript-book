module Test.MySolutions where

import Prelude
import Data.List (filter, head, null, nubByEq)
import Data.Maybe (Maybe)
import Data.AddressBook (AddressBook, Entry, findEntry)

-- Note to reader: Add your solutions to this file

findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street = filter filterEntry >>> head
    where
        filterEntry :: Entry -> Boolean
        filterEntry = _.address.street >>> eq street


isInBook :: String -> String -> AddressBook -> Boolean
isInBook firstName lastName = filter filterEntry >>> not null
    where
      filterEntry :: Entry -> Boolean
      filterEntry entry = entry.firstName == firstName && entry.lastName == lastName


removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubByEq \a -> \b -> a.firstName == b.firstName && a.lastName == b.lastName