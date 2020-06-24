module BookApp where

import Prelude
import BookSup as BookSup

import Pinto.App as App

-- Our entry point is not tremendously exciting
-- but it is an entry point
start = App.simpleStart BookSup.startLink
