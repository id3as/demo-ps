module BookApp where

import Prelude
import BookSup as BookSup

import Pinto.App as App

start = App.simpleStart BookSup.startLink
