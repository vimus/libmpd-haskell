module Unparse (Unparse(..)) where

class Unparse parsed where
    unparse :: parsed -> String
