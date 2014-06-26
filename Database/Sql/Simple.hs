{-# LANGUAGE ExplicitNamespaces #-}

module Database.Sql.Simple
    ( -- * data type
      Query
    , specify
    , Sql
    , Elem
    , Backend(..)
    , Only(..)
    , (:.)((:.))
    -- * function
    , withConnection
    , sql
    , (+:+)
    , type (++)
    ) where

import Database.Sql.Simple.Internal
