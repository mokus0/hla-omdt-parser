{-# LANGUAGE TemplateHaskell #-}
module Text.OMDT.Syntax.Labels where

import Data.Record.Label

import Text.OMDT.Syntax

$(mkLabels [ ''ObjectModel, ''ObjectModelHeader, ''POC
           , ''EnumeratedDataType, ''Enumerator
           , ''ComplexDataType, ''ComplexComponent
           , ''Class, ''Attribute
           , ''Interaction, ''Parameter
           , ''RoutingSpace, ''Dimension
           ])