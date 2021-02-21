module Infra.Ui.Perspective (perspective) where

import Prelude
import Core.Fs (File(..), FileContent, FileName, isBinary)
import Core.StringCodec (encodeToString)
import Data.Map (Map, toUnfoldable)
import Data.Tuple.Nested (type (/\), (/\))
import Infra.Ui.Element as E
import Infra.Ui.Icon as I

perspective :: forall a r. { fileTree :: File | r } -> E.Element a
perspective state = case state.fileTree of
  DirectoryOf entries -> treeView 0 entries
  RegularFileContaining _ -> E.div' []

treeView :: forall a. Int -> Map FileName File -> E.Element a
treeView level entries = E.ul [ E.className "pl-6" ] $ toUnfoldable entries <#> entryView
  where
  entryView :: FileName /\ File -> E.Element a
  entryView (name /\ file) =
    let
      nameEl :: E.Element a
      nameEl = E.text $ encodeToString name
    in
      E.li [ E.className $ groupName level <> " flex flex-row" ]
        $ case file of
            RegularFileContaining content ->
              [ E.div [ E.className $ groupName level <> "-hover:underline" ] [ I.document, nameEl ]
              , contentView content
              ]
            DirectoryOf childEntries ->
              [ E.div' [ I.folder, nameEl ]
              , treeView (level + 1) childEntries
              ]

  contentView :: FileContent -> E.Element a
  contentView content =
    E.div
      [ E.className $ groupName level <> "-hover:opacity-100 flex flex-col opacity-0 border border-double border-black p-4" ]
      [ E.text "File content preview"
      , E.div [ E.className "shadow-inner" ] [ if isBinary content then E.text "binary" else E.pre' [ E.text $ encodeToString content ] ]
      ]

groupName :: Int -> String
groupName level = "group-" <> show level
