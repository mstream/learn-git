module Infra.Ui.Perspective (perspective) where

import Prelude
import Core.Fs (File)
import Core.StringCodec (encodeToString)
import Infra.Ui.Element as E

perspective :: forall a r. { fileTree :: File | r } -> E.Element a
perspective state = E.pre' [ E.text $ show state.fileTree ]
