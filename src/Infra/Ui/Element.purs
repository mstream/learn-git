module Infra.Ui.Element (module Exports, Element, runInDom) where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div, div', input, i, i', li, li', p', p, pre, pre', text, ul, ul') as Exports
import Concur.React.Props (_type, className, d, fill, onChange, onKeyEnter, unsafeTargetValue, value, viewBox) as Exports
import Concur.React.SVG (path, path', svg, svg') as Exports
import Concur.React.Run (runWidgetInDom)
import Effect (Effect)
import Prelude (Unit)

type Element a
  = Widget HTML a

runInDom :: forall a. String -> Element a -> Effect Unit
runInDom domElId el = runWidgetInDom domElId el
