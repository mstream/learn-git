module Infra.Ui.Icon (document, folder, searchCircle) where

import Infra.Ui.Element as E

icon :: forall a. String -> E.Element a
icon d =
  E.div [ E.className "inline-flex self-center" ]
    [ E.svg
        [ E.className "h-4 relative stroke-2 stroke-current top-0.5 w-4"
        , E.fill "none"
        , E.viewBox "0 0 24 24"
        ]
        [ E.path
            [ E.d d ]
            []
        ]
    ]

document :: forall a. E.Element a
document = icon "M7 21h10a2 2 0 002-2V9.414a1 1 0 00-.293-.707l-5.414-5.414A1 1 0 0012.586 3H7a2 2 0 00-2 2v14a2 2 0 002 2z"

folder :: forall a. E.Element a
folder = icon "M3 7v10a2 2 0 002 2h14a2 2 0 002-2V9a2 2 0 00-2-2h-6l-2-2H5a2 2 0 00-2 2z"

searchCircle :: forall a. E.Element a
searchCircle = icon "M8 16l2.879-2.879m0 0a3 3 0 104.243-4.242 3 3 0 00-4.243 4.242zM21 12a9 9 0 11-18 0 9 9 0 0118 0z"
