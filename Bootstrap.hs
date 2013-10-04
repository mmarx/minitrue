module Bootstrap ( fieldSettingsLabel
                 , renderBootstrap
                 ) where

import Prelude
import Yesod hiding ( fieldSettingsLabel
                    , renderBootstrap
                    )
import qualified Yesod.Form as Form

fieldSettingsLabel :: RenderMessage master msg => msg -> FieldSettings master
fieldSettingsLabel msg = fs { fsAttrs = attrs }
  where fs = Form.fieldSettingsLabel msg
        attrs = fsAttrs fs ++ [ ("class", "form-control") ]

renderBootstrap :: Monad m => FormRender m a
renderBootstrap aform fragment = do
  (res, views') <- aFormToForm aform
  let views = views' []
      has (Just _) = True
      has Nothing = False
  let widget = [whamlet|$newline never
                \#{fragment}
                $forall view <- views
                  \ <div .form-group :has $ fvErrors view:.has-error>
                    <label for=#{fvId view}>#{fvLabel view}
                    ^{fvInput view}
                    $maybe tt <- fvTooltip view
                      <p .help-block>#{tt}
                    $maybe err <- fvErrors view
                      <p .help-block>#{err}
                |]
  return (res, widget)
