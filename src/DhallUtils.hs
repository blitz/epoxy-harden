module DhallUtils
  (prettyPrint, toDhall)
where

import qualified Data.Text                             as T
import           Data.Text.Prettyprint.Doc             (LayoutOptions (LayoutOptions),
                                                        PageWidth (AvailablePerLine),
                                                        layoutPretty)
import qualified Data.Text.Prettyprint.Doc.Render.Text as Prettyprint.Text
import           Data.Void                             (Void)
import           Dhall                                 (ToDhall)
import qualified Dhall
import           Dhall.Core                            (Expr)
import           Dhall.Parser                          (Src)
import           Dhall.Pretty                          (CharacterSet (..))
import qualified Dhall.Pretty

prettyPrint :: Dhall.Core.Expr Src Void -> T.Text
prettyPrint = Prettyprint.Text.renderStrict . layoutPretty (LayoutOptions (AvailablePerLine 80 0.9)). Dhall.Pretty.prettyCharacterSet Unicode

toDhall :: ToDhall a => a -> Expr Src Void
toDhall = Dhall.embed Dhall.inject
