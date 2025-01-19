module ToyProlog.Parsing.Common
  ( module Text.Parsec,
    module Text.Parsec.Text,
  )
where

import Text.Parsec hiding (many, (<|>))
import Text.Parsec.Text (Parser)
