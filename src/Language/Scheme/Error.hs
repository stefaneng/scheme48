module Language.Scheme.Error where

import Language.Scheme.Error.Types
import Control.Monad.Except (catchError)

-- | We purposely leave extractValue undefined for a Left constructor, because that represents a programmer error. We intend to use extractValue only after a catchError, so it's better to fail fast than to inject bad values into the rest of the program.
extractValue :: ThrowsError a -> a
extractValue (Right val) = val

-- Needs a type signature
trapError action = catchError action (return . show)
