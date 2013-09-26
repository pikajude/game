-- | Convenience module for re-exporting everything.
module Control.Lens.Geometry (
  module Control.Lens,
  module Control.Lens.Geometry.Vector
) where

import Control.Lens hiding (magnify)
import Control.Lens.Geometry.Vector
