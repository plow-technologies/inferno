module Golden.Spec (goldenTests) where

import Inferno.Types.Syntax (BaseType, Ident, InfernoType, ModuleName, TV)
import Inferno.Types.Type (Namespace)
import Inferno.VersionControl.Types
  ( VCIncompatReason,
    VCObjectHash,
    VCObjectPred,
    VCObjectVisibility,
  )
import Test.Aeson.GenericSpecs
  ( Proxy (..),
    roundtripAndGoldenADTSpecs,
  )
import Test.Hspec (Spec)

goldenTests :: Spec
goldenTests = do
  roundtripAndGoldenADTSpecs (Proxy :: Proxy VCObjectHash)
  roundtripAndGoldenADTSpecs (Proxy :: Proxy Ident)
  roundtripAndGoldenADTSpecs (Proxy :: Proxy ModuleName)
  roundtripAndGoldenADTSpecs (Proxy :: Proxy Namespace)
  roundtripAndGoldenADTSpecs (Proxy :: Proxy VCIncompatReason)
  roundtripAndGoldenADTSpecs (Proxy :: Proxy VCObjectPred)
  roundtripAndGoldenADTSpecs (Proxy :: Proxy VCObjectVisibility)
  roundtripAndGoldenADTSpecs (Proxy :: Proxy BaseType)
  roundtripAndGoldenADTSpecs (Proxy :: Proxy TV)
  roundtripAndGoldenADTSpecs (Proxy :: Proxy InfernoType)
