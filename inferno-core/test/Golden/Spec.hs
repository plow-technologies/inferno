{-# LANGUAGE TypeApplications #-}

module Golden.Spec (goldenTests) where

import Data.Proxy (Proxy (Proxy))
import Inferno.Types.Syntax (BaseType, Ident, InfernoType, ModuleName, TV)
import Inferno.Types.Type (Namespace)
import Inferno.Instances.Arbitrary ()
import Inferno.VersionControl.Types
  ( VCIncompatReason,
    VCObjectHash,
    VCObjectPred,
    VCObjectVisibility,
  )
import qualified Test.Aeson.GenericSpecs as Aeson
  ( roundtripAndGoldenADTSpecs,
  )
import qualified Test.Cereal.GenericSpecs as Cereal
  ( roundtripAndGoldenADTSpecs,
  )
import Test.Hspec (Spec)

goldenTests :: Spec
goldenTests = do
  goldenAesonTests
  goldenCerealTests

goldenAesonTests :: Spec
goldenAesonTests = do
  Aeson.roundtripAndGoldenADTSpecs $ Proxy @VCObjectHash
  Aeson.roundtripAndGoldenADTSpecs $ Proxy @Ident
  Aeson.roundtripAndGoldenADTSpecs $ Proxy @ModuleName
  Aeson.roundtripAndGoldenADTSpecs $ Proxy @Namespace
  Aeson.roundtripAndGoldenADTSpecs $ Proxy @VCIncompatReason
  Aeson.roundtripAndGoldenADTSpecs $ Proxy @VCObjectPred
  Aeson.roundtripAndGoldenADTSpecs $ Proxy @VCObjectVisibility
  Aeson.roundtripAndGoldenADTSpecs $ Proxy @BaseType
  Aeson.roundtripAndGoldenADTSpecs $ Proxy @TV
  Aeson.roundtripAndGoldenADTSpecs $ Proxy @InfernoType

goldenCerealTests :: Spec
goldenCerealTests = do
  Cereal.roundtripAndGoldenADTSpecs $ Proxy @VCObjectHash
