module GenericJSON where

import Prelude
import Control.Alt ((<|>))
import Control.Monad.Except (withExcept)
import Data.Generic.Rep as GR
import Foreign (Foreign, ForeignError(..), fail, unsafeToForeign)
import Foreign as Foreign
import Prim.Row as Row
import Record (union)
import Simple.JSON as JSON
import Type.Prelude (class IsSymbol, Proxy(..), reflectSymbol)
import Unsafe.Coerce (unsafeCoerce)

-- | Read implementation for simple-json for generic things
taggedSumRep
  :: forall a rep
   . GR.Generic a rep
  => GenericTaggedSumRep rep
  => Foreign
  -> Foreign.F a
taggedSumRep f = GR.to <$> genericTaggedSumRep f

-- | Write implementation for simple-json for generic things
writeTaggedSumRep
  :: forall a rep
   . GR.Generic a rep
  => GenericTaggedSumRep rep
  => a
  -> Foreign
writeTaggedSumRep f = writeGenericTaggedSumRep $ GR.from f

-- | Generic Tagged Sum Representations, tagged with a "type" field
class GenericTaggedSumRep rep where
  genericTaggedSumRep :: Foreign -> Foreign.F rep
  writeGenericTaggedSumRep :: rep -> Foreign

instance taggedSumRepSum ::
  ( GenericTaggedSumRep a
  , GenericTaggedSumRep b
  ) =>
  GenericTaggedSumRep (GR.Sum a b) where
  genericTaggedSumRep f =
    GR.Inl <$> genericTaggedSumRep f
      <|> GR.Inr
        <$> genericTaggedSumRep f
  writeGenericTaggedSumRep f = case f of
    GR.Inl x -> writeGenericTaggedSumRep x
    GR.Inr x -> writeGenericTaggedSumRep x

class IsRecordArg a

instance isRecordArg :: IsRecordArg (GR.Argument (Record r))

instance taggedSumRepConstructorRecord ::
  ( GenericTaggedSumRep (GR.Argument (Record r))
  , IsSymbol name
  , IsRecordArg (GR.Argument (Record r))
  , Row.Lacks "__type" r
  ) =>
  GenericTaggedSumRep (GR.Constructor name (GR.Argument (Record r))) where
  genericTaggedSumRep f = do
    r :: { "__type" :: String } <- JSON.read' f
    if r."__type" == name then
      withExcept (map $ ErrorAtProperty name) $ GR.Constructor <$> genericTaggedSumRep f
    else
      fail $ ForeignError $ "Wrong type tag " <> r."__type" <> " where " <> name <> " was expected."
    where
    nameP = Proxy :: Proxy name

    name = reflectSymbol nameP
  writeGenericTaggedSumRep (GR.Constructor z :: GR.Constructor name _) =
    let
      value = JSON.write $ writeGenericTaggedSumRep z

      merged = union ({ "__type": name }) (unsafeCoerce value)
    in
      unsafeToForeign $ merged
    where
    nameP = Proxy :: Proxy name

    name = reflectSymbol nameP
else instance taggedSumRepNoArguments ::
  ( IsSymbol name
  ) =>
  GenericTaggedSumRep (GR.Constructor name GR.NoArguments) where
  genericTaggedSumRep f = do
    r :: String <- JSON.read' f
    if r == name then
      pure $ GR.Constructor GR.NoArguments
    else
      fail $ ForeignError $ "Unknown string tag " <> name
    where
    nameP = Proxy :: Proxy name

    name = reflectSymbol nameP
  writeGenericTaggedSumRep (GR.Constructor z :: GR.Constructor name _) = JSON.write name
    where
    nameP = Proxy :: Proxy name

    name = reflectSymbol nameP
else instance taggedSumRepConstructor ::
  ( GenericTaggedSumRep a
  , IsSymbol name
  ) =>
  GenericTaggedSumRep (GR.Constructor name a) where
  genericTaggedSumRep f = do
    r :: { "__type" :: String, value :: Foreign } <- JSON.read' f
    if r."__type" == name then
      withExcept (map $ ErrorAtProperty name) $ GR.Constructor <$> genericTaggedSumRep r.value
    else
      fail $ ForeignError $ "Wrong type tag " <> r."__type" <> " where " <> name <> " was expected."
    where
    nameP = Proxy :: Proxy name

    name = reflectSymbol nameP
  writeGenericTaggedSumRep (GR.Constructor a) = unsafeToForeign $ { "__type": name, value: JSON.write $ writeGenericTaggedSumRep a }
    where
    nameP = Proxy :: Proxy name

    name = reflectSymbol nameP

instance taggedSumRepArgument ::
  ( JSON.ReadForeign a
  , JSON.WriteForeign a
  ) =>
  GenericTaggedSumRep (GR.Argument a) where
  genericTaggedSumRep f = GR.Argument <$> JSON.readImpl f
  writeGenericTaggedSumRep (GR.Argument a) = JSON.writeImpl a
