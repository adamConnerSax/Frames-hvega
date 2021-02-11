{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Frames.Visualization.VegaLite.Data
  (
    -- * Classes
    ToVLDataValue(..) -- derive this for any col type you wish to render as a datum for a VegaLite visualization

    -- * Types
  , Row
  , VLCoRecHandlers
  , EHandler (..)

    -- * Constraints
  , DataField
  , DataFieldOf

    -- * builders
  , vinylRows
  , emptyRowBuilder
  , addRowBuilder
  , defaultParse
  , addParse

    -- * utilities
  , minMaxFieldF
  , axisBounds

    -- * records -> hvega data
  , addMappedColumn
  , recordToVLDataRow
  , recordsToVLData
  , recordsToData
  , recordsToDataWithParse
  , pivotedRecordsToVLDataRows
  , simplePivotFold
  , recordToDataRow
  , asVLData
  , asVLNumber
  , asVLCoRec
  , textAsVLStr
  , asVLStrViaShow
  , useColName
  , fromToVLDataValue

    -- * helpers
  , colName
  , colNames
  , pName
  , mName
  , fName
  , tName
  , hName
  , oName
  , dName
    -- * Re-exports
  )
where

import           Graphics.Vega.VegaLite.Configuration
                                                ( AxisBounds(..) )

import           Data.Fixed                     ( div'
                                                , divMod'
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Time                     as DT

import qualified Control.Foldl                 as FL
import qualified Data.List                     as List
import           Data.Proxy                     ( Proxy(..) )

import qualified Graphics.Vega.VegaLite        as GV
import qualified Data.Vinyl                    as V
import qualified Data.Vinyl.Functor            as V
import qualified Data.Vinyl.TypeLevel          as V
import qualified Data.Vinyl.CoRec              as V
import qualified Frames                        as F
import qualified Frames.Melt                   as F
import qualified Frames.Transform              as FT
import qualified Frames.MapReduce              as FMR
import qualified Control.MapReduce             as MR

import           GHC.TypeLits                   ( Symbol, KnownSymbol, symbolVal )
import           Data.Kind                      ( Type )
import Unsafe.Coerce (unsafeCoerce)

type Row = F.Record

type DataField t = (V.KnownField t, ToVLDataValue (F.ElField t))
type DataFieldOf rs t = (DataField t, F.ElemOf rs t)

type RowBuilderF a rs = F.Rec (V.Lift (->) (V.Const a) V.ElField) rs

emptyRowBuilder :: RowBuilderF a '[]
emptyRowBuilder = V.RNil

vinylRows
  :: forall (rs :: [(Symbol, Type)]) f a
   . (Functor f, V.RMap rs)
  => RowBuilderF a rs
  -> f a
  -> f (Row rs)
vinylRows dataLoaders dat =
  fmap (\a -> V.rmap (\lg -> V.getLift lg (V.Const a)) dataLoaders) dat

addRowBuilder
  :: forall t ts a
   .  V.KnownField t --DataField t
  => (a -> V.Snd t)
  -> RowBuilderF a ts
  -> RowBuilderF a (t ': ts)
addRowBuilder g builders = V.Lift (V.Field . g . V.getConst) V.:& builders

minMaxFieldF
  :: forall t rs
   . (DataFieldOf rs t, Ord (V.Snd t))
  => FL.Fold (F.Record rs) (Maybe (V.Snd t, V.Snd t))
minMaxFieldF =
  let minF = FL.premap (F.rgetField @t) FL.minimum
      maxF = FL.premap (F.rgetField @t) FL.maximum
      joinTuple (ma, mb) = ma >>= (\a -> mb >>= (\b -> return (a, b)))
  in  fmap joinTuple $ (,) <$> minF <*> maxF

axisBoundsPChannel :: Real a => a -> a -> [GV.PositionChannel]
axisBoundsPChannel lo hi =
  [GV.PScale [GV.SDomain $ GV.DNumbers [realToFrac lo, realToFrac hi]]]

axisBounds
  :: forall t rs
   . (DataFieldOf rs t, Real (V.Snd t))
  => AxisBounds (V.Snd t)
  -> FL.Fold (F.Record rs) [GV.PositionChannel]
axisBounds Default = pure []
axisBounds DataMinMax =
  fmap (maybe [] (uncurry axisBoundsPChannel)) (minMaxFieldF @t)
axisBounds (GivenMinMax lo hi) = pure $ axisBoundsPChannel lo hi

addMappedColumn :: forall t c rs f . (V.KnownField t
                                     , V.KnownField c
                                     , F.ElemOf rs t
                                     , Functor f)
                => (V.Snd t -> V.Snd c)
                -> f (Row rs)
                -> f (Row (rs V.++ '[c]))
addMappedColumn g = fmap (FT.mutate $ \r -> FT.recordSingleton @c (g $ F.rgetField @t r))

recordToVLDataRow'
  :: ( V.RMap rs
     , V.ReifyConstraint ToVLDataValue F.ElField rs
     , V.RecordToList rs
     )
  => Row rs
  -> [(Text, GV.DataValue)]
recordToVLDataRow' xs =
  V.recordToList
    . V.rmap (\(V.Compose (V.Dict x)) -> V.Const $ toVLDataValue x)
    $ V.reifyConstraint @ToVLDataValue xs

recordToVLDataRow
  :: ( V.RMap rs
     , V.ReifyConstraint ToVLDataValue F.ElField rs
     , V.RecordToList rs
     )
  => Row rs
  -> [GV.DataRow]
recordToVLDataRow r = GV.dataRow (recordToVLDataRow' r) []

-- using a record of functions
type VLDataRecF = V.Lift (->) V.ElField (V.Const (T.Text, GV.DataValue))

recordToDataRow
  :: ( V.RApply rs
     , V.RecordToList rs
     )
  => V.Rec VLDataRecF rs
  -> Row rs
  -> [GV.DataRow]
recordToDataRow toDVRec r = GV.dataRow (V.recordToList $ V.rapply toDVRec r) []

{-
recordToDataRow'
  :: forall rs.
  ( V.RApply rs
  , V.RecordToList rs
  , F.ColumnHeaders rs
  )
  => V.Rec (V.Lift (->) V.ElField (V.Const GV.DataValue)) rs
  -> Row rs
  -> [GV.DataRow]
recordToDataRow' toDVRec r =
  let cNames = fmap T.pack $ F.columnHeaders (Proxy :: Proxy (F.Record rs))
  in GV.dataRow (zip cNames $ V.recordToList $ V.rapply toDVRec r) []
-}

asVLData :: V.KnownField t
         => (V.Snd t -> GV.DataValue)
         -> GV.FieldName
         -> VLDataRecF  t
asVLData toVLData fieldName = V.Lift $ V.Const . (\x -> (fieldName, toVLData x)) . V.getField

{-
asVLData' :: forall t. V.KnownField t
         => (V.Snd t -> GV.DataValue)
         -> VLDataRecF t
asVLData' toVLData = asVLData (T.pack $ symbolVal (Proxy :: Proxy (V.Fst t))) toVLData
-}

asVLNumber :: (V.KnownField t
              , Real (V.Snd t)
              )
           => GV.FieldName
           -> VLDataRecF  t
asVLNumber = asVLData (GV.Number . realToFrac)

textAsVLStr :: (V.KnownField t
               , V.Snd t ~ T.Text
               )
           => GV.FieldName
           -> VLDataRecF  t
textAsVLStr = asVLData GV.Str

asVLStrViaShow :: (V.KnownField t
                  , Show (V.Snd t)
                  )
           => GV.FieldName
           -> VLDataRecF  t
asVLStrViaShow = asVLData (GV.Str . T.pack . show)


useColName
  :: forall t. V.KnownField t
  => (GV.FieldName -> VLDataRecF t)
  -> VLDataRecF t
useColName f =
  let cName = T.pack $ symbolVal (Proxy :: Proxy (V.Fst t))
  in f cName

type VLCoRecHandlers vs = EHandlers vs (Text, GV.DataValue)

-- YIKES
identityCoRec ::  forall ts.V.CoRec V.ElField ts -> V.CoRec V.Identity (F.UnColumn ts)
identityCoRec = unsafeCoerce --(V.CoRec @V.Identity @(F.UnColumn ts) . V.Identity . V.getField)

newtype EHandler b a = EH (V.ElField a -> b)
type EHandlers ts b = V.Rec (EHandler b) ts

class HandlersToEHandlers (ts :: [(Symbol, Type)]) where
  handlersToEHandlers :: V.Handlers (F.UnColumn ts) b -> EHandlers ts b

instance HandlersToEHandlers '[] where
  handlersToEHandlers _ = V.RNil

instance (HandlersToEHandlers ts, t ~ (s, a)) => HandlersToEHandlers (t ': ts) where
  handlersToEHandlers (x V.:& xs) = undefined
--    case t of
--      V.H f -> EH (f . V.getField) V.:& handlersToEHandlers ts

matchE :: forall ts b. EHandlers ts b -> V.CoRec V.ElField ts -> b
matchE hs (V.CoRec x) = aux x
  where aux :: forall a. V.RElem a ts (V.RIndex a ts) => V.ElField a -> b
        aux x = case V.rget @a hs of
                  EH f -> f x

asVLCoRec :: (V.KnownField t
             , V.Snd t ~ V.CoRec V.ElField vs
             ) => VLCoRecHandlers vs -> VLDataRecF t
asVLCoRec h = V.Lift $ V.Const . (\x -> matchE h $ V.getField x)


fromToVLDataValue :: ToVLDataValue (F.ElField t) => VLDataRecF t
fromToVLDataValue = V.Lift $ V.Const . toVLDataValue

-- combine multi-row data
pivotedRecordsToVLDataRows ::
  forall ks ps rs f. ( Ord (Row ks)
                     , V.RMap ks
                     , V.ReifyConstraint ToVLDataValue F.ElField ks
                     , V.RecordToList ks
                     , ps F.⊆ rs -- ^ pivot/data columns
                     , ks F.⊆ rs -- ^ key columns
                     , ps F.⊆ rs
                     , Foldable f
                    )
  =>  FL.Fold (Row ps) [(T.Text, GV.DataValue)]
  -> f (Row rs)
  -> [GV.DataRow]
pivotedRecordsToVLDataRows pivotFold rows =
  let rowsFold = FMR.mapReduceFold
        MR.noUnpack
        (FMR.assignKeysAndData @ks @ps)
        (MR.ReduceFold $ (\k -> fmap (\pVLDat -> recordToVLDataRow' k ++ pVLDat) pivotFold))
  in concat $ fmap (\x -> GV.dataRow x []) (FL.fold rowsFold rows)

-- pivot key becomes text, data cols become labeled values, each merged with key per given function
simplePivotFold ::
  forall ps ds. (ps F.⊆ (ps V.++ ds)
                ,ds F.⊆ (ps V.++ ds)
                )
  => (T.Text -> T.Text -> Text)
  -> (Row ps -> T.Text)
  -> (Row ds -> [(T.Text, GV.DataValue)])
  -> FL.Fold (Row (ps V.++ ds)) [(T.Text, GV.DataValue)]
simplePivotFold labelDataWithKey keyToText datToVals =
  let label r = keyToText $ F.rcast r
      datVals r = datToVals $ F.rcast r
      doOne r = fmap ((\(l,d) -> (labelDataWithKey (label r) l ,d))) $ datVals r
  in fmap (concat . fmap doOne) $ FL.list


type RowParseRec rs = F.Rec (V.Const GV.DataType) rs

defaultParse :: RowParseRec '[]
defaultParse = V.RNil

--parseField :: forall t . V.KnownField t => GV.DataType -> RowParseRec '[t]
--parseField dt = V.Const dt V.:& defaultParse

addParse
  :: forall t ts
   . V.KnownField t
  => GV.DataType
  -> RowParseRec ts
  -> RowParseRec (t ': ts)
addParse dt rpr = V.Const dt V.:& rpr

-- TODO: check if one list needs reversing!!
rowParseRecToHvegaParseList
  :: forall rs
   . (V.RFoldMap rs, F.ColumnHeaders rs)
  => RowParseRec rs
  -> [(T.Text, GV.DataType)]
rowParseRecToHvegaParseList rpr =
  List.zip (colNames @rs) $ V.rfoldMap (\(V.Const dt) -> [dt]) rpr

recordsToVLData
  :: ( V.RMap as
     , V.ReifyConstraint ToVLDataValue F.ElField as
     , V.RecordToList as
     , bs F.⊆ as
     , V.RFoldMap bs
     , F.ColumnHeaders bs
     , Foldable f
     )
  => (F.Record rs -> F.Record as)
  -> RowParseRec bs -- ^ Parse info for any field that needs it
  -> f (Row rs)
  -> GV.Data
recordsToVLData transform rpr xs =
  GV.dataFromRows [GV.Parse $ rowParseRecToHvegaParseList rpr]
    $ List.concat
    $ fmap (recordToVLDataRow . transform)
    $ FL.fold FL.list xs

recordsToData
  :: ( V.RApply rs
     , V.RecordToList rs
     , Foldable f
     )
  => V.Rec (V.Lift (->) V.ElField (V.Const (T.Text, GV.DataValue))) rs
  -> f (Row rs)
  -> GV.Data
recordsToData = recordsToDataWithParse []

recordsToDataWithParse
  :: ( V.RApply rs
     , V.RecordToList rs
     , Foldable f
     )
  => [(T.Text, GV.DataType)]
  -> V.Rec (V.Lift (->) V.ElField (V.Const (T.Text, GV.DataValue))) rs
  -> f (Row rs)
  -> GV.Data
recordsToDataWithParse parseList toDataRowRec xs =
  GV.dataFromRows [GV.Parse $ parseList]
    $ List.concat
    $ fmap (recordToDataRow toDataRowRec)
    $ FL.fold FL.list xs


colName :: forall x . (F.ColumnHeaders '[x]) => Text
colName = List.head $ colNames @'[x] --T.pack $ List.head $ F.columnHeaders (Proxy :: Proxy (F.Record '[x]))

colNames :: forall rs . (F.ColumnHeaders rs) => [Text]
colNames = T.pack <$> F.columnHeaders (Proxy :: Proxy (F.Record rs))

pName :: forall x . (F.ColumnHeaders '[x]) => GV.PositionChannel
pName = GV.PName (colName @x)

mName :: forall x . (F.ColumnHeaders '[x]) => GV.MarkChannel
mName = GV.MName (colName @x)

fName :: forall x . (F.ColumnHeaders '[x]) => GV.FacetChannel
fName = GV.FName (colName @x)

tName :: forall x . (F.ColumnHeaders '[x]) => GV.TextChannel
tName = GV.TName (colName @x)

hName :: forall x . (F.ColumnHeaders '[x]) => GV.HyperlinkChannel
hName = GV.HName (colName @x)

oName :: forall x . (F.ColumnHeaders '[x]) => GV.OrderChannel
oName = GV.OName (colName @x)

dName :: forall x . (F.ColumnHeaders '[x]) => GV.DetailChannel
dName = GV.DName (colName @x)

class ToVLDataValue x where
  toVLDataValue :: x -> (Text, GV.DataValue)

instance ToVLDataValue (F.ElField '(s, Int)) where
  toVLDataValue x =
    (T.pack $ V.getLabel x, GV.Number $ realToFrac $ V.getField x)

instance ToVLDataValue (F.ElField '(s, Integer)) where
  toVLDataValue x =
    (T.pack $ V.getLabel x, GV.Number $ realToFrac $ V.getField x)

instance ToVLDataValue (F.ElField '(s, Double)) where
  toVLDataValue x = (T.pack $ V.getLabel x, GV.Number $ V.getField x)

instance ToVLDataValue (F.ElField '(s, Float)) where
  toVLDataValue x =
    (T.pack $ V.getLabel x, GV.Number $ realToFrac $ V.getField x)

instance ToVLDataValue (F.ElField '(s, String)) where
  toVLDataValue x = (T.pack $ V.getLabel x, GV.Str $ T.pack $ V.getField x)

instance ToVLDataValue (F.ElField '(s, Text)) where
  toVLDataValue x = (T.pack $ V.getLabel x, GV.Str $ V.getField x)

instance ToVLDataValue (F.ElField '(s, Bool)) where
  toVLDataValue x = (T.pack $ V.getLabel x, GV.Boolean $ V.getField x)


{-
instance ToVLDataValue (F.ElField '(s, Int32)) where
  toVLDataValue x = (T.pack $ V.getLabel x, GV.Number $ realToFrac $ V.getField x)

instance ToVLDataValue (F.ElField '(s, Int64)) where
  toVLDataValue x = (T.pack $ V.getLabel x, GV.Number $ realToFrac $ V.getField x)
-}

instance ToVLDataValue (F.ElField '(s, DT.Day)) where
  toVLDataValue x =
    (T.pack $ V.getLabel x, GV.DateTime $ toVLDateTime $ V.getField x)

instance ToVLDataValue (F.ElField '(s, DT.TimeOfDay)) where
  toVLDataValue x =
    (T.pack $ V.getLabel x, GV.DateTime $ toVLDateTime $ V.getField x)

instance ToVLDataValue (F.ElField '(s, DT.LocalTime)) where
  toVLDataValue x =
    (T.pack $ V.getLabel x, GV.DateTime $ toVLDateTime $ V.getField x)


vegaLiteMonth :: Int -> GV.MonthName
vegaLiteMonth 1  = GV.Jan
vegaLiteMonth 2  = GV.Feb
vegaLiteMonth 3  = GV.Mar
vegaLiteMonth 4  = GV.Apr
vegaLiteMonth 5  = GV.May
vegaLiteMonth 6  = GV.Jun
vegaLiteMonth 7  = GV.Jul
vegaLiteMonth 8  = GV.Aug
vegaLiteMonth 9  = GV.Sep
vegaLiteMonth 10 = GV.Oct
vegaLiteMonth 11 = GV.Nov
vegaLiteMonth 12 = GV.Dec
vegaLiteMonth _  = undefined

{-
--this is what we should do once we can use time >= 1.9
vegaLiteDay :: DT.DayOfWeek -> GV.DayName
vegaLiteDay DT.Monday = GV.Mon
vegaLiteDay DT.Tuesday = GV.Tue
vegaLiteDay DT.Wednesday = GV.Wed
vegaLiteDay DT.Thursday = GV.Thu
vegaLiteDay DT.Friday = GV.Fri
vegaLiteDay DT.Saturday = GV.Sat
vegaLiteDay DT.Sunday = GV.Mon
-}

vegaLiteDay :: Int -> GV.DayName
vegaLiteDay 1 = GV.Mon
vegaLiteDay 2 = GV.Tue
vegaLiteDay 3 = GV.Wed
vegaLiteDay 4 = GV.Thu
vegaLiteDay 5 = GV.Fri
vegaLiteDay 6 = GV.Sat
vegaLiteDay 7 = GV.Mon
vegaLiteDay _ = undefined

vegaLiteDate :: DT.Day -> [GV.DateTime]
vegaLiteDate x =
  let (y, m, d) = DT.toGregorian x
  in  [ GV.DTYear (fromIntegral y)
      , GV.DTMonth (vegaLiteMonth m)
      , GV.DTDay (vegaLiteDay d)
      ]

vegaLiteTime :: DT.TimeOfDay -> [GV.DateTime]
vegaLiteTime x =
  let (sec, remainder) = (DT.todSec x) `divMod'` 1
      ms               = (1000 * remainder) `div'` 1
  in  [ GV.DTHours (DT.todHour x)
      , GV.DTMinutes (DT.todMin x)
      , GV.DTSeconds sec
      , GV.DTMilliseconds ms
      ]

class ToVLDateTime x where
  toVLDateTime :: x -> [GV.DateTime]

instance ToVLDateTime DT.Day where
  toVLDateTime = vegaLiteDate

instance ToVLDateTime DT.TimeOfDay where
  toVLDateTime = vegaLiteTime

instance ToVLDateTime DT.LocalTime where
  toVLDateTime (DT.LocalTime day timeOfDay) =
    vegaLiteDate day ++ vegaLiteTime timeOfDay
