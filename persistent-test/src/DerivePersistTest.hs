{-# LANGUAGE UndecidableInstances, DeriveGeneric #-} -- FIXME

module DerivePersistTest where

import Database.Persist.TH (mkDeleteCascade, derivePersist, persistLowerCase, stripEntityNamePrefix, DeriveEntityDef(..), DeriveFieldDef(..), DeriveForeignKey(..), mkDeriveEntityDef, mkDeriveFieldDef)
import Database.Persist.Quasi (lowerCaseSettings)
import Data.Maybe (isJust)

import Init
import Debug.Trace

data Plain = Plain {
  plainName :: String,
  plainAge :: Int
} deriving (Eq, Show)

derivePersist persistSettings {mpsGeneric=False, mpsRecordFieldToHaskellName = stripEntityNamePrefix} lowerCaseSettings [
  (mkDeriveEntityDef 'Plain) {
    deriveFields = [(mkDeriveFieldDef 'plainName) {
      sqlNameOverride=Just "sql_name"
      }]
  }]

data PlainPrimary = PlainPrimary {
  plainPrimaryName :: String,
  plainPrimaryRef :: PlainId
} deriving (Eq, Show)
derivePersist persistSettings {mpsGeneric=False, mpsRecordFieldToHaskellName = stripEntityNamePrefix} lowerCaseSettings [
  mkDeriveEntityDef 'PlainPrimary
  ]

newtype SubType = SubType {
  menuObject :: [MenuObject]
} deriving (Eq, Show)

newtype MenuObject = MenuObject {
  sub :: Maybe SubType
} deriving (Eq, Show)

derivePersist persistSettings {mpsGeneric=False, mpsRecordFieldToHaskellName = stripEntityNamePrefix} lowerCaseSettings [
  mkDeriveEntityDef 'SubType,
  mkDeriveEntityDef 'MenuObject
  ]

data TestParent2 = TestParent2 {
  testParentName ::  String
  , testParentName2 :: String
  , testParentAge :: Int
  , testParentExtra44 :: String
  } deriving (Eq, Show)

data TestChild2 = TestChild2 {
  testChildName :: String
  , testChildName2 :: String
  , testChildAge :: Int
  , testChildExtra4 :: String
} deriving (Eq, Show)

derivePersist persistSettings {mpsGeneric=False, mpsRecordFieldToHaskellName = stripEntityNamePrefix} lowerCaseSettings [
    (mkDeriveEntityDef 'TestParent2) {
      primaryId = Just $ Right ['testParentName, 'testParentName2, 'testParentAge]
    },
    (mkDeriveEntityDef 'TestChild2) {
      foreignKeys = [DeriveForeignKey 'TestParent2 "fkparent" ['testChildName, 'testChildName2, 'testChildAge] Nothing]
    }
  ]

derivePersistMigrate = do
  migrate [] (entityDef (Proxy :: Proxy Plain))
  migrate [] (entityDef (Proxy :: Proxy PlainPrimary))
  migrate [] (entityDef (Proxy :: Proxy SubType))
  migrate [] (entityDef (Proxy :: Proxy MenuObject))
  migrate [] (entityDef (Proxy :: Proxy TestParent2))
  migrate [] (entityDef (Proxy :: Proxy TestChild2))

specsWith :: (MonadIO m, MonadFail m) => RunDb SqlBackend m -> Spec
specsWith runDb = describe "derivePersist" $
  describe "primary keys" $ do
    let p1 = Plain "a" 1
    it "Supports basic operations" $ runDb $ do
      k <- insert p1
      p1' <- get k
      Just p1 @== p1'
    
    it "Creates fields" $ runDb $ do
      k <- insert p1
      p1' <- selectFirst [PlainName ==. "a"] []
      Just (Entity k p1) @== p1'

    it "Supports primary key" $ runDb $ do
      k <- insert p1
      let p = PlainPrimary "a" k
      k' <- insert p
      p' <- get k'
      Just p @== p'

    it "Supports recursive" $ runDb $ do
      let m1 = MenuObject $ Just $ SubType []
      let m2 = MenuObject $ Just $ SubType [m1]
      let m3 = MenuObject $ Just $ SubType [m2]
      k3 <- insert m3
      m3' <- get k3
      m3' @== Just m3
      res <- rawSql "SELECT * FROM menu_object" []
      liftIO $ print (res :: [(Single Text, Single Text)])
    
    it "Supports composite keys" $ runDb $ do
      -- copy from CompositeTest
      let p1 = TestParent2 "a1" "b1" 11 "p1"
      let p2 = TestParent2 "a2" "b2" 22 "p2"
      let c1 = TestChild2 "a1" "b1" 11 "c1"
      kp1 <- insert p1
      insert_ p2
      kc1 <- insert c1
      mc <- get kc1
      isJust mc @== True
      let Just c11 = mc
      c1 @== c11
      testChild2Fkparent c11 @== kp1
