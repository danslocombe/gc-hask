{-# LANGUAGE TypeApplications #-}

module PonyTypeSafeExamples where

import PonyTypes
import PonyTypeSafe
import Data.Proxy

e :: Env '[ '( 'Class "A", '[ '( 'Iso, 'Class "A" ) ] )
          , '( 'Class "B", '[ '( 'Iso, 'Class "A" ) ] )
          , '( 'Class "C", '[] ) 
          ]
e = Env

fieldsA :: Fields '[ '( 'Iso , 'Class "A") ]
fieldsA = (ObjectDescr2 1 1) `FieldCons` EmpFields

fieldsB :: Fields '[ '( 'Iso , 'Class "A" )
                   , '( 'Val , 'Class "B" )
                   , '( 'Iso , 'Class "C" )
                   ]
fieldsB = (ObjectDescr2 1 1) `FieldCons` (Null2 `FieldCons` (Null2 `FieldCons` EmpFields))



as0 = Actor2 1 fieldsB `ActStoreCons` EmpActStore
os0 = Object2 1 1 fieldsA `ObjStoreCons` (Object2 1 2 EmpFields `ObjStoreCons` EmpObjStore)
cfg0 = Config2 e as0 os0


-- Read field 0 from Actor 0
ref0 :: ObjectDescr2 ('Class "A")
ref0 = readActFieldConfig (Proxy @0) (Proxy @0) cfg0

-- Does not compile as is not a reference to the correct class
-- read1 :: ObjectDescr2 ('Class "B")
-- read1 = readActFieldConfig (Proxy @0) (Proxy @0) cfg0

-- Even though the described object does not exist it is fine to make this assignment
-- as the types match
ref1 = ObjectDescr2 2 1 :: ObjectDescr2 ('Class "A")
cfg1 = modifyActFieldConfig (Proxy @0) (Proxy @0) ref1 cfg0

-- This is not ok, Can't assign reference of class b to field of class a
-- ref1' = ObjectDescr2 2 1 :: ObjectDescr2 ('Class "B")
-- cfg1' = modifyActFieldConfig (Proxy @0) (Proxy @0) ref1'  cfg0

-- Field 1 has Val capability so we can't write to it
-- _ = modifyActFieldConfig (Proxy @0) (Proxy @1) Null2 cfg0

pathA :: Path2 0 '[0]
pathA = SinglePath

ref2 :: Maybe (ObjectDescr2 ('Class "A"))
ref2 = readPathConfig pathA cfg0 

-- This is not ok (wrong classes)
-- ref2' :: Maybe (ObjectDescr2 ('Class "B"))
-- ref2' = readPathConfig pathA cfg0 

pathB :: Path2 0 '[0, 0]
pathB = ChainPath SinglePath

-- This is ok as Class A has a reference to another class A
_ = readPathConfig pathB cfg0

-- So this is too
pathB' :: Path2 0 '[0, 0, 0, 0, 0]
pathB' = ChainPath $ ChainPath $ ChainPath $ ChainPath SinglePath
_ = readPathConfig pathB' cfg0

pathC :: Path2 0 '[2, 0]
pathC = ChainPath SinglePath

-- Class C doesnt have any fields so we cant do this
-- _ = readPathConfig pathD cfg1

-- :t cfg2 ~ cfg0 as we dont change the types of any fields only values
cfg2 = modifyPathConfig pathA Null2 cfg0

-- Can't assign reference to an object of class B to field of type A
-- ref3 :: ObjectDescr2 ('Class "B")
-- ref3 = Null2
-- cfg3 = modifyPathConfig pathA ref3 cfg0

pathD :: Path2 0 '[1, 0]
pathD = ChainPath SinglePath

-- We can't write through a val reference
-- _ = modifyPathConfig pathD Null2 cfg0
