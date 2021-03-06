{-
Java class file parser.

Implementation is based on the format specification from
"Tim Lindholm, Frank Yellin (1999). The Java Virtual Machine Specification (Second Edition ed.). Prentice Hall."

which is available for free here:
http://java.sun.com/docs/books/jvms/second_edition/html/ClassFile.doc.html

-}

module Parse where

import Data.Binary
import Data.Binary.Get
import Data.Word
import Control.Monad
import qualified Data.ByteString          as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Lazy     as L

{-
    ClassFile {
    	u4 magic;
    	u2 minor_version;
    	u2 major_version;
    	u2 constant_pool_count;
    	cp_info constant_pool[constant_pool_count-1];
    	u2 access_flags;
    	u2 this_class;
    	u2 super_class;
    	u2 interfaces_count;
    	u2 interfaces[interfaces_count];
    	u2 fields_count;
    	field_info fields[fields_count];
    	u2 methods_count;
    	method_info methods[methods_count];
    	u2 attributes_count;
    	attribute_info attributes[attributes_count];
    }
-}

getWord16 :: Get Word16
getWord16 = getWord16be

getWord32 :: Get Word32
getWord32 = getWord32be

data JavaClass = JavaClass {
    classMinorVersion :: Word16
  , classMajorVersion :: MajorVersion
  , classConstantPoolCount :: Word16
  } deriving Show

getJavaClass :: Get JavaClass
getJavaClass = do
  checkClassMagic
  minorVersion   <- getMinorVersion
  majorVersion   <- getMajorVersion
  constPoolCount <- getConstantsPoolCount
  return $ JavaClass minorVersion majorVersion constPoolCount

checkClassMagic :: Get ()
checkClassMagic = do
  magic <- getWord32
  if magic /= 0xCAFEBABE then
    fail "Invalid magic number for Java class"
    else
    return ()

getMinorVersion :: Get Word16
getMinorVersion = getWord16 >>= return

{-
J2SE 7.0 = 51 (0x33 hex),
J2SE 6.0 = 50 (0x32 hex),
J2SE 5.0 = 49 (0x31 hex),
JDK 1.4 = 48 (0x30 hex),
JDK 1.3 = 47 (0x2F hex),
JDK 1.2 = 46 (0x2E hex),
JDK 1.1 = 45 (0x2D hex).
-}

data MajorVersion
  = J2SE_7_0
  | J2SE_6_0
  | J2SE_5_0
  | JDK_1_4
  | JDK_1_3
  | JDK_1_2
  | JDK_1_1
  deriving (Eq, Show)

getMajorVersion :: Get MajorVersion
getMajorVersion = do
    version <- getWord16be
    return $ convert version
    where convert 51 = J2SE_7_0
          convert 50 = J2SE_6_0
          convert 49 = J2SE_5_0
          convert 48 = JDK_1_4
          convert 47 = JDK_1_3
          convert 46 = JDK_1_2
          convert 45 = JDK_1_1

getConstantsPoolCount :: Get Word16
getConstantsPoolCount = getWord16 >>= return

getConstantsPool = undefined

data Tag
  = TAG_STRING
  | TAG_INTEGER
  | TAG_FLOAT
  | TAG_LONG
  | TAG_DOUBLE
  | TAG_CLASS_REF
  | TAG_STRING_REF
  | TAG_FIELD_REF
  | TAG_METHOD_REF
  | TAG_INTERFACE_REF
  | TAG_NAME_AND_TYPE

getTag :: Get Tag
getTag = do
  tag <- getWord8
  return $ convert tag
  where convert 1 = TAG_STRING
        convert 3 = TAG_INTEGER
        convert 4 = TAG_FLOAT
        convert 5 = TAG_LONG
        convert 6 = TAG_DOUBLE
        convert 7 = TAG_CLASS_REF
        convert 8 = TAG_STRING_REF
        convert 9 = TAG_FIELD_REF
        convert 10 = TAG_METHOD_REF
        convert 11 = TAG_INTERFACE_REF
        convert 12 = TAG_NAME_AND_TYPE

parse :: B.ByteString -> JavaClass
parse b = runGet getJavaClass $ L.fromChunks [b]

