{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Aviation.Casr.Logbook(
  module L
, Base64Alphabet(..)
, Word8_3(..)
, Word8_2(..)
, Word8_3s(..)
, Word8_3s_2(..)
, Word8_3s_1(..)
, Base64Alphabet_4(..)
, Base64Alphabet_3(..)
, Base64Alphabet_2(..)
, Base64Alphabet_4s(..)
, Base64Alphabet_4s_3(..)
, Base64Alphabet_4s_2(..)
, base64Octet
) where

import Data.Aviation.Casr.Logbook.Types as L

import Control.Lens
import Data.Bits
import Data.Word
import Data.Maybe
import Prelude

data Base64Alphabet =
  Base64_00
  | Base64_01
  | Base64_02
  | Base64_03
  | Base64_04
  | Base64_05
  | Base64_06
  | Base64_07
  | Base64_08
  | Base64_09
  | Base64_10
  | Base64_11
  | Base64_12
  | Base64_13
  | Base64_14
  | Base64_15
  | Base64_16
  | Base64_17
  | Base64_18
  | Base64_19
  | Base64_20
  | Base64_21
  | Base64_22
  | Base64_23
  | Base64_24
  | Base64_25
  | Base64_26
  | Base64_27
  | Base64_28
  | Base64_29
  | Base64_30
  | Base64_31
  | Base64_32
  | Base64_33
  | Base64_34
  | Base64_35
  | Base64_36
  | Base64_37
  | Base64_38
  | Base64_39
  | Base64_40
  | Base64_41
  | Base64_42
  | Base64_43
  | Base64_44
  | Base64_45
  | Base64_46
  | Base64_47
  | Base64_48
  | Base64_49
  | Base64_50
  | Base64_51
  | Base64_52
  | Base64_53
  | Base64_54
  | Base64_55
  | Base64_56
  | Base64_57
  | Base64_58
  | Base64_59
  | Base64_60
  | Base64_61
  | Base64_62
  | Base64_63
  deriving (Eq, Ord, Show)

data Word8_3 =
  Word8_3
    Word8
    Word8
    Word8
  deriving (Eq, Ord, Show)

data Word8_2 =
  Word8_2
    Word8
    Word8
  deriving (Eq, Ord, Show)

newtype Word8_3s =
  Word8_3s
    [Word8_3]
  deriving (Eq, Ord, Show)

makeWrapped ''Word8_3s

data Word8_3s_2 =
  Word8_3s_2
    Word8_3s
    Word8_2
  deriving (Eq, Ord, Show)

data Word8_3s_1 =
  Word8_3s_1
    Word8_3s
    Word8
  deriving (Eq, Ord, Show)

data Base64Alphabet_4 =
  Base64Alphabet_4
    Base64Alphabet
    Base64Alphabet
    Base64Alphabet
    Base64Alphabet
  deriving (Eq, Ord, Show)

data Base64Alphabet_3 =
  Base64Alphabet_3
    Base64Alphabet
    Base64Alphabet
    Base64Alphabet
  deriving (Eq, Ord, Show)

data Base64Alphabet_2 =
  Base64Alphabet_2
    Base64Alphabet
    Base64Alphabet
  deriving (Eq, Ord, Show)

newtype Base64Alphabet_4s =
  Base64Alphabet_4s
    [Base64Alphabet_4]
  deriving (Eq, Ord, Show)

makeWrapped ''Base64Alphabet_4s

data Base64Alphabet_4s_3 =
  Base64Alphabet_4s_3
    Base64Alphabet_4s
    Base64Alphabet_3
  deriving (Eq, Ord, Show)

data Base64Alphabet_4s_2 =
  Base64Alphabet_4s_2
    Base64Alphabet_4s
    Base64Alphabet_2
  deriving (Eq, Ord, Show)

class AsBase64Alphabet a where
  _Base64Alphabet ::
    Prism' a Base64Alphabet

instance AsBase64Alphabet Base64Alphabet where
  _Base64Alphabet =
    id

instance AsBase64Alphabet Char where
  _Base64Alphabet =
    prism'
    (
      \case
        Base64_00 ->
          'A'
        Base64_01 ->
          'B'
        Base64_02 ->
          'C'
        Base64_03 ->
          'D'
        Base64_04 ->
          'E'
        Base64_05 ->
          'F'
        Base64_06 ->
          'G'
        Base64_07 ->
          'H'
        Base64_08 ->
          'I'
        Base64_09 ->
          'J'
        Base64_10 ->
          'K'
        Base64_11 ->
          'L'
        Base64_12 ->
          'M'
        Base64_13 ->
          'N'
        Base64_14 ->
          'O'
        Base64_15 ->
          'P'
        Base64_16 ->
          'Q'
        Base64_17 ->
          'R'
        Base64_18 ->
          'S'
        Base64_19 ->
          'T'
        Base64_20 ->
          'U'
        Base64_21 ->
          'V'
        Base64_22 ->
          'W'
        Base64_23 ->
          'X'
        Base64_24 ->
          'Y'
        Base64_25 ->
          'Z'
        Base64_26 ->
          'a'
        Base64_27 ->
          'b'
        Base64_28 ->
          'c'
        Base64_29 ->
          'd'
        Base64_30 ->
          'e'
        Base64_31 ->
          'f'
        Base64_32 ->
          'g'
        Base64_33 ->
          'h'
        Base64_34 ->
          'i'
        Base64_35 ->
          'j'
        Base64_36 ->
          'k'
        Base64_37 ->
          'l'
        Base64_38 ->
          'm'
        Base64_39 ->
          'n'
        Base64_40 ->
          'o'
        Base64_41 ->
          'p'
        Base64_42 ->
          'q'
        Base64_43 ->
          'r'
        Base64_44 ->
          's'
        Base64_45 ->
          't'
        Base64_46 ->
          'u'
        Base64_47 ->
          'v'
        Base64_48 ->
          'w'
        Base64_49 ->
          'x'
        Base64_50 ->
          'y'
        Base64_51 ->
          'z'
        Base64_52 ->
          '0'
        Base64_53 ->
          '1'
        Base64_54 ->
          '2'
        Base64_55 ->
          '3'
        Base64_56 ->
          '4'
        Base64_57 ->
          '5'
        Base64_58 ->
          '6'
        Base64_59 ->
          '7'
        Base64_60 ->
          '8'
        Base64_61 ->
          '9'
        Base64_62 ->
          '+'
        Base64_63 ->
          '/'
    )
    (
      \case
        'A' ->
          Just Base64_00
        'B' ->
          Just Base64_01
        'C' ->
          Just Base64_02
        'D' ->
          Just Base64_03
        'E' ->
          Just Base64_04
        'F' ->
          Just Base64_05
        'G' ->
          Just Base64_06
        'H' ->
          Just Base64_07
        'I' ->
          Just Base64_08
        'J' ->
          Just Base64_09
        'K' ->
          Just Base64_10
        'L' ->
          Just Base64_11
        'M' ->
          Just Base64_12
        'N' ->
          Just Base64_13
        'O' ->
          Just Base64_14
        'P' ->
          Just Base64_15
        'Q' ->
          Just Base64_16
        'R' ->
          Just Base64_17
        'S' ->
          Just Base64_18
        'T' ->
          Just Base64_19
        'U' ->
          Just Base64_20
        'V' ->
          Just Base64_21
        'W' ->
          Just Base64_22
        'X' ->
          Just Base64_23
        'Y' ->
          Just Base64_24
        'Z' ->
          Just Base64_25
        'a' ->
          Just Base64_26
        'b' ->
          Just Base64_27
        'c' ->
          Just Base64_28
        'd' ->
          Just Base64_29
        'e' ->
          Just Base64_30
        'f' ->
          Just Base64_31
        'g' ->
          Just Base64_32
        'h' ->
          Just Base64_33
        'i' ->
          Just Base64_34
        'j' ->
          Just Base64_35
        'k' ->
          Just Base64_36
        'l' ->
          Just Base64_37
        'm' ->
          Just Base64_38
        'n' ->
          Just Base64_39
        'o' ->
          Just Base64_40
        'p' ->
          Just Base64_41
        'q' ->
          Just Base64_42
        'r' ->
          Just Base64_43
        's' ->
          Just Base64_44
        't' ->
          Just Base64_45
        'u' ->
          Just Base64_46
        'v' ->
          Just Base64_47
        'w' ->
          Just Base64_48
        'x' ->
          Just Base64_49
        'y' ->
          Just Base64_50
        'z' ->
          Just Base64_51
        '0' ->
          Just Base64_52
        '1' ->
          Just Base64_53
        '2' ->
          Just Base64_54
        '3' ->
          Just Base64_55
        '4' ->
          Just Base64_56
        '5' ->
          Just Base64_57
        '6' ->
          Just Base64_58
        '7' ->
          Just Base64_59
        '8' ->
          Just Base64_60
        '9' ->
          Just Base64_61
        '+' ->
          Just Base64_62
        '/' ->
          Just Base64_63
        _ ->
          Nothing
    )

    
instance AsBase64Alphabet Int where
  _Base64Alphabet =
    base64AlphabetOrd

instance AsBase64Alphabet Integer where
  _Base64Alphabet =
    base64AlphabetOrd

instance AsBase64Alphabet Word where
  _Base64Alphabet =
    base64AlphabetOrd

instance AsBase64Alphabet Word8 where
  _Base64Alphabet =
    base64AlphabetOrd

instance AsBase64Alphabet Word16 where
  _Base64Alphabet =
    base64AlphabetOrd

instance AsBase64Alphabet Word32 where
  _Base64Alphabet =
    base64AlphabetOrd

instance AsBase64Alphabet Word64 where
  _Base64Alphabet =
    base64AlphabetOrd

instance (Eq a, Num a) => AsBase64Alphabet (Identity a) where
  _Base64Alphabet =
    _Wrapped . base64AlphabetOrd

instance (Eq a, Num a) => AsBase64Alphabet (Const a b) where
  _Base64Alphabet =
    _Wrapped . base64AlphabetOrd

base64Octet ::
  Iso'
    Word8_3
    Base64Alphabet_4
base64Octet =
  iso
    (
      \(Word8_3 w1 w2 w3) ->
        let base64Alphabet n =
              fromMaybe Base64_63 (n ^? base64AlphabetOrd)
            w1' =
              shiftR w1 2
            w2' =
              (shiftL w1 4 .&. 63) .|. shiftR w2 4
            w3' =
              (shiftL w2 2 .&. 63) .|. shiftR w3 6
            w4' =
              w3 .&. 63
        in  Base64Alphabet_4
              (base64Alphabet w1')
              (base64Alphabet w2')
              (base64Alphabet w3')
              (base64Alphabet w4')
    )
    (
      \(Base64Alphabet_4 w1 w2 w3 w4) ->
        let w1' =
              _Base64Alphabet # w1
            w2' =
              _Base64Alphabet # w2
            w3' =
              _Base64Alphabet # w3
            w4' =
              _Base64Alphabet # w4
            w1'' =
              shiftL w1' 2 .|. shiftR w2' 4
            w2'' =
              shiftL w2' 4 .|. shiftR w3' 2
            w3'' =
              shiftL w3' 6 .|. w4'
        in  Word8_3 w1'' w2'' w3''
    )


base64Octets ::
  Iso'
    Word8_3s
    Base64Alphabet_4s
base64Octets =
  _Wrapped . cloneIso (listIso base64Octet) . _Unwrapped'

-- ?
listIso ::
  AnIso a a' b b'
  -> AnIso [a] [a'] [b] [b']
listIso i =
  withIso
    i
    (
      \j k ->
        iso
          (fmap j)
          (fmap k)
    )

-- Word8_3s <-> Base64Alphabet_4s
-- Word8_3s_2 <-> Base64Alphabet_4s_3
-- Word8_3s_1 <-> Base64Alphabet_4s_2

-- Word8_3 <-> Base64Alphabet_4

-- ByteString

---- not exported

base64AlphabetOrd ::
  (Eq a, Num a) =>
  Prism'
    a
    Base64Alphabet
base64AlphabetOrd =
  prism'
    (
      \case
        Base64_00 ->
          0
        Base64_01 ->
          1
        Base64_02 ->
          2
        Base64_03 ->
          3
        Base64_04 ->
          4
        Base64_05 ->
          5
        Base64_06 ->
          6
        Base64_07 ->
          7
        Base64_08 ->
          8
        Base64_09 ->
          9
        Base64_10 ->
          10
        Base64_11 ->
          11
        Base64_12 ->
          12
        Base64_13 ->
          13
        Base64_14 ->
          14
        Base64_15 ->
          15
        Base64_16 ->
          16
        Base64_17 ->
          17
        Base64_18 ->
          18
        Base64_19 ->
          19
        Base64_20 ->
          20
        Base64_21 ->
          21
        Base64_22 ->
          22
        Base64_23 ->
          23
        Base64_24 ->
          24
        Base64_25 ->
          25
        Base64_26 ->
          26
        Base64_27 ->
          27
        Base64_28 ->
          28
        Base64_29 ->
          29
        Base64_30 ->
          30
        Base64_31 ->
          31
        Base64_32 ->
          32
        Base64_33 ->
          33
        Base64_34 ->
          34
        Base64_35 ->
          35
        Base64_36 ->
          36
        Base64_37 ->
          37
        Base64_38 ->
          38
        Base64_39 ->
          39
        Base64_40 ->
          40
        Base64_41 ->
          41
        Base64_42 ->
          42
        Base64_43 ->
          43
        Base64_44 ->
          44
        Base64_45 ->
          45
        Base64_46 ->
          46
        Base64_47 ->
          47
        Base64_48 ->
          48
        Base64_49 ->
          49
        Base64_50 ->
          50
        Base64_51 ->
          51
        Base64_52 ->
          52
        Base64_53 ->
          53
        Base64_54 ->
          54
        Base64_55 ->
          55
        Base64_56 ->
          56
        Base64_57 ->
          57
        Base64_58 ->
          58
        Base64_59 ->
          59
        Base64_60 ->
          60
        Base64_61 ->
          61
        Base64_62 ->
          62
        Base64_63 ->
          63
    )
    (
      \case
        0 ->
          Just Base64_00
        1 ->
          Just Base64_01
        2 ->
          Just Base64_02
        3 ->
          Just Base64_03
        4 ->
          Just Base64_04
        5 ->
          Just Base64_05
        6 ->
          Just Base64_06
        7 ->
          Just Base64_07
        8 ->
          Just Base64_08
        9 ->
          Just Base64_09
        10 ->
          Just Base64_10
        11 ->
          Just Base64_11
        12 ->
          Just Base64_12
        13 ->
          Just Base64_13
        14 ->
          Just Base64_14
        15 ->
          Just Base64_15
        16 ->
          Just Base64_16
        17 ->
          Just Base64_17
        18 ->
          Just Base64_18
        19 ->
          Just Base64_19
        20 ->
          Just Base64_20
        21 ->
          Just Base64_21
        22 ->
          Just Base64_22
        23 ->
          Just Base64_23
        24 ->
          Just Base64_24
        25 ->
          Just Base64_25
        26 ->
          Just Base64_26
        27 ->
          Just Base64_27
        28 ->
          Just Base64_28
        29 ->
          Just Base64_29
        30 ->
          Just Base64_30
        31 ->
          Just Base64_31
        32 ->
          Just Base64_32
        33 ->
          Just Base64_33
        34 ->
          Just Base64_34
        35 ->
          Just Base64_35
        36 ->
          Just Base64_36
        37 ->
          Just Base64_37
        38 ->
          Just Base64_38
        39 ->
          Just Base64_39
        40 ->
          Just Base64_40
        41 ->
          Just Base64_41
        42 ->
          Just Base64_42
        43 ->
          Just Base64_43
        44 ->
          Just Base64_44
        45 ->
          Just Base64_45
        46 ->
          Just Base64_46
        47 ->
          Just Base64_47
        48 ->
          Just Base64_48
        49 ->
          Just Base64_49
        50 ->
          Just Base64_50
        51 ->
          Just Base64_51
        52 ->
          Just Base64_52
        53 ->
          Just Base64_53
        54 ->
          Just Base64_54
        55 ->
          Just Base64_55
        56 ->
          Just Base64_56
        57 ->
          Just Base64_57
        58 ->
          Just Base64_58
        59 ->
          Just Base64_59
        60 ->
          Just Base64_60
        61 ->
          Just Base64_61
        62 ->
          Just Base64_62
        63 ->
          Just Base64_63
        _ ->
          Nothing
    )
