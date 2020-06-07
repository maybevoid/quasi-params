module Test.QuasiParam.Stress where

import Test.Tasty
import Test.Tasty.HUnit

import QuasiParam.Name

tests :: TestTree
tests = testGroup "Quasi parameters stree tests"
  [ testStress
  ]

getManyParams
  :: ( Param "param01" String
     , Param "param02" String
     , Param "param03" String
     , Param "param04" String
     , Param "param05" String
     , Param "param06" String
     , Param "param07" String
     , Param "param08" String
     , Param "param09" String
     , Param "param10" String
     , Param "param11" String
     , Param "param12" String
     , Param "param13" String
     , Param "param14" String
     , Param "param15" String
     , Param "param16" String
     , Param "param17" String
     , Param "param18" String
     , Param "param19" String
     , Param "param20" String
     )
  => [ String ]
getManyParams =
  [ captureParam @"param01"
  , captureParam @"param02"
  , captureParam @"param03"
  , captureParam @"param04"
  , captureParam @"param05"
  , captureParam @"param06"
  , captureParam @"param07"
  , captureParam @"param08"
  , captureParam @"param09"
  , captureParam @"param10"
  , captureParam @"param11"
  , captureParam @"param12"
  , captureParam @"param13"
  , captureParam @"param14"
  , captureParam @"param15"
  , captureParam @"param16"
  , captureParam @"param17"
  , captureParam @"param18"
  , captureParam @"param19"
  , captureParam @"param20"
  ]

testStress :: TestTree
testStress = testCase "test many parameters" $
  withParam @"param01" "value01" $
  withParam @"param02" "value02" $
  withParam @"param03" "value03" $
  withParam @"param04" "value04" $
  withParam @"param05" "value05" $
  withParam @"param06" "value06" $
  withParam @"param07" "value07" $
  withParam @"param08" "value08" $
  withParam @"param09" "value09" $
  withParam @"param10" "value10" $
  withParam @"param11" "value11" $
  withParam @"param12" "value12" $
  withParam @"param13" "value13" $
  withParam @"param14" "value14" $
  withParam @"param15" "value15" $
  withParam @"param16" "value16" $
  withParam @"param17" "value17" $
  withParam @"param18" "value18" $
  withParam @"param19" "value19" $
  withParam @"param20" "value20" $
    assertEqual
      "should be able to get back parameters"
      getManyParams
      [ "value01"
      , "value02"
      , "value03"
      , "value04"
      , "value05"
      , "value06"
      , "value07"
      , "value08"
      , "value09"
      , "value10"
      , "value11"
      , "value12"
      , "value13"
      , "value14"
      , "value15"
      , "value16"
      , "value17"
      , "value18"
      , "value19"
      , "value20"
      ]
