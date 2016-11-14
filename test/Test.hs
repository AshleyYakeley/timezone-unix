module Main(main) where
{
    import Data.Maybe;
    import System.Environment;
    import Test.Tasty;
    import Test.Tasty.HUnit;
    import Test.Tasty.Golden;
    import Data.Time;
    import Data.Time.Clock.TAI hiding (utcDayLength,utcToTAITime,taiToUTCTime);
    import Data.Time.LocalTime.TimeZone.Series;
    import Data.Time.LocalTime.TimeZone.Unix;


    testCountryCodes :: TestTree;
    testCountryCodes = testCase "getCountryCodes" $ do
    {
        codes <- getCountryCodes;
        assertBool "at least 200 countries" $ length codes >= 200;
    };

    testZoneDescriptions :: [ZoneDescription] -> TestTree;
    testZoneDescriptions zones = testCase "getZoneDescriptions" $ do
    {
        assertBool "at least 300 zones" $ length zones >= 300;
    };

    testGetTimeZoneSeries :: Maybe ZoneName -> TestTree;
    testGetTimeZoneSeries mname = testCase "getTimeZoneSeriesForZone" $ getTimeZoneSeriesForZone mname >> return ();

    testGetOlsonData :: Maybe ZoneName -> TestTree;
    testGetOlsonData mname = testCase "getOlsonDataForZone" $ getOlsonDataForZone mname >> return ();

    testLoadZones :: [ZoneName] -> TestTree;
    testLoadZones names = testGroup "load" $ fmap (\mname -> testGroup (fromMaybe "default" mname)
        [
            testGetTimeZoneSeries mname,
            testGetOlsonData mname
        ]) (Nothing : (fmap Just names));

    testUnknownZoneName :: TestTree;
    testUnknownZoneName = testCase "unknown ZoneName" $ do
    {
        setEnv "TZ" "unknown";
        tzs <- getCurrentTimeZoneSeries;
        assertEqual "default ZoneName" (TimeZoneSeries utc []) tzs;
    };

    testGetLeapSecondList :: TestTree;
    testGetLeapSecondList = testGroup "getLeapSecondList" [
        testCase "values" $ do
        {
            lsl <- getLeapSecondList;
            assertBool "expiration is later than version" $ lslExpiration lsl > lslVersion lsl;
        },
        goldenVsFile "leap-seconds" "test/leap-seconds.ref" "test/leap-seconds.out" $ do
        {
            lsl <- getLeapSecondList;
            -- just look at the first 26, since the list will grow.
            writeBinaryFile "test/leap-seconds.out" $
                concat $ fmap (\(d,n) -> show d ++ " " ++ show n ++ "\n") $ take 26 $ lslTransitions lsl;
        }
        ];

    sampleLeapSecondMap :: LeapSecondMap Maybe;
    sampleLeapSecondMap = leapSecondListToMap $ MkLeapSecondList
    {
        lslVersion = fromGregorian 1974 1 1,
        lslExpiration = fromGregorian 1975 1 1,
        lslTransitions =
        [
            (fromGregorian 1972 1 1,10),
            (fromGregorian 1972 7 1,11)
        ]
    };

    assertJust :: Maybe a -> IO a;
    assertJust (Just a) = return a;
    assertJust Nothing = do
    {
        assertFailure "Nothing";
        return undefined;
    };

    testLeapSecondTransition :: TestTree;
    testLeapSecondTransition = testGroup "leap second transition" $ let
    {
        dayA = fromGregorian 1972 6 30;
        dayB = fromGregorian 1972 7 1;

        utcTime1 = UTCTime dayA 86399;
        utcTime2 = UTCTime dayA 86400;
        utcTime3 = UTCTime dayB 0;

        mAbsTime1 = utcToTAITime sampleLeapSecondMap utcTime1;
        mAbsTime2 = utcToTAITime sampleLeapSecondMap utcTime2;
        mAbsTime3 = utcToTAITime sampleLeapSecondMap utcTime3;
    } in
    [
        testCase "mapping" $ do
        {
            assertEqual "dayA" (Just 10) $ sampleLeapSecondMap dayA;
            assertEqual "dayB" (Just 11) $ sampleLeapSecondMap dayB;
        },
        testCase "day length" $ do
        {
            assertEqual "dayA" (Just 86401) $ utcDayLength sampleLeapSecondMap dayA;
            assertEqual "dayB" (Just 86400) $ utcDayLength sampleLeapSecondMap dayB;
        },
        testCase "differences" $ do
        {
            absTime1 <- assertJust mAbsTime1;
            absTime2 <- assertJust mAbsTime2;
            absTime3 <- assertJust mAbsTime3;
            assertEqual "absTime2 - absTime1" 1 $ diffAbsoluteTime absTime2 absTime1;
            assertEqual "absTime3 - absTime2" 1 $ diffAbsoluteTime absTime3 absTime2;
        },
        testGroup "round-trip"
        [
            testCase "1" $ do
            {
                absTime <- assertJust mAbsTime1;
                utcTime <- assertJust $ taiToUTCTime sampleLeapSecondMap absTime;
                assertEqual "round-trip" utcTime1 utcTime;
            },
            testCase "2" $ do
            {
                absTime <- assertJust mAbsTime2;
                utcTime <- assertJust $ taiToUTCTime sampleLeapSecondMap absTime;
                assertEqual "round-trip" utcTime2 utcTime;
            },
            testCase "3" $ do
            {
                absTime <- assertJust mAbsTime3;
                utcTime <- assertJust $ taiToUTCTime sampleLeapSecondMap absTime;
                assertEqual "round-trip" utcTime3 utcTime;
            }
        ]
    ];

    main :: IO ();
    main = do
    {
        zones <- getZoneDescriptions;
        let
        {
            names = fmap zoneName zones;
        };
        defaultMain $ testGroup "timezone-unix"
        [
            testCountryCodes,
            testZoneDescriptions zones,
            testLoadZones names,
            testUnknownZoneName,
            testGetLeapSecondList,
            testLeapSecondTransition
        ];
    };
}
