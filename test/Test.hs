module Main(main) where
{
    import Data.Maybe;
    import System.Environment;
    import Test.Tasty;
    import Test.Tasty.HUnit;
    import Test.Tasty.Golden;
    import Data.Time.Clock.POSIX;
    import Data.Time.LocalTime;
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
                concat $ fmap (\(t,n) -> show (posixSecondsToUTCTime t) ++ " " ++ show n ++ "\n") $ take 26 $ lslTransitions lsl;
        }
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
            testGetLeapSecondList
        ];
    };
}
