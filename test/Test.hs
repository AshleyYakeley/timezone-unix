module Main(main) where
{
    import Data.Maybe;
    import Data.Foldable;
    import Control.Exception;
    import System.Environment;
    import System.Directory;
    import Test.Tasty;
    import Test.Tasty.HUnit;
    import Test.Tasty.Golden;
    import Data.Time;
    import Data.Time.Clock.LeapSeconds;
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
        forM_ zones $ \MkZoneDescription{zoneLocation=(lat,long,acc),..} -> do
        {
            assertBool "zoneCountries length" $ length zoneCountries >= 1;
            forM_ zoneCountries $ \zc -> assertEqual "zoneCountries member length" 2 $ length zc;
            _ <- evaluate lat;
            _ <- evaluate long;
            _ <- evaluate acc;
            _ <- evaluate zoneName;
            _ <- evaluate zoneComment;
            return ();
        }
    };

    testGetTimeZoneSeries :: Maybe TimeZoneSpec -> TestTree;
    testGetTimeZoneSeries mname = testCase "getTimeZoneSeriesForZone" $ getTimeZoneSeriesForZone mname >> return ();

    testGetOlsonData :: Maybe TimeZoneSpec -> TestTree;
    testGetOlsonData mname = testCase "getOlsonDataForZone" $ getOlsonDataForZone mname >> return ();

    testLoadZones :: [TimeZoneSpec] -> TestTree;
    testLoadZones names = testGroup "load" $ fmap (\mname -> testGroup (fromMaybe "default" mname)
        [
            testGetTimeZoneSeries mname,
            testGetOlsonData mname
        ]) (Nothing : (fmap Just names));

    testUnknownZoneName :: TestTree;
    testUnknownZoneName = testCase "unknown TimeZoneSpec" $ do
    {
        setEnv "TZ" "unknown";
        tzs <- getCurrentTimeZoneSeries;
        assertEqual "default TimeZoneSpec" (TimeZoneSeries utc []) tzs;
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

    main :: IO ();
    main = do
    {
        ls_exists <- doesFileExist "/usr/share/zoneinfo/leap-seconds.list"; -- workaround for Travis, which doesn't have this file
        zones <- getZoneDescriptions;
        let
        {
            names = fmap zoneName zones;
        };
        defaultMain $ testGroup "timezone-unix" $
        [
            testCountryCodes,
            testZoneDescriptions zones,
            testLoadZones names,
            testUnknownZoneName
        ] ++
        if ls_exists then [testGetLeapSecondList] else [];
    };
}
