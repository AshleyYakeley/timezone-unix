module Main(main) where
{
    import System.Environment;
    import Test.Tasty;
    import Test.Tasty.HUnit;
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

    testGetTimeZoneSeries :: ZoneName -> TestTree;
    testGetTimeZoneSeries name = testCase "getTimeZoneSeriesForZone" $ getTimeZoneSeriesForZone name >> return ();

    testGetOlsonData :: ZoneName -> TestTree;
    testGetOlsonData name = testCase "getOlsonDataForZone" $ getOlsonDataForZone name >> return ();

    testLoadZones :: [ZoneName] -> TestTree;
    testLoadZones names = testGroup "load" $ fmap (\name -> testGroup name
        [
            testGetTimeZoneSeries name,
            testGetOlsonData name
        ]) names;

    testGetCurrentZoneName :: [ZoneName] -> TestTree;
    testGetCurrentZoneName names = testCase "getCurrentZoneName" $ do
    {
        unsetEnv "TZ";
        name <- getCurrentZoneName;
        assertBool "default name found" $ elem name names;
    };

    testUnknownZoneName :: TestTree;
    testUnknownZoneName = testCase "unknown ZoneName" $ do
    {
        setEnv "TZ" "unknown";
        tzs <- getCurrentTimeZoneSeries;
        assertEqual "default ZoneName" (TimeZoneSeries utc []) tzs;
    };

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
            testGetCurrentZoneName names,
            testUnknownZoneName
        ];
    };
}
