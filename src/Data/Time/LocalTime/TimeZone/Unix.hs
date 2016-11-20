module Data.Time.LocalTime.TimeZone.Unix
(
    -- * Time Zone
    getCurrentTimeZoneSeries,
    getTimeZoneSeriesForZone,
    -- * Country Information
    CountryCode,
    getCountryCodes,
    -- * Zone Information
    ZoneName,
    ZoneDescription(..),
    getZoneDescriptions,
    -- * OlsonData
    getCurrentOlsonData,
    getOlsonDataForZone,
    -- * Leap Seconds
    getLeapSecondList,
) where
{
    import Data.Char;
    import Data.Maybe;
    import Data.List;
    import System.Environment;
    import System.Directory;
    import System.FilePath.Posix;
    import Data.Time;
    import Data.Time.LocalTime.TimeZone.Series;
    import Data.Time.LocalTime.TimeZone.Olson;
    import Data.Time.Clock.LeapSeconds;


    separate :: Char -> String -> [String];
    separate sep s = let
    {
        (val,rest) = break ((==) sep) s;
        vals = case rest of
        {
            [] -> [];
            _:s' -> separate sep s';
        };
    } in val : vals;

    findPath :: [FilePath] -> IO (Maybe FilePath);
    findPath [] = return Nothing;
    findPath (path:paths) = do
    {
        exists <- doesFileExist path;
        if exists then return (Just path) else findPath paths;
    };

    -- http://www.pathname.com/fhs/pub/fhs-2.3.html#SPECIFICOPTIONS15
    -- http://askubuntu.com/questions/118015/where-is-usr-share-zoneinfo-specified
    zoneFilePath :: String -> FilePath;
    zoneFilePath filename = "/usr/share/zoneinfo/" ++ filename;

    readZoneInfoFile :: ([String] -> Maybe a) -> [String] -> IO [a];
    readZoneInfoFile toValues filenames = do
    {
        let
        {
            filepaths = fmap zoneFilePath filenames;
        };
        mfilepath <- findPath filepaths;
        filepath <- case mfilepath of
        {
            Just path -> return path;
            Nothing -> fail $ "could not find " ++ intercalate " " filepaths;
        };
        text <- readFile filepath;
        let
        {
            readLine ('#':_) = return Nothing;
            readLine "" = return Nothing;
            readLine s = case toValues $ separate '\t' s of
            {
                Just a -> return $ Just a;
                Nothing -> fail $ "unexpected line in "++filepath++": " ++ s;
            };
        };
        mstrs <- traverse readLine $ lines text;
        return $ catMaybes mstrs;
    };

    type CountryCode = String;

    -- | Get the country codes and names found in @iso3166.tab@.
    -- Note that earlier versions of zoneinfo don't include this file, in which case this will throw exception matching 'isDoesNotExistError'.
    ;
    getCountryCodes :: IO [(CountryCode,String)];
    getCountryCodes = let
    {
        toCountryCode [code,country] = Just (code,country);
        toCountryCode _ = Nothing;
    } in readZoneInfoFile toCountryCode ["iso3166.tab"];


    type ZoneName = String;

    data ZoneDescription = MkZoneDescription
    {
        zoneCountries :: [CountryCode],
        zoneLocation :: String,
        zoneName :: ZoneName,
        zoneComment :: String
    };

    -- | Get the zone descriptions found in @zone1970.tab@ (or @zone.tab@).
    ;
    getZoneDescriptions :: IO [ZoneDescription];
    getZoneDescriptions = let
    {
        toZoneDescription [countries,location,name] = Just $ MkZoneDescription
        {
            zoneCountries = separate ',' countries,
            zoneLocation = location,
            zoneName = name,
            zoneComment = ""
        };
        toZoneDescription [countries,location,name,comment] = Just $ MkZoneDescription
        {
            zoneCountries = separate ',' countries,
            zoneLocation = location,
            zoneName = name,
            zoneComment = comment
        };
        toZoneDescription _ = Nothing;
    } in readZoneInfoFile toZoneDescription ["zone1970.tab","zone.tab"];

    validZoneNamePath :: Maybe ZoneName -> Maybe FilePath;
    validZoneNamePath Nothing = Just "/etc/localtime";
    validZoneNamePath (Just name) = let
    {
        components = splitDirectories name;

        goodComponent "" = False;
        goodComponent "/" = False;
        goodComponent "." = False;
        goodComponent ".." = False;
        goodComponent comp = all goodChar comp;

        goodChar '.' = True;
        goodChar '-' = True;
        goodChar '_' = True;
        goodChar c = isAscii c && isLetter c;

        good = (length components >= 1) && all goodComponent components;
    } in if good then Just (zoneFilePath name) else Nothing;

    defaultTimeZoneSeries :: TimeZoneSeries;
    defaultTimeZoneSeries = TimeZoneSeries utc [];

    interpretOldTZ :: String -> Maybe TimeZoneSeries;
    interpretOldTZ _ = Nothing; -- TODO: interpret old-style TZ format

    -- | Get the 'TimeZoneSeries' for this 'ZoneName' (or for the system default), defaulting to UTC if the name is not found.
    ;
    getTimeZoneSeriesForZone :: Maybe ZoneName -> IO TimeZoneSeries;
    getTimeZoneSeriesForZone mname = case validZoneNamePath mname of
    {
        Just path -> do
        {
            exists <- doesFileExist path;
            if exists then getTimeZoneSeriesFromOlsonFile path else return defaultTimeZoneSeries;
        };
        Nothing -> return $ fromMaybe defaultTimeZoneSeries $ do
        {
            name <- mname;
            interpretOldTZ name;
        };
    };

    -- | Get the current 'TimeZoneSeries' (as specifed by @TZ@ env-var or else the system default).
    ;
    getCurrentTimeZoneSeries :: IO TimeZoneSeries;
    getCurrentTimeZoneSeries = do
    {
        mtzvar <- lookupEnv "TZ";
        getTimeZoneSeriesForZone mtzvar;
    };

    -- | Read the 'OlsonData' file for this 'ZoneName' (or for the system default). The usual file exceptions may be thrown.
    ;
    getOlsonDataForZone :: Maybe ZoneName -> IO OlsonData;
    getOlsonDataForZone mname = case validZoneNamePath mname of
    {
        Just path -> getOlsonFromFile path;
        Nothing -> fail $ "Not a valid ZoneInfo name: " ++ show mname;
    };

    -- | Get the current 'OlsonData' (as specifed by @TZ@ env-var or else the system default).
    ;
    getCurrentOlsonData :: IO OlsonData;
    getCurrentOlsonData = do
    {
        mtzvar <- lookupEnv "TZ";
        getOlsonDataForZone mtzvar;
    };

    -- | Get the leap-second list found in @leap-seconds.list@.
    -- Note that earlier versions of zoneinfo don't include this file, in which case this will throw exception matching 'isDoesNotExistError'.
    ;
    getLeapSecondList :: IO LeapSecondList;
    getLeapSecondList = do
    {
        let
        {
            filepath = zoneFilePath "leap-seconds.list";
        };
        text <- readFile filepath;
        case parseNISTLeapSecondList text of
        {
            Just lsl -> return lsl;
            Nothing -> fail $ "failed to parse " ++ filepath;
        };
    };
}
