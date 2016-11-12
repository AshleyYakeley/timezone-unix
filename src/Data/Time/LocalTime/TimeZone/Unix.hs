module Data.Time.LocalTime.TimeZone.Unix
(
    -- * Time Zones
    getCurrentTimeZoneSeries,
    ZoneName,
    getCurrentZoneName,
    getTimeZoneSeriesForZone,
    -- * Other information
    CountryCode,
    getCountryCodes,
    ZoneDescription(..),
    getZoneDescriptions,
    getOlsonDataForZone,
) where
{
    import Data.Char;
    import Data.Maybe;
    import Data.List;
    import System.Environment;
    import System.Directory;
    import System.FilePath.Posix;
    import System.Posix.Files;
    import Data.Time.LocalTime;
    import Data.Time.LocalTime.TimeZone.Series;
    import Data.Time.LocalTime.TimeZone.Olson;


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

    -- | Get the country codes and names found in @\/usr\/share\/zoneinfo\/iso3166.tab@
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

    -- | Get the zone descriptions found in @\/usr\/shar\e/zoneinfo\/zone1970.tab@ (or @zone.tab@).
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

    validZoneNamePath :: ZoneName -> Maybe FilePath;
    validZoneNamePath name = let
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

    -- | Get the 'TimeZoneSeries' for this 'ZoneName', defaulting to UTC if the name is not found.
    ;
    getTimeZoneSeriesForZone :: ZoneName -> IO TimeZoneSeries;
    getTimeZoneSeriesForZone name = case validZoneNamePath name of
    {
        Just path -> do
        {
            exists <- doesFileExist path;
            if exists then getTimeZoneSeriesFromOlsonFile path else return defaultTimeZoneSeries;
        };
        Nothing -> return defaultTimeZoneSeries; -- TODO: interpret old-style TZ format
    };

    -- | Read the 'OlsonData' file for this 'ZoneName'. The usual file exceptions may be thrown.
    ;
    getOlsonDataForZone :: ZoneName -> IO OlsonData;
    getOlsonDataForZone name = case validZoneNamePath name of
    {
        Just path -> getOlsonFromFile path;
        Nothing -> fail $ "Not a valid ZoneInfo name: " ++ name;
    };

    getSystemDefaultZoneName :: IO ZoneName;
    getSystemDefaultZoneName = do
    {
        -- https://www.freedesktop.org/software/systemd/man/localtime.html
        lpath <- readSymbolicLink "/etc/localtime";
        let
        {
            abspath = "/etc" </> lpath;
            name = makeRelative "/usr/share/zoneinfo" abspath;
        };
        return name;
    };

    -- | Get the current 'ZoneName' (in @TZ@ env-var or else the system default specified by the @\/etc\/localtime@ symlink)
    ;
    getCurrentZoneName :: IO ZoneName;
    getCurrentZoneName = do
    {
        mtzvar <- lookupEnv "TZ";
        case mtzvar of
        {
            Just tzvar -> return tzvar;
            Nothing -> getSystemDefaultZoneName;
        };
    };

    -- | Get the current 'TimeZoneSeries' (as specifed by @TZ@ env-var or else the system default).
    ;
    getCurrentTimeZoneSeries :: IO TimeZoneSeries;
    getCurrentTimeZoneSeries = do
    {
        name <- getCurrentZoneName;
        getTimeZoneSeriesForZone name;
    };
}
