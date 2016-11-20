-- | UNIX-specific handling of time data.
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

    -- | ISO 3166 alpha-2 country code (i.e., two capital letters).
    ;
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


    -- | Name of a zone, e.g. \"America/Los_Angeles\".
    ;
    type ZoneName = String;

    -- | Information about a zone.
    ;
    data ZoneDescription = MkZoneDescription
    {
        -- | The countries that overlap the zone.
        zoneCountries :: [CountryCode],
        -- | (Latitude, longitude) of principal location in degrees, + is north, east.
        zoneLocation :: (Rational,Rational),
        -- | Zone name.
        zoneName :: ZoneName,
        -- | Comments; present if and only if a country has multiple zones.
        zoneComment :: String
    };

    getSign :: Char -> Maybe Rational;
    getSign '+' = Just 1;
    getSign '-' = Just (-1);
    getSign _ = Nothing;

    getDigit :: Char -> Maybe Rational;
    getDigit c | c < '0' = Nothing;
    getDigit c | c > '9' = Nothing;
    getDigit c = Just $ toRational $ (fromEnum c) - (fromEnum '0');

    getDigit2 :: Char -> Char -> Maybe Rational;
    getDigit2 c1 c2 = do
    {
        r1 <- getDigit c1;
        r2 <- getDigit c2;
        return $ r1 * 10 + r2;
    };

    getDigit3 :: Char -> Char -> Char -> Maybe Rational;
    getDigit3 c1 c2 c3 = do
    {
        r1 <- getDigit2 c1 c2;
        r2 <- getDigit c3;
        return $ r1 * 10 + r2;
    };

    parseISO6709 :: String -> Maybe (Rational,Rational);
    parseISO6709 [xsn,xd1,xd2,xm1,xm2,ysn,yd1,yd2,yd3,ym1,ym2] = do
    {
        xsgn <- getSign xsn;
        xdeg <- getDigit2 xd1 xd2;
        xmin <- getDigit2 xm1 xm2;
        ysgn <- getSign ysn;
        ydeg <- getDigit3 yd1 yd2 yd3;
        ymin <- getDigit2 ym1 ym2;
        return (xsgn * (xdeg + xmin / 60),ysgn * (ydeg + ymin / 60));
    };
    parseISO6709 [xsn,xd1,xd2,xm1,xm2,xs1,xs2,ysn,yd1,yd2,yd3,ym1,ym2,ys1,ys2] = do
    {
        xsgn <- getSign xsn;
        xdeg <- getDigit2 xd1 xd2;
        xmin <- getDigit2 xm1 xm2;
        xsec <- getDigit2 xs1 xs2;
        ysgn <- getSign ysn;
        ydeg <- getDigit3 yd1 yd2 yd3;
        ymin <- getDigit2 ym1 ym2;
        ysec <- getDigit2 ys1 ys2;
        return (xsgn * (xdeg + xmin / 60 + xsec / 3600),ysgn * (ydeg + ymin / 60 + ysec / 3600));
    };
    parseISO6709 _ = Nothing;

    getISO6709 :: String -> (Rational,Rational);
    getISO6709 location = case parseISO6709 location of
    {
        Just loc -> loc;
        Nothing -> error $ "bad IS06709: " ++ location;
    };

    -- | Get the zone descriptions found in @zone1970.tab@ (or @zone.tab@).
    ;
    getZoneDescriptions :: IO [ZoneDescription];
    getZoneDescriptions = let
    {
        toZoneDescription [countries,location,name] = Just $ MkZoneDescription
        {
            zoneCountries = separate ',' countries,
            zoneLocation = getISO6709 location,
            zoneName = name,
            zoneComment = ""
        };
        toZoneDescription [countries,location,name,comment] = Just $ MkZoneDescription
        {
            zoneCountries = separate ',' countries,
            zoneLocation = getISO6709 location,
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
