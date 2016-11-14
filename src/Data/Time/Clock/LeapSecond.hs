module Data.Time.Clock.LeapSecond
(
    LeapSecondMap,
    utcDayLength,
    utcToTAITime,
    taiToUTCTime,
) where
import Data.Fixed
import Data.Time
import Data.Time.Clock.TAI hiding (utcDayLength,utcToTAITime,taiToUTCTime)


type LeapSecondMap m = Day -> m Int

utcDayLength :: Monad m => LeapSecondMap m -> Day -> m DiffTime
utcDayLength lsmap day = do
    i0 <- lsmap day
    i1 <- lsmap $ addDays 1 day
    return $ realToFrac (86400 + i1 - i0)

dayStart :: Monad m => LeapSecondMap m -> Day -> m AbsoluteTime
dayStart lsmap day = do
    i <- lsmap day
    return $ addAbsoluteTime (realToFrac $ (toModifiedJulianDay day) * 86400 + toInteger i) taiEpoch

utcToTAITime :: Monad m => LeapSecondMap m -> UTCTime -> m AbsoluteTime
utcToTAITime lsmap (UTCTime day dtime) = do
    t <- dayStart lsmap day
    return $ addAbsoluteTime dtime t

taiToUTCTime :: Monad m => LeapSecondMap m -> AbsoluteTime -> m UTCTime
taiToUTCTime lsmap abstime = let
    stable day = do
        dayt <- dayStart lsmap day
        len <- utcDayLength lsmap day
        let
            dtime = diffAbsoluteTime abstime dayt
            day' = addDays (div' dtime len) day
        if day == day' then return (UTCTime day dtime) else stable day'
    in stable $ ModifiedJulianDay $ div' (diffAbsoluteTime abstime taiEpoch) 86400
