module Main where

import System.Environment (getArgs)
import Text.ParserCombinators.Parsec
import Data.Either (lefts, rights)
import Data.Maybe (isJust, fromJust, fromMaybe)

import Text.XML.HXT.Arrow hiding (getName)
import Text.XML.HXT.Arrow.XmlArrow hiding (getName)
import Control.Arrow.ArrowList




main :: IO ()
main = 
    do args <- getArgs
       (infn:outfn:depicao:dsticao:[]) <-  if (length args /= 4) 
                                              then error "Usage: gps2pln <input> <output> <departure icao> <destination icao>"
                                              else return $ args
       convert infn outfn (depicao, dsticao)


convert :: String -> String -> (String, String) -> IO ()
convert infn outfn depdst = 
    do file <- readFile infn
       let datas        = lines file
           datas'       = dropMetaline datas
           unparsed_wps = (drop 1) $ datas'
           parsed_wps   = readWaypoints unparsed_wps
       runX $ root [] [plnFile parsed_wps depdst] >>> writeDocument [(a_indent, v_1)] outfn
       return ()
       -- return $ runX $ root [] [plnFile parsed_wps] >>> writeDocument [(a_indent, v_1)] "blub.xml"




readWaypoints :: [String] -> [Wegpunkt]
readWaypoints xs = 
    if ((length.lefts $ parse_result) > 0)
       then error ("there was a problem with a waypoint: " ++ (show problem))
       else rights parse_result
   where parse_result = map (parse pWegpunkt "") xs
         problem      = head.lefts $ parse_result

dropMetaline :: [String] -> [String]
dropMetaline x@(ml:_) = 
    if (head.words $ ml) == "FL95.de"
       then (drop 1) x
       else x


pt2arr :: ArrowXml arr => Wegpunkt -> arr XmlTree XmlTree
pt2arr wp = 
    mkelem "ATCWaypoint" [ sattr "id" (idify.getName $ wp) ] 
    [ mkelem "ATCWaypointType" [] [ txt "User" ]
    , mkelem "WorldPosition"   [] [ txt $ lla wp ]
    , mkelem "Descr"           [] [ txt (getName wp) ]
    ]    

airport2arr :: ArrowXml arr => String -> Wegpunkt -> arr XmlTree XmlTree
airport2arr icao wp = 
    mkelem "ATCWaypoint" [ sattr "id" (idify.getName $ wp) ] 
    [ mkelem "ATCWaypointType" [] [ txt "Airport" ]
    , mkelem "WorldPosition"   [] [ txt $ lla wp ]
    , mkelem "ICAO"            []
      [ mkelem "ICAOIdent" [] [ txt $ icao ] 
      ]
    ]    


plnFile :: ArrowXml arr => [Wegpunkt] -> (String, String) -> arr XmlTree XmlTree
plnFile wps (depicao, dsticao) =
    mkelem "SimBase.Document" [ sattr "Type"     "AceXML"
                              , sattr "version" "1,0"
                              ]
    [ mkelem "Descr" [] [ txt "AceXML Document" ]
    , mkelem "FlightPlan.FlightPlan" [] 
      [ mkelem "Title"             [] [ txt (depName ++ " to " ++ dstName) ]
      , mkelem "FPType"            [] [ txt "VFR" ]
      , mkelem "RouteType"         [] [ txt "VOR" ]
      , mkelem "CruisingAlt"       [] [ txt "3500" ]
      , mkelem "DepartureID"       [] [ txt $ idify depName ]
      , mkelem "DepartureLLA"      [] [ txt (lla dep) ]
      , mkelem "DestinationID"     [] [ txt $ idify dstName ]
      , mkelem "DestinationLLA"    [] [ txt (lla dst) ]
      , mkelem "Descr"             [] [ txt (depName ++ ", " ++ dstName) ]
--    , mkelem "DeparturePosition" [] [ txt "24" ]
      , mkelem "DepartureName"     [] [ txt depName ]
      , mkelem "DestinationName"   [] [ txt dstName ]
      , mkelem "AppVersion"        [] 
        [ mkelem "AppVersionMajor" [] [ txt "10" ]
        , mkelem "AppVersionBuild" [] [ txt "6172" ]
        ]
      , depArr
      , catA rest
      , dstArr
      ]
    ]
  where dep     = head $ wps
        dst     = head.reverse $ wps
        depName = getName dep
        dstName = getName dst
        wps_    = dropFstLst wps
        wp_fst  = head $ take 1 $ wps
        wp_lst  = head $ take 1 . reverse $ wps
        rest    = map pt2arr wps_
        depArr  = airport2arr depicao wp_fst
        dstArr  = airport2arr dsticao wp_lst

dropFstLst :: [a] -> [a]
dropFstLst = reverse . drop 1 . reverse . drop 1




prepZero :: Int -> Int -> String
prepZero n x = let l = length (show x)
                in  (replicate (n-l) '0') ++ (show x)

grd_, min_, sec_ :: Koordinate -> Int
grd_ (x,_,_) = x
min_ (_,x,_) = x
sec_ (_,_,x) = x

idify :: String -> String
idify = take 10

hgh_ :: Int -> String
hgh_ x = (prepZero 6 x) ++ ".00"

lla :: Wegpunkt -> String
lla d  =   "N"
       ++ (show (grd_.getBreite $ d) ++ "°")  ++ " "
       ++ (show (min_.getBreite $ d) ++ "'")  ++ " "
       ++ (show (sec_.getBreite $ d) ++ ".00" ++ "\"")
       ++ ","
       ++ "E" 
       ++ ((prepZero 3 $ grd_.getLaenge $ d) ++ "°")  ++ " "
       ++ (show (min_.getLaenge $ d) ++ "'")  ++ " "
       ++ (show (sec_.getLaenge $ d) ++ ".00" ++ "\"")
       ++ ",+"
       ++ (if (isJust.getHoehe $ d) then (hgh_.fromJust.getHoehe $ d) else (hgh_ 1))


type Grad    = Int
type Minute  = Int
type Sekunde = Int

type Koordinate = (Grad, Minute, Sekunde)
type Laenge = Koordinate
type Breite = Koordinate

data Wegpunkt = MkWegpunkt
    { getChar   :: Char
    , getName   :: String
    , getBreite :: Koordinate
    , getLaenge :: Koordinate
    , getHoehe  :: Maybe Int
    } deriving (Show)

instance Read (Wegpunkt) where
    readsPrec _ s = case (parse pWegpunkt "") s of 
                        Left _  -> []
                        Right x -> [(x, "error while parsing Wegpunkt")]
    

pWegpunkt = 
    do chr  <- pChar
       pSpacer
       name <- pName
       pSpacer
       brt  <- pBreite
       pSpacer
       lng  <- pLaenge
       pSpacer
       hgh  <- try ( do hgh  <- pHoehe
                        hgh' <- return $ (read hgh :: Int)
                        return (Just hgh')
                   )
               <|> ( do return (Nothing)
                   )
       spaces
       return (MkWegpunkt chr name brt lng hgh)

pSpacer = char '\t'
pChar   = anyChar 
pName   = many1 $ choice [alphaNum, char ' ', char '/', char '.']
pBreite = pKoordinate 'N'
pLaenge = pKoordinate 'E'
pHoehe  = many1 digit

pKoordinate c = 
    do char c
       char ' '
       grd  <- many1 digit
       grd' <- return $ (read grd :: Int)
       char ' '
       min  <- many1 digit
       min' <- return $ (read min :: Int)
       char '.'
       dec  <- many1 digit
       dec' <- return $ (read dec :: Int)
       return (grd', min', round (fromIntegral $ dec' * 6) :: Int)
