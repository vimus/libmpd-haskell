module Displayable (Displayable(..)) where

import Network.MPD.Utils
import Network.MPD.Parse

-- | A uniform interface for types that
-- can be turned into raw responses
class Displayable a where
    empty   :: a             -- ^ An empty instance
    display :: a -> String   -- ^ Transform instantiated object to a
                             --   string

instance Displayable Count where
    empty = Count { cSongs = 0, cPlaytime = 0 }
    display s = unlines $
        ["songs: "    ++ show (cSongs s)
        ,"playtime: " ++ show (cPlaytime s)]

instance Displayable Device where
    empty = Device 0 "" False
    display d = unlines $
        ["outputid: "      ++ show (dOutputID d)
        ,"outputname: "    ++ dOutputName d
        ,"outputenabled: " ++ showBool (dOutputEnabled d)]

instance Displayable Song where
    empty = Song { sgArtist = "", sgAlbum = "", sgTitle = "", sgFilePath = ""
                 , sgGenre  = "", sgName  = "", sgComposer = ""
                 , sgPerformer = "", sgLength = 0, sgDate = 0
                 , sgTrack = (0,0), sgDisc = (0,0), sgIndex = Nothing }
    display s = unlines $
        ["file: "      ++ sgFilePath s
        ,"Artist: "    ++ sgArtist s
        ,"Album: "     ++ sgAlbum s
        ,"Title: "     ++ sgTitle s
        ,"Genre: "     ++ sgGenre s
        ,"Name: "      ++ sgName s
        ,"Composer: "  ++ sgComposer s
        ,"Performer: " ++ sgPerformer s
        ,"Date: "      ++ show (sgDate s)
        ,"Track: "     ++ (let (x,y) = sgTrack s in show x++"/"++show y)
        ,"Disc: "      ++ (let (x,y) = sgDisc  s in show x++"/"++show y)
        ,"Time: "      ++ show (sgLength s)]
        ++ maybe [] (\x -> [case x of Pos n -> "Pos: " ++ show n
                                      ID  n -> "Id: "  ++ show n]) (sgIndex s)

instance Displayable Stats where
    empty = Stats { stsArtists = 0, stsAlbums = 0, stsSongs = 0, stsUptime = 0
                  , stsPlaytime = 0, stsDbPlaytime = 0, stsDbUpdate = 0 }
    display s = unlines $
        ["artists: " ++ show (stsArtists s)
        ,"albums: " ++ show (stsAlbums s)
        ,"songs: " ++ show (stsSongs s)
        ,"uptime: " ++ show (stsUptime s)
        ,"playtime: " ++ show (stsPlaytime s)
        ,"db_playtime: " ++ show (stsDbPlaytime s)
        ,"db_update: " ++ show (stsDbUpdate s)]
