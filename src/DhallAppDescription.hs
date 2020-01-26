{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module DhallAppDescription where

import           Data.List (sortBy)
import           Data.Ord  (comparing)
import           Dhall

data KObjectImpl = Exit
                 | KLog { prefix :: Text }
                 | Process { pid :: Natural, binary :: Text, capabilities :: [Natural]}
                 | Thread { parentPid :: Natural }
  deriving (Generic, FromDhall, Show)

data KObject = KObject { gid :: Natural, impl :: KObjectImpl }
  deriving (Generic, FromDhall, Show)

data ApplicationDescription = ApplicationDescription { kobjects :: [KObject]}
  deriving (Generic, FromDhall, Show)

processes :: ApplicationDescription -> [KObject]
processes a = sortBy (comparing processPid) $ filter isProcess (kobjects a)
  where isProcess KObject{impl=Process{}} = True
        isProcess _                       = False
        processPid KObject{impl=Process{pid=p}} = p
        processPid _                            = error "Not a process"

processPid :: KObject -> Natural
processPid KObject{impl=Process{pid=p}} = p
processPid _                            = error "not a process"

processCaps :: KObject -> [Natural]
processCaps KObject{impl=Process{capabilities=c}} = c
processCaps _                                     = error "not a process"

kobjectKind :: KObject -> Text
kobjectKind KObject{impl=Exit}      = "exit_kobject"
kobjectKind KObject{impl=KLog{}}    = "klog_kobject"
kobjectKind KObject{impl=Process{}} = "process"
kobjectKind KObject{impl=Thread{}}  = "thread"

parseApplicationDescription :: FilePath -> IO ApplicationDescription
parseApplicationDescription = detailed . inputFile auto
