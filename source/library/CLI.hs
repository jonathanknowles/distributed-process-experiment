module CLI
    ( getConfig
    ) where

import Config
import Message
import Time

import Data.Monoid         ( (<>) )
import Options.Applicative ( (<**>)
                           , Parser
                           , ParserInfo
                           , auto
                           , command
                           , execParser
                           , fullDesc
                           , help
                           , helper
                           , info
                           , long
                           , metavar
                           , option
                           , progDesc
                           , strOption
                           , subparser
                           , value )

getConfig :: IO Config
getConfig = execParser configInfo

configInfo :: ParserInfo Config
configInfo = info (config <**> helper) fullDesc

config :: Parser Config
config = subparser
    (  command "master" masterCommand
    <> command "slave"  slaveCommand )

bindConfig :: Parser BindConfig
bindConfig = BindConfig <$> hostConfig <*> portConfig

masterCommand :: ParserInfo Config
masterCommand = info (Master <$> masterConfig) (progDesc "run as a master")

slaveCommand :: ParserInfo Config
slaveCommand = info (Slave <$> slaveConfig) (progDesc "run as a slave" )

masterConfig :: Parser MasterConfig
masterConfig = MasterConfig <$> bindConfig <*> poolConfig

slaveConfig :: Parser SlaveConfig
slaveConfig = SlaveConfig <$> bindConfig

poolConfig :: Parser PoolConfig
poolConfig = PoolConfig
    <$> sendPeriodConfig
    <*> waitPeriodConfig
    <*> seedConfig
    <*> blockSizeConfig

hostConfig :: Parser String
hostConfig = strOption
    (  long "host"
    <> metavar "HOSTNAME"
    <> help "bind to this host name" )

portConfig :: Parser String
portConfig = strOption
    (  long "port"
    <> metavar "INT"
    <> help "bind to this port number" )

blockSizeConfig :: Parser MessageCount
blockSizeConfig = MessageCount <$> option auto
    (  long "block-size"
    <> value 1024
    <> metavar "INT"
    <> help "how many messages to include within each block" )

seedConfig :: Parser MessageSeed
seedConfig = createMessageSeed <$> option auto
    (  long "with-seed"
    <> value 0
    <> metavar "INT"
    <> help "the initial seed" )

sendPeriodConfig :: Parser TimeDuration
sendPeriodConfig = flip durationFromInteger Seconds <$> option auto
    (  long "send-for"
    <> metavar "INT"
    <> help "duration of sending period (seconds)" )

waitPeriodConfig :: Parser TimeDuration
waitPeriodConfig = flip durationFromInteger Seconds <$> option auto
    (  long "wait-for"
    <> metavar "INT"
    <> help "duration of waiting period (seconds)" )
