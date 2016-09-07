# Lesson 7 - Environment Variables

In the last lesson, we set up various sorts of loggers.  Now, we're going to let the user set a logging level through environment variables - and while we're at it, we might as well let them set stuff like the port and the connection information too.

## Setting up DataTypes

This being Haskell and all, our first step is to create a couple data types to hold information about the sorts of configurations we'll allow.  In my model, I want two different dimensions for my logging environment: what level of logging to use (the `Environment`, which I may use in other portions of the project as well) and the place to log to (`LogTo`):

```haskell
data Environment = Test
                 | Development
                 | Production
  deriving (Show, Eq, Read)

data LogTo = STDOut
             | STDErr
             | File String
  deriving (Show, Eq, Read)
```

Our `Environment` can be `Testing` (no logging from the WAI logger), `Production` (Apache-style, just enough to give us information about requests and responses, mainly for looking up stuff when it goes wrong), and `Development` (fully-formatted and human-readable).  `LogTo` can be to stdout, stderr, or to a file handler.

We're also deriving `Read` instances for these data types, since we'll read them in from `IO`.  This necessitates that they be typed exactly as they are written here; "STDOut" will work, but "stdout" will not.  If you prefer, you can write your own parser to capture the different options, allowing for different capitalizations.  But I've elected to keep things simple here.

## Getting Environment Variables

Now that we have our data types, we'll want to grab them from the environment.  The normal environment variable function, `lookupEnv`, suffers from two major problems for our purposes: It simply provides `Nothing` if the environment variable doesn't exist, and it only provides `String`s.  We'll want a one-stop-shop for providing our information, so we'll allow default options and parsing.

Along those lines, we'll import `Text.Read` to have access to the `readMaybe` function, so that we can parse environment variables without them crashing on us if they provide illegal values.  Alternatively, we could use `Either` or run them in an `ExceptT` transformer to tell us what goes wrong and where; once we set up the basic framework here, you can mess with it to do whatever you want.

```haskell
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

...

lookupEnvDefault :: Read a => String -> a -> IO a
lookupEnvDefault var def = do
  env <- lookupEnv var
  return (fromMaybe def $ env >>= readMaybe)
```

We have to `bind` the result of `lookupEnv var` to `readMaybe`, since both individually return `Maybe` values.  If either one fails, the entire thing is `Nothing`.

Next, let's use our newfound powers to set some configuration, starting with the port:

```haskell
import App (Config ( .. )
           , AppM
           , Environment ( .. )
           , Logging ( .. )
           , lookupEnvDefault
           )

...

startApp :: [String] -> IO ()
startApp args = do
  port <- lookupEnvDefault "SERVANT_PORT" 8080
  pool <- Pool.createPool openConnection PGS.close 1 10 5
  logger <- newStdoutLoggerSet defaultBufSize
  midware  <- mkREquestLogger $ def { destination = Logger logger }
  pushLogStrLn logger $ toLogStr $
    "Listening on port " ++ show port
  run port $ midware $ app (Config pool logger)
```

Play with this a bit to see what happens when there is an environment variable, when there isn't one, when it's a string instead of an integer, and so on.  We're logging to `stdout`, so you can see right away what happens.

Next, let's turn to our connection information:

```haskell
openConnection :: IO PGS.Connection
openConnection = do
  user <- lookupEnvDefault "SERVANT_PG_USER" "blogtutorial"
  pwd <- lookupEnvDefault "SERVANT_PG_PWD" "blogtutorial"
  db <- lookupEnvDefault "SERVANT_PG_DB" "blogtutorial"
  PGS.connect PGS.defaultConnectInfo
                 { PGS.connectUser = user
                 , PGS.connectPassword = pwd
                 , PGS.connectDatabase = db
                 }
```

Since `openConnection` is in `IO` anyhow, we can easily add in connection information as necessary.

## Optional: Using a Connection Info Type

Perhaps you'd prefer using types with the `ConnectionInfo`.  In that case, we can provide a data type, extend it with a `Default` option, create a conversion function, and get all the info in one fell swoop:

```haskell
import Data.Default.Class (def, Default)

...

data ConnectionInfo = ConnectionInfo
                      { connUser :: String
                      , connPassword :: String
                      , connDatabase :: String
                      }

instance Default ConnectionInfo where
  def = ConnectionInfo
        { connUser = "blogtutorial"
        , connPassword = "blogtutorial"
        , connDatabase = "blogtutorial"
        }

getConnInfo :: IO ConnectionInfo
getConnInfo =
  ConnectionInfo <$>
    lookupEnvDefault "SERVANT_PG_USER" (connUser def) <*>
    lookupEnvDefault "SERVANT_PG_PWD" (connPassword def) <*>
    lookupEnvDefault "SERVANT_PG_DB" (connDatabase def)

connInfoToPG :: ConnectionInfo -> PGS.ConnectInfo
connInfoToPG connInfo = PGS.defaultConnectInfo
                        { PGS.connectUser = connUser connInfo
                        , PGS.connectPassword = connPassword connInfo
                        , PGS.connectDatabase = connDatabase connInfo
                        }

openConnection :: IO PGS.Connection
openConnection = do
  connInfo <- getConnInfo
  PGS.connect (connInfoToPG connInfo)
```

If you don't want to use a `Default` instance, you can just set up a `defaultConnectionInfo :: ConnectionInfo` value yourself; I just used it since we're already importing `Data.Default.Class` anyhow.

You might notice, though, that we've just used several times to code to accomplish the same task as previously.  We do have more modular, type-safe code now; but, we were also only using 3 lines before and now have about 25.  You'll have to decide whether simplicity or modularity is more important to your application.

## Setting Up Customized Loggers

And now on to the main point of this lesson: configuring our loggers:

```haskell
makeLogger :: LogTo -> IO LoggerSet
makeLogger logTo = case logTo of
        STDOut -> newStdoutLoggerSet defaultBufSize
        STDErr -> newStderrLoggerSet defaultBufSize
        File filename -> newFileLoggerSet defaultBufSize filename

makeMiddleware :: LoggerSet -> Environment -> IO Middleware
makeMiddleware logger env = case env of
      Test -> return id
      Production -> mkRequestLogger $ def { destination = Logger logger
                                          , outputFormat = Apache FromSocket
                                          }
      Development -> mkRequestLogger $ def { destination = Logger logger }
```
One function configures the logger based on our `LogTo`, and the other sets the logger up in our `Middleware` based on the logging output location.  Then we interleave those into our `startApp` function:

```haskell
startApp :: [String] -> IO ()
startApp args = do
  port <- lookupEnvDefault "SERVANT_PORT" 8080
  env <- lookupEnvDefault "SERVANT_ENV" Production
  logTo <-  lookupEnvDefault "SERVANT_LOG" STDOut
  pool <- Pool.createPool openConnection PGS.close 1 10 5
  logger <- makeLogger logTo
  midware <- makeMiddleware logger env
  pushLogStrLn logger $ toLogStr $
    "Listening on port " ++ show port ++ " at level " ++ show env ++ " and logging to "  ++ show logTo
  run port $ midware $ app (Config pool logger)
```
And we are done!

## Adding Command-Line Arguments

This works fine if we're logging to `stdout` or `stderr`, but can be a little cumbersome if we want to log to a file.  We can set "SERVANT_LOG" to "File \"filename.txt\"", and that will work, but we can also pass in a command-line argument.

To keep this simple, we'll just take one argument: a log file.  If it exists, we'll set the `Logger` to `File filename`; otherwise, we'll use whatever is in the enivironment variable, falling back to `STDOut` as a last resort.

Let's go to our "app/Main.hs" file, which we've neglected up until now:

```haskell
import System.Environment

main :: IO ()
main = getArgs >>= startApp
```

And process the possible argument using `listToMaybe`:

```haskell
startApp :: [String] -> IO ()
startApp args = do
  port <- lookupEnvDefault "SERVANT_PORT" 8080
  env <- lookupEnvDefault "SERVANT_ENV" Production
  logTo <- case listToMaybe args of
    Just filename -> return $ File filename
    Nothing -> lookupEnvDefault "SERVANT_LOG" STDOut
  pool <- Pool.createPool openConnection PGS.close 1 10 5
  logger <- makeLogger logTo
  midware <- makeMiddleware logger env
  pushLogStrLn logger $ toLogStr $
    "Listening on port " ++ show port ++ " at level " ++ show env ++ " and logging to "  ++ show logTo ++ " with args " ++ unwords args
  run port $ midware $ app (Config pool logger)
```

## Other Directions

We could have processed all of our options using command-line arguments instead.  I opted for environment variables because A) they are easier to set up &agrave; la carte (so we wouldn't have to get into command-line argument parsing), and B) most of them will be relatively stable for a given server environment.  But if you want to pass in arguments instead, you can look at the [optparse-applicative](https://hackage.haskell.org/package/optparse-applicative) library.

Alternatively, for some things such as database configuration info, you may want to have a configuration file lying around with the relevant information, so that it would be easy to upload to a server but also possible to remove from, say, a git repository.

In any of these cases, the hard part has been to set aside places in `IO` to grab the information we need.  If you want to switch to arguments or config files, it should be relatively easy to do so from this setup.

## Run

Build, set your environment variables to your heart's content, then try `stack exec blog-tutorial-exe my_log_file.txt` to output logging to a file.
