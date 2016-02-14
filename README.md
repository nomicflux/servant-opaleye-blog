# Lesson 2 - Opaleye

You've gotten a Servant server up and running - great!  Now, let's add a database.

The goal for this lesson is to have some basic familarity with setting up Opaleye.  It is complicated; however, I hope that I can show that it doesn't need to be painful, and that the complexity is there for good reason.  We'll set up Opaleye for the *User* datatype first, which will be straightforward.  Then, we'll set it up for the *BlogPost* datatype, where we'll see why some of the boilerplate exists.

If you look at the files, you'll notice all sorts of language extensions being used (`FlexibleInstances`, `MultiParamTypeClasses`, and so on).  To keep this tutorial from becoming too complicated, I am going to gloss over these.  Add what is in the files, or what the compiler tells you to, and you should be good.  In general, I won't talk too much about dependencies and imports either, unless people complain that they would be helpful; they are all in the files above, if you need them.

## Step 1: Set up database

In order to use a database, we'll need a database.  I have included blogtutorial_schema.sql if you wish to import the schema into Postgres.  If you would prefer to create the tables manually, here is a rough-and-ready version of what I use:
```pgsql
CREATE TABLE users (
    email VARCHAR UNIQUE NOT NULL,
    password BYTEA NOT NULL,
    PRIMARY KEY(email)
);

CREATE TABLE posts (
    id SERIAL,
    title VARCHAR NOT NULL,
    body VARCHAR NOT NULL,
    users_email VARCHAR NOT NULL,
    "timestamp" TIMESTAMPTZ DEFAULT now(),
    PRIMARY KEY(id)
);

ALTER TABLE posts ADD CONSTRAINT fk_posts_users FOREIGN KEY(users_email) REFERENCES users(email);
```
Make sure that the password is a *BYTEA*, as we'll be storing binary data in there in future lessons.

## Step 2: Separate out model files

Next, we'll make some adjustments to our API files from the previous lesson.  There, we mixed together the API and the data definitions.  But we'll want to use those data definitions in multiple places: when connecting to our database, when composing queries, when accessing the API, and so on.  We don't want to make everything depend on everything else, so let's place our models in a separate folder.  Create a "src/Models" directory.  We'll start with Models/User.hs.

While we're on the subject, we'll also split out our queries into their own folder: "src/Queries".  Query files and API files alike will rely on Model files, but Queries and APIs will have no reliance on each other.

## Step 3: Create the User Model

### Polymorphic Users

We'll be extending the model we created last time.  The biggest difference is that we will create a `User'` datatype:
```haskell
data User' email pwd = User
                         { userEmail    :: email
                         , userPassword :: pwd
                         }
```
Opaleye's tutorials would use `a` for `email` and `b` for `pwd`, but this way the code is clearer about what the different fields are.

This may seem somewhat odd to you.  Why go to the trouble of setting up datatypes in a typesafe language, only to throw them away and let someone instantiate a `User'` with any two types they want?

And this is indeed a bit odd.  Ideally, things like this can be taken care of in the future with dependent types and other deep magicks.  But for the moment, this lets us treat the Opaleye definition of a `User`, and our normal Haskell definition of a `User`, as the same sort of object.  This is even though Opaleye's types need to play nice with Postgres, while the rest of our logic really shouldn't have to think about Postgres at all.  And later, we'll set up separate read and write types as well.

Confused?  If not, you're a genius; pat yourself on the back.  For you mere mortals, just follow along, and hopefully by the end of this lesson you'll have some understading of this craziness, at least enough to use it for your own projects.

Ok, so we've set up the `User'` datatype.  Let's make the concrete instances which we'll actually use:
```haskell
type User = User' Email ByteString
type UserColumn = User' (Column PGText) (Column PGBytea)
```
So we have the type alias `User`, which will be our `User` from the previous lesson.  We also have `UserColumn`, which gives Opaleye what it needs to interact with a database.

Note that the password is a `ByteString` and not a regular `String`.  That will add a little extra processing for this lesson, but will make things much easier in the future.  We will need to change the `parseJSON` function to accomodate this:
```haskell
instance FromJSON User where
  parseJSON (Object o) = User <$>
                              o .: "email" <*>
                              (BS.pack <$> o .: "password")
  parseJSON _ = mzero
```

### Product Profunctors

The magic, however, happens with the following:
```haskell
$(makeAdaptorAndInstance "pUser" ''User')
```
If you are unfamiliar with the `$(...)` syntax, this is an example of *TemplateHaskell*.  Don't worry too much about it; the main point is that we are creating something called a __product profunctor__.  Like many Haskell terms, this is technobabble for a relatively simple and useful concept.  Let's say that I have a pair of things: `('hello', 1)`.  I want to create a function which will take the first element, a string, and convert it to uppercase.  I also want it to take the second element, an integer, and increment it.  So I should be able to type `pairFunc ('hello', 1)` and end up with `('HELLO', 2)`.

I can create such a function like this:
```haskell
pairFunc = p2 (map toUpper, (+1))
```
In other words, it takes a pair of functions, and converts it to a function acting on a pair.

This is kind of nice, but it involves importing a library and learning a new syntax for something which wouldn't be too hard to program otherwise.  However, in our case, we get another benefit: the product profunctor we created, `pUser`, is defined for our polymorphic type `User'`.  This means that we can apply `pUser` to Opaleye's types or to our own code's types, and forget about the distinction to an extent.

If you feel that this is all a little much, however, you can ignore profunctors and do everything manually.  I have included them because everything in the Opaleye tutorials uses them, so it pays to understand what they mean if you want to look into anything more advanced.

### Setting up the Table

To set up an Opaleye table, we'll do the following:
```haskell
userTable :: Table UserColumn UserColumn
userTable = Table "users" (pUser User { userEmail = required "email"
                                      , userPassword = required "password"
                                      })
```
Notice the type: it is `Table UserColumn UserColumn`.  `UserColumn` is repeated because Table needs to know both what type it will write to the database, and what type it will read from the database.  For right now, these are the same.

`pUser` is the product profunctor discussed above.  It creates a function which takes a `User'` and returns another `User'`.  We could have written a helper function like such:
```haskell
userTransform user = User { userEmail = required "email" (userEmail user)
                          , userPassowrd = required "password" (userPassword user)
                          }

userTable' :: Table UserColumn UserColumn
userTable' = Table "users" userTransform
```

At this point, we'll also create a helper function to converting our datatype into an Opaleye-Postgres format:
```haskell
userToPG :: User -> UserColumn
userToPG = pUser User { userEmail = pgString
                      , userPassword = pgStrictByteString
                      }
```

### Summary of Opaleye

To break down the complexity of Opaleye a bit, I'd like to break down the steps we just did into a quick list:

1. Create polymorphic type
2. Create concrete types
3. Make product profunctor (if you want)
4. Set up table
5. Set up conversions

## Step 4: Creating User Queries

You probably have a headache from setting up an Opaleye table.  Perhaps you are a step away from running off screaming; the fact that I am about to say that we will start writing Opaleye queries may put you over the edge.  If so, relax; the queries are the nice part about Opaleye.

Let's start with a simple query which grabs all of our `User`s:
```haskell
usersQuery :: Query UserColumn
usersQuery = queryTable userTable
```
That's it.  No, really, it is.  Next, let's grab `User`s by email:
```haskell
userByEmailQuery :: Email -> Query UserColumn
userByEmailQuery email = proc () -> do
                           user <- usersQuery -< ()
                           restrict -< userEmail user .== pgString email
                           returnA -< user
```
If you are unfamiliar with Arrow syntax, this will look a little wonky, but hopefully you can follow the basic logic: get all the `User`s from the above query, restrict them by only pulling the ones with the email we want, and return those.  Notice that we can place one query smack-dab in another with no concerns about composability.

And that is it.

## Step 5: Create the BlogPost Model

Let's follow the breakdown we had above:

### Step 5.1: Create Polymorphic Type

While boilerplatey, this step is straightforward:
```haskell
data BlogPost' id title body email time = BlogPost
                                            { bpId         :: id
                                            , bpTitle      :: title
                                            , bpBody       :: body
                                            , bpUsersEmail :: email
                                            , bpTimestamp  :: time
                                            }
```

### Step 5.2: Creating Concrete Types

This is where we'll differ from the `User` model.  With a `User`, we read and write the same things; conversion to JSON will remove a `userPassword` field, but we don't need to be concerned with that for types.

For blog posts, we are doing two things differently.  First, we have Postgres automatically assign a serial ID; this means that we don't need users to __POST__ an ID when submitting a blog post (and would prefer that they did not).  Second, each post has a timestamp, which is also automatically generated and does not have to been included when we __POST__ posts and ship them off to the database.

So we'll need four different concrete types: reading from the database, writing to the database, reading from JSON, and writing to JSON.  This is a lot, but it is all straightforward:
```haskell
type BlogPostRead = BlogPost' BlogPostID String String Email DateTime
type BlogPostWrite = BlogPost' (Maybe BlogPostID) String String Email (Maybe DateTime)
type BPColumnRead = BlogPost' (Column PGInt8)
                              (Column PGText)
                              (Column PGText)
                              (Column PGText)
                              (Column PGTimestamptz)
type BPColumnWrite = BlogPost' (Maybe (Column PGInt8))
                               (Column PGText)
                               (Column PGText)
                               (Column PGText)
                               (Maybe (Column PGTimestamptz))
```

We'll also update the `FromJSON` instance to reflect the fact that two of the fields are optional:
```haskell
instance FromJSON BlogPostWrite where
  parseJSON (Object o) = BlogPost <$>
                                  o .:? "id" <*>
                                  o .: "title" <*>
                                  o .: "body" <*>
                                  o .: "email" <*>
                                  o .:? "timestamp"
  parseJSON _ = mzero
```

### Step 5.3: Make Product Profunctor

This is, again, just a single line of TemplateHaskell:
```haskell
$(makeAdaptorAndInstance "pBlogPost" ''BlogPost')
```

### Step 5.4: Set up table

This is mostly the same as before:
```haskell
blogPostTable :: Table BPColumnWrite BPColumnRead
blogPostTable = Table "posts" (pBlogPost BlogPost { bpId = optional "id"
                                                  , bpTitle = required "title"
                                                  , bpBody = required "body"
                                                  , bpUsersEmail = required "users_email"
                                                  , bpTimestamp = optional "timestamp"
                                                  })
```
The two optional fields are now marked `optional` instead of `required`.  Also, the type is `Table BPColumnWrite BPColumnRead`; the type for reading to the database is different from the type reading out of the database.  (If you flip their order, like I always do, the compiler will come to your aid and berate you mercilessly.)

### Step 5.5: Set up conversions

Again, this is similar to the situation with the `User` model:
```haskell
blogPostToPG :: BlogPostWrite -> BPColumnWrite
blogPostToPG = pBlogPost BlogPost { bpId = const Nothing
                                  , bpTitle = pgString
                                  , bpBody = pgString
                                  , bpUsersEmail = pgString
                                  , bpTimestamp = const Nothing
                                  }
```
I wanted to ensure that someone would be unable to submit an ID or a Timestamp, so I force them to be `Nothing`.  Perhaps you would wish to perform some sanity checks instead, or to let them through if someone provides them and let them be `Nothing` otherwise.  The nice thing about a conversion function like this is that you get to set up however you want you system to work, and have the assurance of knowing that your changes in this one place will keep the rest of the application updated.

### Summary

We can see that, even in this more complicated case, we follow the same five steps as before and end up with working Opaleye code.  So when you feel flustered, just breathe, have some tea, and do the next step.

## Step 6: Creating BlogPost queries

This will be almost the same as creating `User` queries.  See how much you can do on your own without looking at the file.

The main difference to note is that you will need to specify whether you want a `BPColumnRead` or a `BPColumnWrite`.  Otherwise, everything will be exactly the same.

## Step 7: Connecting to the Database

We're getting close.  One thing we have not done yet is actually connect to the database.  Opaleye does the work of setting up tables and such, but we still need to connect to a server.

Connection information is in `IO`.  If we look at Lib.hs, we have one function which deals in `IO`, and that is `startApp`.  This seems like as good a place as any to put the connection to our database:
```haskell
startApp :: IO ()
startApp = do
             con <- PGS.connect PGS.defaultConnectInfo
                                { PGS.connectUser = "blogtutorial"
                                , PGS.connectPassword = "blogtutorial"
                                , PGS.connectDatabase = "blogtutorial"
                                }
             run 8080 $ app con
```
Once connected, we'll need to thread that connection along to the application, and from there to all of our servers.  As you can see in the code snippet above, we call `app` with `con` as an argument.  Let's update that now:
```haskell
app :: PGS.Connection -> Application
app con = serve api $ server con
```
And we had to pass `con` on to `server` so that everyone can use it:
```haskell
server :: PGS.Connection -> Server API
server con = userServer con
        :<|> blogPostServer con
```
And from here, it's the concern of our individual sub-APIs.  Passing the buck at its finest.

## Step 8: Update the APIs

Now we have the connection info passed into our server, and by extension each sub-API's own individual server.  We'll have to update their functions so that we use the database, instead of the hardcoded examples we had previously.

Let's start with Api/User.hs.  Let's add `import qualified Database.PostgreSQL.Simple as PGS` to the beginning of the file.  Then, let's update the `userServer` to utilize our connection:
```haskell
userServer :: PGS.Connection -> Server UserAPI
userServer con = getUsers con
            :<|> getUserByEmail con
            :<|> postUser con
```

And now let's update the functions:
```haskell
getUsers :: PGS.Connection -> AppM [User]
getUsers con = liftIO $ runQuery con usersQuery

getUserByEmail :: PGS.Connection -> Email -> AppM (Maybe User)
getUserByEmail con email = liftIO $ listToMaybe <$> runQuery con (userByEmailQuery email)

postUser :: PGS.Connection -> User -> AppM Int64
postUser con user = liftIO $ runInsert con userTable $ userToPG user
```
All of the functions now take a `PGS.Connection` as their first argument - we'll clean that up in Lesson 4.  We use `runQuery con` to, well, run the queries which we have built in the `Queries/User.hs` file.  Similarly, we use `runInsert con` to actually insert a user.  Just make sure to use our conversion function first: `userToPG`.

Previous to this step, we've just set up the logic for database interactions.  Now that we're actually using the database, we'll need to run all of this in `IO`; to do this, make sure to use `liftIO` on anything involving actual database results.

One more minor point: Since I'm trying to keep things simple, instead of returning a list of users from a __POST__ request, I'm merely returning the output of `runInsert`, which is an `Int64`.  The `UserAPI` is changed to match.

An usual, everything in Api/BlogPost.hs follows suit, and could be rewritten just with the information I've provided about Api/User.hs.  If you do get stuck, feel free to peek at the source code.

## Step 9: Update Cabal

As usual, make sure that you have noted all new dependencies and modules in Cabal (in particular, all of our "Models/" and "Queries/").  The run `stack build`, `stack exec blog-tutorial-exe`, and `curl` to your heart`s content.

## Step 10: Walk Away From the Computer

You just made it through one heck of a tutorial.  Give your poor brain a chance to relax.
