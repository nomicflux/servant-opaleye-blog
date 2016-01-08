- Separate out into Model files

- Not going into specifics of extensions - rule of thumb: add it if the compiler tells you to

- User.hs:
- Make datatype polymorphic
- Create concrete types
- Make password ByteString now, to now worry about db changes later
- Change FromJSON to deal with bytestring
- Product Profunctors!
- Make table
- Make conversion function

- BlogPost.hs:
- Similar
- Need 4 concrete types: differences between reading and writing
- Set so automatically disregards any id and timestamp sent in

- Add Queries

- Pass in Database Info!
- Add to main, since IO (will change eventually)
- Thread connection info throughout
- (Changing Post return values to give back db info)
- In Api/BlogPost.hs, remember to change datatypes accordingly

- Update cabal file
