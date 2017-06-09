# CacheSimulator

This is a small program do simulate different caching algorithms.

# Execute

```bash
# Install haskell
sudo apt-get haskell-platform

# Get stack
curl -sSL https://get.haskellstack.org/ | sh

# Clone the project and initialize stack
git clone https://github.com/wochinge/CacheSimulator.git
cd CacheSimulator
stack setup
stack build

# Execute with arguments of your choice, e.g.:
stack exec CacheSimulator-exe -- --cacheSize 123456 --ideal --writeAddsToCache --logfile ~/logfiles/file1
```

## Options
-   `--logfile`: Describes the path to your log file
-   `--help`: Shows all Options
-   `--size`: Calculates the minimal storage which would be needed to store all files
    (this is use useful if you want to set your cache size to fractions of the whole storage)
-   `--lru`: Simulates a LRU cache
-   `--mfu`: Simulates a MFU cache
-   `--2q`: Simulates 2q (similar to Lru-2)
-   `--car`: Simulates a CAR cache (<http://www.cse.iitd.ernet.in/~sbansal/pubs/fast04.pdf>)
-   `--cart`: Simulates a CART cache (<http://www.cse.iitd.ernet.in/~sbansal/pubs/fast04.pdf>)
-   `--ideal`: Simulates a ideal cache (ideal cache = algorithm which knows the future)
-   `--cacheSize`: Allows you to set a cache size in bytes (default: 1 GB)
-   `--writeAddsToCache`: Whenver a file is written, it is also instantly added to the cache

## Requirements
To do so, you need a log file, which has lines in the following form:

```
<date> <time> <read || write || remove> <unique fileID> <file size in bytes>
```

E.g.
```
2016-10-01 00:00:09 read 0123 524288000
2016-10-01 00:00:09 write 0123 524288000
2016-10-01 00:00:09 remove 4567 524288000
```
