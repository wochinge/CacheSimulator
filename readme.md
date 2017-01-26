# CacheSimulator

This is a small program do simulate different caching algorithms.

## Options
-   `--logfile`: Describes the path to your log file
-   `--help`: Shows all Options
-   `--size`: Calculates the minimal storage which would be needed to store all files
    (this is use useful if you want to set your cache size to fractions of the whole storage)
-   `--lru`: Simulates a LRU cache
-   `--cacheSize`: Allows you to set a cache size in bytes (default: 1 GB)

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
