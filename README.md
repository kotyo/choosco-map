# choosco-map
Order Statistic Tree based map implementation for clojure

Besides normal map operations it supports retrieving and deleting items by position.

For the internal data representation it uses vectors so converting to/from edn should work also.

All operations are sublinear.

## TODOs
- [ ] Add a build system, so it can be easily packed
- [ ] Add tests
- [ ] Implement protocols like: IMap, ISeq, ... and any other applicable. (This comes with API change)
- [ ] Fix = equality check 