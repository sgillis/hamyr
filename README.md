Hamyr
=====

Hymir server in Haskell

Possible improvements
---------------------
 - [x] Improve 404 error message to be more descriptive
 - [x] Merge implementation of `transferValue` for `TransferValue` and `ReserveValue`
 - [x] sendResponseStatus return JSON
 - [x] Refactor functions to be more pure in Transactions.hs
 - [ ] QuickCheck tests
 - [x] Benchmark
 - [ ] Move to MySQL
 - [ ] Implement bonus amounts
 - [ ] Implement multiple CurrencySelection options
 - [x] Caching
 - [ ] Shield direct access to WalletBalance in Model.hs, should only use
       `getBalance` as an entry point to `WalletBalance`
 - [ ] Improve module structure
 - [x] One transaction per function
 - [ ] Haddock documentation
