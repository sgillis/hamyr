Hamyr
=====

Hymir server in Haskell

Possible improvements
---------------------
 - [.] Improve 404 error message to be more descriptive
 - [x] Merge implementation of `transferValue` for `TransferValue` and `ReserveValue`
 - [.] sendResponseStatus return JSON
 - [x] Refactor functions to be more pure in Transactions.hs
 - [ ] QuickCheck tests
 - [x] Benchmark
 - [ ] Move to MySQL
 - [ ] Implement bonus amounts
 - [ ] Implement multiple CurrencySelection options
 - [.] Caching
 - [ ] Shield direct access to WalletBalance in Model.hs, should only use
       `getBalance` as an entry point to `WalletBalance`
 - [ ] Improve module structure
 - [.] One transaction per function
 - [ ] Haddock documentation
