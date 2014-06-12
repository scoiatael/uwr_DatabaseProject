## Haskell and PSQL

Uses
- haskell
- postgresql
- vty-gui for ui

to start :
1. create db 'projekt.db' and make sure user 'postgres' has all rights to this db.
2. make sure you can connect as 'postgres' to this db (psql -U postgres projekt.db)
2. either 
- runghc Frontend.hs - make sure you have all needed haskell packages.
- or ./CoffeeShopx64 if you happen to be on x64 Linux

