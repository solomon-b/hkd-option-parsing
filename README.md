# Proof of Concept Options Parsing with Hiddledy

Using Hgher Kinded Data via Higgledy we can unify all our options parsing and truly elminate all lingering unnecesary Maybes.

```
➜ cert_path=/domain metadata_url=http://www.bing.com cabal run hkd-option-parsing -- --hostname localhost --username fred --db_uri http://www.google.com --cert_path /home
Options {hostname = Last {getLast = Just "localhost"}, port = Last {getLast = Just 80}, devMode = Last {getLast = Just False}, userName = Last {getLast = Just (User "fred")}, dbUrl = Last {getLast = Just http://www.google.com}, certPath = Last {getLast = Just (Just "/domain")}, metadataDB = Last {getLast = Just http://www.bing.com}}
```
