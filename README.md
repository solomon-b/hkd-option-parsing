# Proof of Concept Options Parsing with Hiddledy

Using Hgher Kinded Data via Higgledy we can unify all our options parsing and truly elminate all lingering unnecesary Maybes.

```
➜ cert_path=/domain metadata_url=http://www.bing.com cabal run hkd-option-parsing -- --hostname localhost --username fred --db_uri http://www.google.com --cert_path /home
Just (Options {hostname = "localhost", port = 80, devMode = False, userName = User "fred", dbUrl = http://www.google.com, certPath = Just "/domain", metadataDB = http://www.bing.com})
```
