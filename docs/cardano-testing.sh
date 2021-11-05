while inotifywait cardano-testing.md cardano-testing.bib; do pandoc --citeproc cardano-testing.md -o cardano-testing.pdf; done
