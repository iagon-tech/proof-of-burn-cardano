while inotifywait burn.md burn.bib; do pandoc --citeproc burn.md -o burn.pdf; done
