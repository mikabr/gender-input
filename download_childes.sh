mkdir childes
cd childes

wget -r -np -nH --no-parent -e robots=off -R "index.html*" http://childes.psy.cmu.edu/data/Eng-NA-MOR/
find . -name "*.zip" | while read filename; do unzip -o -d "`dirname "$filename"`" "$filename"; done;
find . -type f -name '*.zip' -delete
