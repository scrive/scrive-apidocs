#!/bin/bash -e

tmp_dir="tmp"
dest="files"
cssfile="style.css"

timestamp="$(TZ=UTC date)"

# Make directories
if [ -d "$dest" ]; then
	rm -r "$dest"
fi
mkdir "$dest"
mkdir "$dest/templates"

# Compile the processor
printf "Compile the HTML processor...\t\t\n"
ghc processHTML
printf "Compile the HTML processor...\t\tDone.\n"
# Run the pre-processor which puts files together
./pre.sh "$tmp_dir"

printf "Processing HTML files...\t\t"
# Process the files
for file in $tmp_dir/*.html; do
	# Extract the filename
	html=$(sed -e "s/${tmp_dir}\/\(.*html\)/\1/g" <<< $file)
	htmlspaces=$(sed -e "s/_/ /g" <<< $html)
	# Run main HTML processor
	./processHTML "${file}" "${cssfile}" |
	# Remove Google URL redirect from links.
	sed -e 's/http[s]*:\/\/www.google.com\/url?q=//g' |
	# Remove params appended to URLs.
	sed -e 's/&amp;[a-z]*=[A-z0-9]*//g' |
	# Fix forward slash (%3A) and colons (%2A) on actual URLs
	sed -e 's/%3A/:/g' -e 's/%2F/\//g' |
	# Add timestamp
	sed -e "s/%LAST_UPDATED_TIMESTAMP%/${timestamp}/g" > "${dest}/${htmlspaces}"
done
echo "Done."

printf "Checking W3C HTML5 compliance...\n"
ls ${dest}/*.html | xargs -I % python html5check.py %
printf "Checking W3C HTML5 compliance...\tDone.\n"

printf "Make string template versions...\t"
# Make string template versions of the HTML files
cd "$dest"
for file in *.html; do
	ST_NAME="$(sed -e 's/\.html//g' -e 's/ //g' <<< $file)"
	ST_FILE="templates/${ST_NAME}.st"
	echo -e "###\n${ST_NAME}=" | cat - "$file" > /tmp/makeStringTemplate && mv /tmp/makeStringTemplate "${ST_FILE}"
	echo "###" >> "${ST_FILE}"
done
cd ..
echo "Done."

st_files=("Appendix3EvidenceLog"
"Appendix5EvidenceofIntent")
st_filesdir="../templates/evidencelog/files"

printf "Copy .st files to templates folder...\t"
if [ -d $st_filesdir ]; then
	rm -r $st_filesdir
fi
mkdir $st_filesdir

for f in ${st_files[@]}
do
	cp $dest/templates/$f.st $st_filesdir/
done
echo "Done."

html_files=("Evidence Quality of Scrive esigned Documents"
"Appendix 1 Evidence Quality Framework"
"Appendix 2 Service Description"
"Appendix 6 Digital Signature Documentation")
html_files_dir="../files"

printf "Copy HTML files used by kontrakcja...\t"
for f in "${html_files[@]}"; do
	cp "$dest/$f.html" "$html_files_dir/"
done
echo "Done."
