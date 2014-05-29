#!/bin/bash

if [ $# -lt 3 ]; then
  echo "Usage:" "$0" "SRC_DIR TEMPLATES_DIR TEXTS_DIR [--show-all]"
  echo
  echo "Check for dead templates by searching both src and templates"
  echo "directories for their occurences and print the ones that were"
  echo "not found in either of them."
  echo
  echo "Note: if you want to see number of found occurences"
  echo "for all templates, then use option --show-all."
  echo
  echo "Note: this may not give 100% accurate results, ie. it won't"
  echo "mark used templates as dead, but it sometimes can overlook"
  echo "dead template (eg. by finding the string in src directory,"
  echo "which is not necessarily a template name)."
  exit 0
fi

if [ -d "$1" ]; then
  src_dir="$1"
else
  echo "$1 is not a directory"
  exit 1
fi

if [ -d "$2" ]; then
  templates_dir="$2"
else
  echo "$2 is not a directory"
  exit 1
fi

if [ -d "$3" ]; then
  texts_dir="$3"
else
  echo "$3 is not a directory"
  exit 1
fi

if [ "$4" = "--show-all" ]; then
  show_all=true
else
  show_all=false
fi

# extract names of all templates from a given file
function get_templates {
  while read file; do
    # we strip a file with templates from comments (lines that start
    # with single '#'), then we split it using ##..\n as record separator
    # and we get templates (well, mostly). then we separate fields
    # using '=', so we get template name in first field. then we check
    # if whole record contains '=' (to filter out invalid stuff) and
    # we print first field, ie. template name.
    grep -v '^#[^#]' "$file" | awk -W posix '
      BEGIN {
        RS="[#]{2,}[^\n]*[\n]";
        FS="[ \t\n]*="
      }
      $0 ~ "=" {
        print $1;
      }
      '
  done
}

# check for occurences in both src and templates directory
function check_occurences {
  while read template; do
    # with src it's easy, we just look for template name in quotation marks.
    # this, hovever, may sometimes lead to false counts and I don't
    # see a way to tell whether found string is really template name.
    src_occ=`grep "\"${template}\"" "$src_dir" -R | wc -l`
    # with templates it's more tricky, but also the result is more reliable. we want
    # template name with non-alnum character at the beginning (to prevent matches
    # like otherTEMPLATE...), optional :noescape after template name and parentheses
    # with arbirary amount of text between them (ie. arguments or lack of them).
    templates_occ=`grep "[^A-Za-z0-9_]${template}\(:noescape\)\{0,1\}(" "$templates_dir" -R | wc -l`
    texts_occ=`grep "[^A-Za-z0-9_]${template}\(:noescape\)\{0,1\}(" "$texts_dir" -R | wc -l`
    if [ $show_all = true -o $(($src_occ + $templates_occ + $texts_occ)) = "0" ]; then
      printf "src: %2d, templates: %2d, texts: %2d - %s\n" $src_occ $templates_occ $texts_occ $template
    fi
  done
}

find "$templates_dir" -name *.st -print | get_templates | check_occurences
