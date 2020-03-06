# Evidence Package

## Requirements
You will need at least: `optipng`, `ghc` & Python 2.
Plus any Haskell packages `ghc` will complain about.

## Short instructions
1. Download HTML files manually from Google Drive into `exports/`
   (see first paragraph below).
2. Run `./runProcess.sh`
3. If output looks strange then something is probably wrong
4. Commit stuff in `{kontrakcja}/files/` and
   `{kontrakcja}/templates/evidencelog/files/`
5. Run tests and stuff like that...

## What the F\* does this do?
All you need to do is `./runProcess.sh`.

This will expect the HTML files exported from Google Docs in `exports/`.
To get these you will need to go through all the documents in the
[Google Drive folder]
(https://drive.google.com/drive/folders/1EVAqTWbnxFrA59mTcMywzMDgNV7HdW-8)
for the evidence package and do "File -> Download as -> Web page (.html,
zipped)".

Move or unzip whatever Google gives you into `exports/`.

Now `runProcess.sh` will call `pre.sh` which copies the stuff from `exports/`
into `tmp/` but also moves all the images together and does some `sed` magic to
avoid namespace conflicts. This allows us to run `optipng` to save a few KB.

It then calls `processHTML` on all of these HTML files.

This does several things: it processes the Google-exported HTML to remove a
bunch of fluff, and make it W3C compliant and stuff like that (read the source).
It also removes any embedded and inlined styles and adds `style.css` as an
embedded stylesheet to every document.
Finally it converts all images into embedded `base64` encoded images, as
evidence attachments need to be self-contained.

Back to `runProcess.sh`, it uses `html5check.py` to check that the documents are
HTML5 compliant.

Then we convert the documents into String Template versions.
All this is put into `files/` and `files/templates/`.

Finally the script copies the relevant HTMl and `*.st` files into the
directories used by `kontrakcja`:

    kontrakcja/files/
    kontrakcja/templates/evidencelog/files/

These are things that you need to commit when updating the Evidence Package.
