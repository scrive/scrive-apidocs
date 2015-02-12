# frontend unit tests

## Install

    $ cd frontend/
    $ npm i

## Run

    $ cd frontend/
    $ grunt test

Coverage reports can be founder under `frontend/coverage/$launcher/index.html`.

## Writing

1. The unit test are written using mocha (http://mochajs.org/) using the BDD ui.
2. The file name of the unit test should correspond to the file it is testing i.e
   "test/common/select.js" tests what is in the file "scripts/common/select.jsx".
3. Use React's own test utilities (https://facebook.github.io/react/docs/test-utils.html)
   for testing components.


## Updating localization.js

The localization file can be updated from your localhost by running:

  $ cd frontend/
  $ grunt fetchlocalization
