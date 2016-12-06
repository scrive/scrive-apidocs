# Scrive API Documentation using Slate

Forked and edited from the original
[Slate repository](https://github.com/lord/slate).

<p align="center">
  <img src="https://raw.githubusercontent.com/lord/img/master/logo-slate.png" alt="Slate: API Documentation Generator" width="226">
</p>

## What's in here?

**Use with caution:**
merging to `master` auto-deploys to
[http://apidocs.scrive.com/](http://apidocs.scrive.com/) using Travis CI.

Although `master` is set up as a protected branch and requires a review and
Travis CI to pass, so it's not that dangerous...

### Differrences from Slate upstream

This repository contains the [Open API specification](https://www.openapis.org/)
(FKA Swagger 2.0)
documentation for the Scrive Document API and a copy of _Slate_.

The _Open API_ documentation lives in `documentation/` and instructions on using
that are included below.

Everything else is from _Slate_, with the following modifications:

* `source/` has many style changes and `index.html.md` is auto-generated using
  `openapi2slate`
* `.travis.yml`, `.gitignore`, `deploy.sh` changed to suit our modified build
  and deploy needs
* Removed `.github/*` templates for Issues and Pull Requests
* Added `deploy_rsa.enc` to magically grant Travis CI accces to GitHub
  (encrypted access token that only Travis can read)
* Modified this `README.md`

Other than that other differences probably mean that upstream made some changes.

Also, thanks to all the
[original contributors](https://github.com/lord/slate#contributors) of _Slate_!

## Updating the Documentation

### Dependencies

Unfortunately, the current UI tooling for OpenAPI specifications did not meet
our requirements, so we hacked together
[`openapi2slate`](https://www.npmjs.com/package/openapi2slate).

You will need to `npm install --global openapi2slate`.

This gives you a command line tool where you specify an input file, and it gives
you Slate Markdown via `stdout`.

You will also need to install the Slate dependencies, running `bundle install`
in this directory should be enough.  More details are available in the
[original documentation](https://github.com/lord/slate#getting-started-with-slate).

### Generating documentation on your local machine

You need to generate the Markdown in the directory for Slate to pick it up:

```
openapi2slate documentation/scrive_api.yaml > source/index.html.md
```

Then run a local server that watches changes in `source/`:

```
bundle exec middleman server
```

Now you should be able to view the generated page on some `localhost` port (it
will tell you).

After editing files in `documentation/` you need to manually update
`index.html.md` to see the changes :(

### Updating documentation on http://apidocs.scrive.com/

<p>
  <a href="https://travis-ci.org/scrive/scrive-apidocs">
    <img src="https://travis-ci.org/scrive/scrive-apidocs.svg?branch=master" alt="Master Build Status">
  </a>
</p>

[http://apidocs.scrive.com/](http://apidocs.scrive.com/) uses GitHub pages as
configured for this repository.

You will need to create a Pull Request and get someone to review it.

Once merged (thus updating `master`)  a build will auto-trigger and deployment
will happen via [Travis CI](https://travis-ci.org/scrive/scrive-apidocs).  You
can look into `.travis.yml` if you are curious...


That's it!
