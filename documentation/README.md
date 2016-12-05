# Scrive API Documentation

We are using the [OpenAPI specification](https://openapis.org/specification)
(FKA [Swagger 2.0](http://swagger.io/)) to specify our API.

Unfortunately, the current UI tooling for OpenAPI specifications did not meet
our requirements, so we hacked together
[`openapi2slate`](https://www.npmjs.com/package/openapi2slate).
You will need to do `npm install --global openapi2slate`.

This then outputs Slate Markdown to `stdout`.

## Generating documentation on your local machine

You need to generate the Markdown in the directory for Slate to pick it up:

```
openapi2slate documentation/scrive_api.yaml > source/index.html.md
```

Then install dependencies for Slate and run a local server:

```
bundle install
bundle exec middleman server
```

Now you should be able to view the generated page on some `localhost` port (it
will tell you).
To update the documentation, update `index.html.md` using `openapi2slate`.
Refresh the browser and you should see the changes.

## Updating documentation on http://apidocs.scrive.com/

[http://apidocs.scrive.com/](http://apidocs.scrive.com/) uses GitHub pages as
configured for this repository.

Updating `master` will auto-trigger a build and deployment via
[Travis CI](https://travis-ci.org/scrive/scrive-apidocs).

That's it!
