# Scrive API Documentation

We are using the [OpenAPI specification](https://openapis.org/specification)
(FKA [Swagger 2.0](http://swagger.io/)) to specify our API.

Unfortunately, the current UI tooling for OpenAPI specifications did not meet
our requirements, so we hacked together
[`openapi2slate`](https://www.npmjs.com/package/openapi2slate).
You will need to do `npm install --global openapi2slate`.

This then outputs Slate Markdown to `stdout`.

## Generating documentation on your local machine

Our clone of Slate lives in the
[`scrive-apidocs`](https://github.com/scrive/scrive-apidocs) repository.
Instructions there tell you how to install dependencies to get a Slate server
running, this is:

```
bundle install
bundle exec middleman server
```

Now you should be able to view the generated page on some `localhost` port (it
will tell you).
To update the documentation, take the output from `openapi2slate` and replace
the `source/index.html.md` in your cloned Slate repository.
Refresh the browser and you should see changes.

## Updating documentation on http://apidocs.scrive.com/

[http://apidocs.scrive.com/](http://apidocs.scrive.com/) uses GitHub pages as
configured for the `scrive-apidocs` repository.

Run `openapi2slate` against `scrive_api.yaml` to generate an updated
`index.html.md`, then use this and create a new Pull Request on
[`scrive-apidocs` repsitory](https://github.com/scrive/scrive-apidocs), once
merged to `master` Travis CI will auto-deploy those changes.
