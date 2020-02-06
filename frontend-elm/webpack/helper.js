
const generateVersionId =
  ( buildDate = 'build_date'
  , buildNumber = 'build_number'
  , buildVcs = 'build_vcs_number'
  ) =>
    {
      const versionString =
        `${buildDate}.${buildNumber}.${buildVcs}`

      const versionHex =
        Buffer.from(versionString).toString('hex')

      return versionHex
    }

const versionIdFromEnv = () =>
  generateVersionId
    ( process.env.BUILD_DATE
    , process.env.BUILD_NUMBER
    , process.env.BUILD_VCS_NUMBER
    )

module.exports =
  { generateVersionId
  , versionIdFromEnv
  }