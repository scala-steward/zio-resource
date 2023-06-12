import Dependencies.*
import Libraries.*

Global / onChangedBuildSource := ReloadOnSourceChanges

Test / fork := true
IntegrationTest / fork := true

inThisBuild(
  Seq(
    resolvers ++= Resolver.sonatypeOssRepos("releases"),
    organization := "io.funkode",
    scalaVersion := "3.3.0",
    versionScheme := Some("early-semver"),
    semanticdbEnabled := true,
    semanticdbVersion := scalafixSemanticdb.revision,
    scalafixDependencies += "com.github.liancheng" %% "organize-imports" % "0.6.0",
    sonatypeCredentialHost := "s01.oss.sonatype.org",
    sonatypeRepository := "https://s01.oss.sonatype.org/service/local",
    startYear := Some(2023),
    licenses += ("MIT", new URL("https://opensource.org/licenses/MIT")),
    homepage := Some(url("https://github.com/carlos-verdes/zio-resource")),
    scmInfo := Some(
      ScmInfo(
        url("https://github.com/carlos-verdes/zio-resource"),
        "git@github.com:carlos-verdes/zio-resource.git"
      )
    ),
    developers := List(
      Developer(
        "carlos-verdes",
        "Carlos Verdes",
        "cverdes@gmail.com",
        url("https://github.com/carlos-verdes")
      ),
      Developer(
        "OlegEfrem",
        "Oleg Efrem",
        "oleg.efrem@gmail.com",
        url("https://github.com/OlegEfrem")
      )
    )
  )
)

ThisBuild / scalacOptions ++=
  Seq(
    "-deprecation",
    // "-explain",
    "-feature",
    "-language:implicitConversions",
    "-unchecked",
    "-Xfatal-warnings",
//    "-Yexplicit-nulls", // experimental (I've seen it cause issues with circe)
    "-Ykind-projector",
//    "-Ysafe-init", // experimental (I've seen it cause issues with circe)
    "-Yretain-trees",
    "-Wunused:all",
    "Wvalue-discard"
  ) ++ Seq("-rewrite", "-indent") ++ Seq("-source", "future-migration")

lazy val commonLibs = Seq(scalaUri, logBack, zioConfMagnolia, zioConfTypesafe)
lazy val zioLibs = Seq(zio, zioHttp, zioJson, zioConcurrent, zioConfMagnolia, zioConfTypesafe)
lazy val testLibs = Seq(zioTest, zioTestSbt, zioJGolden).map(_ % "it, test")

lazy val root =
  project
    .in(file("."))
    .configs(IntegrationTest)
    .settings(Defaults.itSettings)
    .settings(headerSettings(Test, IntegrationTest))
    .settings(
      Seq(
        name := "zio-resource",
        libraryDependencies ++= (commonLibs ++ testLibs ++ Seq(zioArangodb)),
        testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
      ),
      headerLicense := Some(HeaderLicense.MIT("2023", "Carlos Verdes", HeaderLicenseStyle.SpdxSyntax))
    )
    .enablePlugins(AutomateHeaderPlugin)

ThisBuild / coverageExcludedFiles := ".*Main.*;zio\\.json\\.*"

addCommandAlias("ll", "projects")
addCommandAlias("checkFmtAll", ";scalafmtSbtCheck;scalafmtCheckAll")
addCommandAlias("testAll", ";compile;test;stryker")
//addCommandAlias("sanity", ";compile;scalafmtAll;test;stryker")
addCommandAlias(
  "sanity",
  ";clean;coverage;compile;headerCreate;scalafixAll;scalafmtAll;test;it:test;coverageAggregate;coverageOff"
)
