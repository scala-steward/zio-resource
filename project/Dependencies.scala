import sbt._

object Dependencies {

  object Versions {

    val tapirV = "1.1.4"
    val sttpClientV = "3.8.13"

    val zioV = "2.0.10"
    val zioConfigV = "3.0.2"
    val zioHttpV = "0.0.5"
    val zioLoggingV = "2.1.0"
    val zioJsonV = "0.5.0"
    val zioConfMagnoliaV = "3.0.7"
    val zioConfTypesafeV = "3.0.7"
    val zioCryptoV = "0.0.1"
    val zioPreludeV = "1.0.0-RC18"
    val zioSchemaV = "0.4.10"

    val zioArangodbV = "0.2.2"

    val logBackV = "1.4.6"
    val scalaUriV = "4.0.3"
    val scodecV = "2.2.0"
    val testContainersV = "0.40.12"

    val tinkV = "1.7.0"
    val jwtZioJsonV = "9.1.2"
    val web3jV = "5.0.0"

    val shapelessV = "3.3.0"
  }

  object Libraries {

    import Versions._

    val zio = "dev.zio" %% "zio" % zioV
    val zioConcurrent = "dev.zio" %% "zio-concurrent" % zioV
    val zioConfMagnolia = "dev.zio" %% "zio-config-magnolia" % zioConfMagnoliaV
    val zioConfTypesafe = "dev.zio" %% "zio-config-typesafe" % zioConfTypesafeV
    val zioCrypto = "dev.zio" %% "zio-crypto" % zioCryptoV
    val zioHttp = "dev.zio" %% "zio-http" % zioHttpV
    val zioJson = "dev.zio" %% "zio-json" % zioJsonV
    val zioJGolden = "dev.zio" %% "zio-json-golden" % zioJsonV
    val zioPrelude = "dev.zio" %% "zio-prelude" % zioPreludeV
    val zioStreams = "dev.zio" %% "zio-streams" % zioV
    val zioTest = "dev.zio" %% "zio-test" % zioV
    val zioTestSbt = "dev.zio" %% "zio-test-sbt" % zioV
    val zioSchema = "dev.zio" %% "zio-schema" % zioSchemaV

    val zioArangodb = "io.funkode" %% "zio-arangodb-http" % zioArangodbV

    val logBack = "ch.qos.logback" % "logback-classic" % logBackV
    val jansi = "org.fusesource.jansi" % "jansi" % "2.4.0"
    val testContainers = "com.dimafeng" %% "testcontainers-scala-core" % testContainersV
    val scalaUri = "io.lemonlabs" %% "scala-uri" % scalaUriV
    val scodecBits = "org.scodec" %% "scodec-bits" % scodecV
    val scodecCore = "org.scodec" %% "scodec-core" % scodecV

    val tink = "com.google.crypto.tink" % "tink" % tinkV
    val jwtZioJson = "com.github.jwt-scala" %% "jwt-zio-json" % jwtZioJsonV
    val web3j = "org.web3j" % "core" % web3jV

    val shapeless = ("org.typelevel" %% "shapeless3-deriving" % shapelessV)
  }
}
