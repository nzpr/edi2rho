import sbt.Keys.libraryDependencies
import sbt.{CrossVersion, compilerPlugin, _}

object Dependencies {

  val catsVersion       = "2.7.0"
  val catsEffectVersion = "3.3.11"

  val catsKernel  = "org.typelevel" %% "cats-kernel" % catsVersion       //    .withSources()    .withJavadoc()
  val catsCore    = "org.typelevel" %% "cats-core"   % catsVersion       //).withSources().withJavadoc()
  val catsEffects = "org.typelevel" %% "cats-effect" % catsEffectVersion //    .withSources()    .withJavadoc()

  val fs2            = "co.fs2"                       %% "fs2-io"               % "3.2.7"
  val jacksondatabin = "com.fasterxml.jackson.core"   % "jackson-databind"      % "2.4.0"
  val jacksonmodule  = "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.13.2"

  val fs2rabbit      = "dev.profunktor"       %% "fs2-rabbit"       % "5.0.0"
  val scalapbRuntime = "com.thesamet.scalapb" %% "scalapb-runtime"  % scalapb.compiler.Version.scalapbVersion % "protobuf"
  val grpcNetty      = "io.grpc"              % "grpc-netty-shaded" % scalapb.compiler.Version.grpcJavaVersion

  val scalacheck = "org.scalacheck"         %% "scalacheck"              % "1.15.2"
  val compat     = "org.scala-lang.modules" %% "scala-collection-compat" % "2.6.0"
  val magnolia   = "com.propensive"         %% "magnolia"                % "0.17.0"
  val paradise = compilerPlugin(
    "org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full
  )
  val kindProjector = compilerPlugin(
    "org.typelevel" %% "kind-projector" % "0.13.2" cross CrossVersion.full
  )

  val pureconfig = "com.github.pureconfig" %% "pureconfig"    % "0.17.1"
  val bcprov     = "org.bouncycastle"      % "bcprov-jdk15on" % "1.68"
  val bcpkix     = "org.bouncycastle"      % "bcpkix-jdk15on" % "1.68"
  val jaxb       = "javax.xml.bind"        % "jaxb-api"       % "2.3.1"

  val kalium    = "com.github.rchain" % "kalium"         % "0.8.1"
  val secp256k1 = "com.github.rchain" % "secp256k1-java" % "0.1"

  val lightningj = ("org.lightningj" % "lightningj" % "0.5.2-Beta").intransitive() //we only use the lib for one util class (org.lightningj.util.ZBase32) that has no dependencies

  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"
  val deps: Seq[ModuleID] =
    Seq(
      catsKernel,
      catsCore,
      catsEffects,
      fs2,
      jacksondatabin,
      jacksonmodule,
      fs2rabbit,
      scalapbRuntime,
      grpcNetty,
      scalacheck,
      compat,
      magnolia,
      paradise,
      kindProjector,
      pureconfig,
      bcprov,
      bcpkix,
      jaxb,
      kalium,
      secp256k1,
      lightningj
    )
}
