import BNFC._
import Dependencies.deps
import sbt.enablePlugins

import java.lang.Runtime.getRuntime

lazy val projectSettings = Seq(
  name := "rabbit2rho",
  organization := "coop.rchain",
  scalaVersion := "2.12.15",
  version := "0.1.0-SNAPSHOT",
  resolvers ++=
    (Resolver.mavenLocal +: Resolver.sonatypeOssRepos("releases")) ++
      Resolver.sonatypeOssRepos("snapshots") :+
      ("jitpack" at "https://jitpack.io"),
  Test / testOptions += Tests.Argument("-oD"), //output test durations
  javacOptions ++= Seq("-source", "11", "-target", "11"),
  Test / fork := true,
  Test / parallelExecution := false,
  Test / testForkedParallel := false,
  assembly / assemblyMergeStrategy := {
    // For some reason, all artifacts from 'io.netty' group contain this file with different contents.
    // Discarding it as it's not needed.
    case path if path.endsWith("io.netty.versions.properties") => MergeStrategy.discard
    // The scala compiler includes native bindings for jansi under the same path jansi does.
    // This should pick the ones provided by jansi.
    case path if path.startsWith("META-INF/native/") && path.contains("jansi") => MergeStrategy.last
    case path                                                                  => MergeStrategy.defaultMergeStrategy(path)
  }
) ++
  // skip api doc generation if SKIP_DOC env variable is defined
  Seq(sys.env.get("SKIP_DOC")).flatMap { _ =>
    Seq(
      Compile / packageDoc / publishArtifact := false,
      Compile / doc / sources := Seq.empty
    )
  }

lazy val commonSettings = projectSettings

lazy val edi2rho = (project in file("."))
  .settings(commonSettings: _*)
  .settings(bnfcSettings: _*)
  .enablePlugins(Fs2Grpc)
  .settings(
    name := "rabbit2rho",
    scalacOptions ++= Seq(
      "-language:existentials",
      "-language:higherKinds",
      "-Yno-adapted-args",
      "-Xfatal-warnings",
      "-Xlint:_,-missing-interpolator" // disable "possible missing interpolator" warning
    ),
    Compile / packageDoc / publishArtifact := false,
    Compile / doc / sources := Seq.empty,
    libraryDependencies ++= deps,
    scalapbCodeGeneratorOptions += CodeGeneratorOption.FlatPackage,
    scalacOptions ++= Seq(
      "-Xfuture",
      "-Ypartial-unification",
      "-Ywarn-dead-code",
      "-Ywarn-numeric-widen",
      "-deprecation",
      "-encoding",
      "UTF-8",
      "-feature",
      "-language:_",
      "-unchecked",
      "-Xfatal-warnings",
      //With > 16: [error] invalid setting for -Ybackend-parallelism must be between 1 and 16
      //https://github.com/scala/scala/blob/v2.12.6/src/compiler/scala/tools/nsc/settings/ScalaSettings.scala#L240
      "-Ybackend-parallelism",
      getRuntime.availableProcessors().min(16).toString,
      "-Xlint:-unused,-adapted-args,-inaccessible,_",
      "-Ywarn-unused:implicits",
      "-Ywarn-macros:after",
      "-Ywarn-unused:locals",
      "-Ywarn-unused:patvars",
      "-Ywarn-unused:privates"
    ),
    Test / javaOptions ++= Seq("-Xss240k", "-XX:MaxJavaStackTraceDepth=10000", "-Xmx128m"),
    assembly / assemblyMergeStrategy := {
      // For some reason, all artifacts from 'io.netty' group contain this file with different contents.
      // Discarding it as it's not needed.
      case path if path.endsWith("io.netty.versions.properties") => MergeStrategy.discard
      case path if path.endsWith("module-info.class")            => MergeStrategy.discard
      // The scala compiler includes native bindings for jansi under the same path jansi does.
      // This should pick the ones provided by jansi.
      case path if path.startsWith("META-INF/native/") && path.contains("jansi") =>
        MergeStrategy.last
      case path => MergeStrategy.defaultMergeStrategy(path)
    },
    assembly / mainClass := Some("coop.rchain.rabbit2rho.App"),
    assembly / assemblyJarName := "rabbit2rho.jar"
  )
