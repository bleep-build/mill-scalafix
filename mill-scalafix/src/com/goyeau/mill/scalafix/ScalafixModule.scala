package bleep.plugin.scalafix

import bleep.{Started, fixedClasspath, model}
import ryddig.Logger
import scalafix.interfaces.Scalafix
import scalafix.interfaces.ScalafixError.*

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.{RichOption, RichOptional}

class ScalafixPlugin(
    started: Started,
    scalafixConfig: Option[Path] = None,
    scalafixIvyDeps: List[model.Dep] = Nil
) {

  def fix(projects: List[model.CrossProjectName], args: List[String]): Unit = {

    val repos: List[coursierapi.Repository] =
      CoursierUtils.toApiRepositories(started.build.resolvers.values, started.config)

    lazy val scalafix: Scalafix = Scalafix.fetchAndClassloadInstance("2.13", repos.asJava)

    projects.foreach { crossName =>
      started.build.explodedProjects(crossName).scala.flatMap(_.version) match {
        case Some(scalaVersion) =>
          started.logger.info(s"Running Scalafix for ${crossName.value}")

          val sources: Seq[Path] =
            ScalafixModule.filesToFix(started.projectPaths(crossName).sourcesDirs.all.toList)

          val project = started.bloopProject(crossName)

          ScalafixModule.fixAction(
            scalafix = scalafix,
            log = started.logger,
            repos = repos,
            sources = sources,
            classpath = fixedClasspath(project),
            scalaVersion = scalaVersion.scalaVersion,
            scalacOptions = project.scala.toList.flatMap(_.options),
            scalafixIvyDeps = scalafixIvyDeps,
            scalafixConfig = scalafixConfig,
            args = args,
            wd = started.buildPaths.buildDir
          )
        case None => started.logger.error(s"Cannot run Scalafix on project with no Scala version: ${crossName.value}")
      }
    }
  }
}

object ScalafixModule {
  def fixAction(
      scalafix: Scalafix,
      log: Logger,
      repos: List[coursierapi.Repository],
      sources: Seq[Path],
      classpath: Seq[Path],
      scalaVersion: String,
      scalacOptions: Seq[String],
      scalafixIvyDeps: List[model.Dep],
      scalafixConfig: Option[Path],
      args: Seq[String],
      wd: Path
  ): Either[String, Unit] =
    if (sources.nonEmpty) {
      val configured = scalafix
        .newArguments()
        .withParsedArguments(args.asJava)
        .withWorkingDirectory(wd)
        .withConfig(scalafixConfig.toJava)
        .withClasspath(classpath.asJava)
        .withScalaVersion(scalaVersion)
        .withScalacOptions(scalacOptions.asJava)
        .withPaths(sources.asJava)
        .withToolClasspath(
          Seq.empty.asJava,
          scalafixIvyDeps.map(CoursierUtils.toCoordinates).iterator.toSeq.asJava,
          repos.asJava
        )

      log.info(s"Rewriting and linting ${sources.size} Scala sources against rules: ${configured.rulesThatWillRun.asScala.mkString(", ")}")
      val errors = configured.run()
      if (errors.isEmpty) Right(())
      else {
        val errorMessages = errors.map {
          case ParseError => "A source file failed to be parsed"
          case CommandLineError =>
            configured.validate().toScala.fold("A command-line argument was parsed incorrectly")(_.getMessage)
          case MissingSemanticdbError =>
            "A semantic rewrite was run on a source file that has no associated META-INF/semanticdb/.../*.semanticdb"
          case StaleSemanticdbError =>
            """The source file contents on disk have changed since the last compilation with the SemanticDB compiler plugin.
              |To resolve this error re-compile the project and re-run Scalafix""".stripMargin
          case TestError =>
            "A Scalafix test error was reported. Run `fix` without `--check` or `--diff` to fix the error"
          case LinterError  => "A Scalafix linter error was reported"
          case NoFilesError => "No files were provided to Scalafix so nothing happened"
          case _            => "Something unexpected happened running Scalafix"
        }
        Left(errorMessages.mkString("\n"))
      }
    } else Right(())

  def filesToFix(sources: Seq[Path]): Seq[Path] =
    sources.filter(p => p.toFile.exists() && Files.isDirectory(p)).flatMap { path =>
      Files.walk(path).iterator().asScala.filter(p => Files.isRegularFile(p) && p.toString.endsWith(".scala"))
    }
}
