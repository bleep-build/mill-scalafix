package bleep.plugin.scalafix

import bleep.{constants, model}
import coursier.core.Authentication

object CoursierUtils {
  def toApiRepositories(repos: List[model.Repository], bleepConfig: model.BleepConfig): List[coursierapi.Repository] =
    (repos ++ constants.DefaultRepos).map(r => toApiRepository(r, bleepConfig))

  private def staticAuth(bleepConfig: model.BleepConfig, uri: java.net.URI): Option[Authentication] =
    bleepConfig.authentications.flatMap(_.configs.get(uri).collect { case model.AuthEntry.Static(a) => a })

  def toApiRepository(repo: model.Repository, bleepConfig: model.BleepConfig): coursierapi.Repository =
    repo match {
      case mvn: model.Repository.Maven =>
        val credentialsOpt = staticAuth(bleepConfig, mvn.uri).map(toApiCredentials)
        coursierapi.MavenRepository
          .of(mvn.uri.toString)
          .withCredentials(credentialsOpt.orNull)
      case ivy: model.Repository.Ivy =>
        val credentialsOpt = staticAuth(bleepConfig, ivy.uri).map(toApiCredentials)
        coursierapi.IvyRepository
          .of(ivy.uri.toString)
          .withCredentials(credentialsOpt.orNull)
      case other =>
        throw new Exception(s"Unrecognized repository: " + other)
    }

  def toApiCredentials(auth: Authentication): coursierapi.Credentials =
    coursierapi.Credentials.of(auth.user, auth.passwordOpt.getOrElse(""))

  def toCoordinates(dep: model.Dep): String =
    dep.repr
}
