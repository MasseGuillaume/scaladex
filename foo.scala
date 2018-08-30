import ch.epfl.scala.index.model.{Project, Release}
import ch.epfl.scala.index.model.release.ScalaDependency
import ch.epfl.scala.index.data.{DataPaths, LocalPomRepository}
import ch.epfl.scala.index.data.project.ProjectConvert
import ch.epfl.scala.index.data.github.GithubDownload
import ch.epfl.scala.index.data.maven.PomsReader

import scala.collection.mutable.{Map => MMap, Set => MSet}
import scala.util.Success

import java.nio.file.{Files, Paths}
import java.nio.charset.StandardCharsets

import System.{lineSeparator => nl}

val paths = DataPaths(List(
  "../scaladex-contrib",  
  "../scaladex-index",
  "../scaladex-credentials"
))
// val paths = DataPaths(Nil)

val projectConverter = new ProjectConvert(paths)

val centralPoms = PomsReader(LocalPomRepository.MavenCentral, paths).load()

val projectsAndReleases = projectConverter(centralPoms.collect {
  case Success(pomAndMeta) => pomAndMeta
})

val releases = projectsAndReleases.flatMap(_._2) 
val relases212 = releases.filter(r => r.scalaVersion == Some("2.12")) 
// ========

def filterDependency(dependency: ScalaDependency): Boolean = {
  import dependency.reference._
  dependency.scope == Some("compile") &&
  ! (
    (organization == "scala" && repository == "scala") ||
    (organization == "scala-js" && repository == "scala-js") ||
    (organization == "scala-native" && repository == "scala-native")
  )
}

def show(reference: Release.Reference): String = {
  import reference._
  s"$organization/$repository/$artifact"
}

val dependencies = 
  for {
    release <- relases212
    dependency <- release.scalaDependencies
    if filterDependency(dependency)
  } yield (show(release.reference), show(dependency.reference))


import $ivy.`io.verizon.quiver::core:7.1.0-SNAPSHOT`
import quiver._ 

def create[T](graph: Seq[(T, T)]): Graph[T, Unit, Unit] = {
  val nodes = graph.flatMap{ case (a, b) => Seq(a, b)}.map(n => LNode(n, ()))
  val edges = graph.map { case (a, b) => LEdge(a, b, ())}
  empty[T, Unit, Unit].addNodes(nodes).addEdges(edges)
}

import quiver._
val graph = create(dependencies).reverse

val out = graph.nodes.map(n => (n, graph.reachable(n).size)).sortBy(_._2).map{ 
  case (k, v) => k.padTo(80, " ").mkString("") + v.toString
}.reverse

Files.write(Paths.get("out.txt"), out.mkString(nl).getBytes(StandardCharsets.UTF_8))