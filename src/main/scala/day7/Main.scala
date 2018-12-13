package day7

import atto._, Atto._

import cats.data.Chain
import cats.effect._
import cats.implicits._

import fs2._
import fs2.io._

import java.nio.file.Paths

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.global

object Main extends IOApp {
  def run(args: List[String]): IO[ExitCode] = {
    args(0) match {
      case "part1" => part1
      case _       => IO.pure(ExitCode.Error)
    }
  }

  def topologicalSort(edgeList: List[(Char, Char)]) = {
    @tailrec def tsort(dependenciesMap: Map[Char, Set[Char]], acc: Chain[Char]): Chain[Char] = {
      val noDependencies = dependenciesMap.filter { case (_, v) => v.isEmpty }

      noDependencies.keys.toList.sorted.headOption match {
        case Some(next) =>
          // remove next from downstream dependencies and the dependency map altogether
          val newDependenciesMap = dependenciesMap
            .mapValues(dependencies => dependencies - next) - next

          tsort(newDependenciesMap, acc.append(next))
        case None =>
          acc
      }
    }

    // map of child -> parent nodes
    val dependenciesMap: Map[Char, Set[Char]] =
      edgeList.foldLeft(Map.empty[Char, Set[Char]]) { case (acc, (parent, child)) =>
        acc.updated(child, acc.getOrElse(child, Set.empty[Char]) + parent)
          .updated(parent, acc.getOrElse(parent, Set.empty[Char]))
      }

    tsort(dependenciesMap, Chain.empty[Char])
  }

  def part1 = {
    input
      .compile.toList
      .map(topologicalSort)
      .flatTap(result => IO(println(result.toList.mkString)))
      .as(ExitCode.Success)
  }

  val inputParser: Parser[(Char, Char)] =
    (string("Step "), letter, string(" must be finished before step "), letter)
      .mapN((_, from, _, to) => from -> to)

  val input: Stream[IO, (Char, Char)] =
    file.readAll[IO](Paths.get("src/main/scala/day7/input2.txt"), global, 256)
      .through(text.utf8Decode)
      .through(text.lines)
      .filter(_.nonEmpty)
      .map(inputParser.parse(_).done)
      .collect { case ParseResult.Done(_, pair) => pair }
}
