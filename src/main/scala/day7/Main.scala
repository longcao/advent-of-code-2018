package day7

import atto._, Atto._

import cats.data.Chain
import cats.effect._
import cats.implicits._

import fs2._
import fs2.io._

import java.nio.file.Paths

import scala.collection.immutable.SortedSet
import scala.concurrent.ExecutionContext.global

object Main extends IOApp {
  def run(args: List[String]): IO[ExitCode] = {
    args(0) match {
      case "part1" => part1
      case _       => IO.pure(ExitCode.Error)
    }
  }

  def topologicalSort(edgeList: List[(Char, Char)]) = {
    def tsort(dependenciesMap: Map[Char, SortedSet[Char]], acc: Chain[Char]): Chain[Char] = {
      val (noDependencies, hasDependencies) =
        dependenciesMap.partition { case (_, v) => v.isEmpty }

      if (noDependencies.isEmpty) {
        acc
      } else {
        val next = noDependencies.keys.toList.sorted

        val newDependenciesMap =
          hasDependencies.mapValues { dependencies =>
            dependencies -- next.to[SortedSet]
          }

        val newAcc = acc.concat(Chain.fromSeq(next))

        tsort(newDependenciesMap, newAcc)
      }
    }

    // map of child -> parent nodes
    val dependenciesMap: Map[Char, SortedSet[Char]] =
      edgeList.foldLeft(Map.empty[Char, SortedSet[Char]]) { case (acc, (parent, child)) =>
        acc.updated(child, acc.getOrElse(child, SortedSet.empty[Char]) + parent)
          .updated(parent, acc.getOrElse(parent, SortedSet.empty[Char]))
      }

    tsort(dependenciesMap, Chain.empty[Char])
  }

  def part1 = {
    input
      .compile.toList
      .map(topologicalSort)
      .flatTap { result =>
        IO {
          println(result)
        }
      }
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
