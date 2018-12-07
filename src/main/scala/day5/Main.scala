package day5

import cats.data.Chain
import cats.effect._
import cats.implicits._

import fs2._
import fs2.io._

import java.nio.file.Paths

import scala.concurrent.ExecutionContext.global

object Main extends IOApp {
  def run(args: List[String]): IO[ExitCode] = {
    args(0) match {
      case "part1" => part1
      case "part2" => part2
      case _       => IO.pure(ExitCode.Error)
    }
  }

  def react(polymer: List[Char]): Chain[Char] = {
    polymer.foldLeft(Chain.empty[Char]) { (chain, char) =>
      chain.uncons match {
        case Some((head, tail)) =>
          // compare their decimal values, alpha chars are 32 apart from the other case
          if (Math.abs(head.compare(char)) == 32) {
            tail
          } else {
            chain.prepend(char)
          }
        case None =>
          chain.prepend(char)
      }
    }
  }

  def part1 = input.compile.toList
    .map(polymer => react(polymer))
    .flatTap(result => IO(println(s"Final result: ${result.length}")))
    .as(ExitCode.Success)

  def part2 = ('a' to 'z').toList
    .map { char =>
      input.compile.toList.map { polymer =>
        val filtered = polymer.filterNot { unit =>
          unit == char || unit == char.toUpper
        }

        react(filtered).length
      }
    }
    .sequence
    .flatTap(reacted => IO(println(reacted.min)))
    .as(ExitCode.Success)

  val input: Stream[IO, Char] =
    file.readAll[IO](Paths.get("src/main/scala/day5/input.txt"), global, 256)
      .map(_.toChar)
      .filter(_.isLetter)
}
