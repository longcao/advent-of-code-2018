package day5

import cats.effect._
import cats.data.Chain
import cats.implicits._

import fs2._
import fs2.io._

import java.nio.file.Paths

import scala.concurrent.ExecutionContext.global

object Main extends IOApp {
  def run(args: List[String]): IO[ExitCode] = {
    args(0) match {
      case "part1" => part1
      case _       => IO.pure(ExitCode.Error)
    }
  }

  def part1 = input.compile
    .fold(Chain.empty[Char]) { (chain, char) =>
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
    .flatTap(result => IO(println(s"Final result: ${result.length}")))
    .as(ExitCode.Success)

  val input: Stream[IO, Char] =
    file.readAll[IO](Paths.get("src/main/scala/day5/input.txt"), global, 256)
      .map(_.toChar)
      .filter(_.isLetter)
}
