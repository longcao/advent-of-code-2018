package day3

import atto._, Atto._

import cats.effect._
import cats.implicits._

import fs2._
import fs2.io._

import java.nio.file.Paths

import scala.concurrent.ExecutionContext.global

case class Claim(
  id: Int,
  x: Int,
  y: Int,
  width: Int,
  height: Int)

object Main extends IOApp {
  val input: Stream[IO, String] =
    file.readAll[IO](Paths.get("src/main/scala/day3/input.txt"), global, 256)
      .through(text.utf8Decode)
      .through(text.lines)
      .filter(_.nonEmpty)

  def parseClaim(str: String): ParseResult[Claim] = {
    val parser: Parser[Claim] = for {
      _       <- char('#')
      id      <- int
      _       <- string(" @ ")
      x       <- int
      _       <- char(',')
      y       <- int
      _       <- string(": ")
      width   <- int
      _       <- char('x')
      height  <- int
    } yield Claim(id = id, x = x, y = y, width = width, height = height)

    parser.parseOnly(str)
  }

  def run(args: List[String]): IO[ExitCode] = {
    input.map(parseClaim)
      .collect { case ParseResult.Done(_, claim) => claim }
      .compile.drain

    val matrix = Array.ofDim[Int](2000, 2000)

    IO.pure(ExitCode.Success)
  }
}
