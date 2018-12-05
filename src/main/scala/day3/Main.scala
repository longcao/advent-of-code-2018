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
  def run(args: List[String]): IO[ExitCode] = {
    args(0) match {
      case "part1" => part1
      //case "part2" => part2
      case _       => IO.pure(ExitCode.Error)
    }
  }

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

  def printMatrix(matrix: Array[Array[Int]]): IO[Unit] =
    IO(matrix.map(_.mkString(" ")).foreach(println))

  def countOverlaps(matrix: Array[Array[Int]]): Int =
    matrix.map(row => row.filter(_ >= 2).size).sum

  def addClaim(matrix: Array[Array[Int]], claim: Claim): Array[Array[Int]] = {
    val newMatrix = matrix.clone()

    for {
      row    <- claim.y until (claim.y + claim.height)
      column <- claim.x until (claim.x + claim.width)
    } {
      val currentCount = newMatrix(row)(column)
      newMatrix(row)(column) = currentCount + 1
    }

    newMatrix
  }

  def part1 = input.map(parseClaim)
    .collect { case ParseResult.Done(_, claim) => claim }
    .compile
    .fold (Array.ofDim[Int](1000, 1000)) { (matrix, claim) =>
      addClaim(matrix, claim)
    }
    .map(matrix => countOverlaps(matrix))
    .flatTap(overlaps => IO(println(overlaps)))
    .as(ExitCode.Success)
}
