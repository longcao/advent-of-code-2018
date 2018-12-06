package day4

import atto._
import Atto._

import cats.effect._
import cats.implicits._

import fs2._
import fs2.io._

import java.nio.file.Paths
import java.time.LocalDate

import scala.concurrent.ExecutionContext.global

sealed trait EventType
object EventType {
  case class  ShiftBegins(guardId: Int) extends EventType
  case object FallsAsleep               extends EventType
  case object WakesUp                   extends EventType

  def parser: Parser[EventType] = {
    (string("Guard #"), int, string(" begins shift")).mapN((_, guardId, _) => ShiftBegins(guardId)) |
    string("falls asleep").as(FallsAsleep: EventType) |
    string("wakes up")    .as(WakesUp: EventType)
  }
}

case class LogEntry(
  date      : LocalDate,
  minute    : Int,
  eventType : EventType)

object LogEntry {
  val localDate: Parser[LocalDate] =
    (int, char('-'), int, char('-'), int)
      .mapN((year, _, month, _, day) => LocalDate.of(year, month, day))

  def parser: Parser[LogEntry] =
    (char('['), localDate, spaceChar, int, char(':'), int, string("] "), EventType.parser)
      .mapN((_, date, _, _, _, minute, _, eventType) => LogEntry(date, minute, eventType))
}

case class GuardAsleep(
  guardId : Int,
  date    : LocalDate,
  minute  : Int)

object Main extends IOApp {
  def run(args: List[String]): IO[ExitCode] = {
    logEntries
      .map(splitShifts(_).toList)
      .map { shifts =>
        shifts.flatMap(shiftToAsleep)
      }
      .flatTap { minutesAsleep =>
        IO {
          val guardsSleeping: Map[Int, List[GuardAsleep]] =
            minutesAsleep.groupBy(_.guardId)

          val mostAsleepGuardId: Int =
            guardsSleeping
              .mapValues(_.length)
              .maxBy(_._2)._1

          val mostCommonMinute = guardsSleeping(mostAsleepGuardId)
            .groupBy(_.minute)
            .mapValues(_.length)
            .maxBy(_._2)._1

          println(s"Most asleep guard: #$mostAsleepGuardId")
          println(s"Most common minute #$mostAsleepGuardId is asleep: $mostCommonMinute")
          println(s"mostAsleepGuardId * mostCommonMinute: ${mostAsleepGuardId * mostCommonMinute}")
        }
      }
      .as(ExitCode.Success)
  }

  def splitShifts(entries: List[LogEntry]): Vector[List[LogEntry]] = {
    def _splitShifts(logEntries: List[LogEntry], acc: Vector[List[LogEntry]]): Vector[List[LogEntry]] = {
      if (logEntries.isEmpty) {
        acc
      } else {
        val shiftBegins = logEntries.head
        val (shiftEvents, restOfShifts) = logEntries.tail.span { entry =>
          entry.eventType match {
            case EventType.ShiftBegins(_) => false
            case _                        => true
          }
        }

        _splitShifts(restOfShifts, acc :+ (shiftBegins :: shiftEvents))
      }
    }

    _splitShifts(entries, Vector())
  }

  def shiftToAsleep(shift: List[LogEntry]): List[GuardAsleep] = {
    val shiftBegins = shift.head
    val shiftEvents = shift.tail
    val guardId = shiftBegins.eventType
      .asInstanceOf[EventType.ShiftBegins].guardId // big yikes

    shiftEvents.sliding(2, 2).flatMap { sleepWindow =>
      val fallsAsleep = sleepWindow(0)
      val wakesUp     = sleepWindow(1)

      for {
        minute <- fallsAsleep.minute until wakesUp.minute
      } yield (GuardAsleep(guardId, fallsAsleep.date, minute))
    }.toList
  }

  val input: Stream[IO, String] =
    file.readAll[IO](Paths.get("src/main/scala/day4/input.txt"), global, 256)
      .through(text.utf8Decode)
      .through(text.lines)
      .filter(_.nonEmpty)

  val logEntries: IO[List[LogEntry]] = input.compile.toList
    .map { rawStrings =>
      rawStrings
        .sorted
        .map(LogEntry.parser.parseOnly)
        .collect { case ParseResult.Done(_, logEntry) => logEntry }
    }
}
