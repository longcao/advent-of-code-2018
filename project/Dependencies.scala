import sbt._

object Version {
  val fs2 = "1.0.0"
}

object Dependencies {
  val atto        = "org.tpolecat"      %% "atto-core"    % "0.6.4"
  val fs2Core     = "co.fs2"            %% "fs2-core"     % Version.fs2
  val fs2Io       = "co.fs2"            %% "fs2-io"       % Version.fs2
}
