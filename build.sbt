
name := """ttaqm"""

version := "1.0.3"

lazy val root = (project in file(".")).enablePlugins(PlayScala, LauncherJarPlugin)

scalaVersion := "2.11.11"

libraryDependencies ++= Seq(
  jdbc,  
  cache,
  ws,
  "org.scalikejdbc" %% "scalikejdbc"                  % "2.5.2",
  "org.scalikejdbc" %% "scalikejdbc-config"           % "2.5.2",
  "org.scalikejdbc" %% "scalikejdbc-play-initializer" % "2.5.1",
  "com.github.nscala-time" %% "nscala-time" % "2.16.0"
)
// https://mvnrepository.com/artifact/org.apache.poi/poi-ooxml
libraryDependencies += "org.apache.poi" % "poi-ooxml" % "5.0.0"

// https://mvnrepository.com/artifact/com.microsoft.sqlserver/mssql-jdbc
libraryDependencies += "com.microsoft.sqlserver" % "mssql-jdbc" % "9.4.0.jre8"

// https://mvnrepository.com/artifact/com.itextpdf/itextpdf
libraryDependencies += "com.itextpdf" % "itextpdf" % "5.5.13.2"

// https://mvnrepository.com/artifact/com.itextpdf.tool/xmlworker
libraryDependencies += "com.itextpdf.tool" % "xmlworker" % "5.5.13.2"

// https://mvnrepository.com/artifact/org.jsoup/jsoup
libraryDependencies += "org.jsoup" % "jsoup" % "1.14.2"

mappings in Universal ++=
  (baseDirectory.value / "report_template" * "*" get) map
    (x => x -> ("report_template/" + x.getName))

mappings in Universal ++=
  (baseDirectory.value / "export/hour" * "*" get) map
    (x => x -> ("export/hour/" + x.getName))

mappings in Universal ++=
  (baseDirectory.value / "export/minute" * "*" get) map
    (x => x -> ("export/minute/" + x.getName))
    
mappings in Universal ++=
  (baseDirectory.value / "export/calibration" * "*" get) map
    (x => x -> ("export/calibration/" + x.getName))

mappings in Universal ++= 
 List(file("public/css/bootstrap.min.css") -> "public/css/bootstrap.min.css",
 	file("public/css/aqm.css") -> "public/css/aqm.css"
 )



PlayKeys.fileWatchService := play.runsupport.FileWatchService.sbt(2000)

scalacOptions += "-feature"

routesGenerator := InjectedRoutesGenerator

fork in run := false
