package farray

import bleep.*
import bleep.internal.FileUtils
import org.openjdk.jmh.generators.bytecode.JmhBytecodeGenerator

import java.io.File
import java.nio.file.{Files, Path}
import scala.collection.Seq

object GenJmh extends BleepCodegenScript("GenJmh") {
  def run(started: Started, commands: Commands, benchmarksRunnerProject: model.CrossProjectName, args: List[String]): Unit = {
    val benchmarksProject = model.CrossProjectName(model.ProjectName("benchmarks"), None)

    val paths = started.projectPaths(benchmarksRunnerProject)
    val sourceDir = paths.sourcesDirs.generated
    val resourceDir = paths.resourcesDirs.generated
    val classesDir = started.projectPaths(benchmarksProject).classes

    // rebuild everything
    FileUtils.deleteDirectory(sourceDir)
    Files.createDirectories(sourceDir)
    FileUtils.deleteDirectory(resourceDir)
    Files.createDirectories(resourceDir)

    JmhBytecodeGenerator.main(Array(classesDir.toString, sourceDir.toString, resourceDir.toString, "default"))
  }
}
