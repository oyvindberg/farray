package farray

import bleep.*
import bleep.internal.FileUtils
import org.openjdk.jmh.generators.bytecode.JmhBytecodeGenerator

import java.nio.file.Files

object GenJmh extends BleepCodegenScript("GenJmh") {
  override def run(started: Started, commands: Commands, targets: List[GenJmh.Target], args: List[String]): Unit = {
    val benchmarksProject = model.CrossProjectName(model.ProjectName("benchmarks"), None)

    targets.foreach { target =>

      val classesDir = started.projectPaths(benchmarksProject).classes

      // rebuild everything
      FileUtils.deleteDirectory(target.sources)
      Files.createDirectories(target.sources)
      FileUtils.deleteDirectory(target.resources)
      Files.createDirectories(target.resources)

      JmhBytecodeGenerator.main(Array(classesDir.toString, target.sources.toString, target.resources.toString, "default"))
    }

  }
}
