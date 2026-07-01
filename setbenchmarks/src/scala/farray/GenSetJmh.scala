package farray

import bleep.*
import bleep.internal.FileUtils
import org.openjdk.jmh.generators.bytecode.JmhBytecodeGenerator

import java.nio.file.Files

/** The FSet benchmarks' JMH bytecode generator — a parallel of [[GenJmh]] for the FArray suite, pointed at the `setbenchmarks` project's compiled classes so
  * the two suites generate into separate runner source trees.
  */
object GenSetJmh extends BleepCodegenScript("GenSetJmh") {
  override def run(started: Started, commands: Commands, targets: List[GenSetJmh.Target], args: List[String]): Unit = {
    val setBenchmarksProject = model.CrossProjectName(model.ProjectName("setbenchmarks"), None)

    targets.foreach { target =>
      val classesDir = started.projectPaths(setBenchmarksProject).classes

      // rebuild everything
      FileUtils.deleteDirectory(target.sources)
      Files.createDirectories(target.sources)
      FileUtils.deleteDirectory(target.resources)
      Files.createDirectories(target.resources)

      JmhBytecodeGenerator.main(Array(classesDir.toString, target.sources.toString, target.resources.toString, "default"))
    }
  }
}
