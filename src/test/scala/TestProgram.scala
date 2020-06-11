import java.nio.file.{Files, Path, Paths}
import java.util.Comparator

import cats.Id
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec

class TestProgram extends AnyFlatSpec with Matchers {
  val root: Path = Paths.get("./tmp")
  Files.createDirectory(root)

  implicit val fs: RealFileSystem[Id] = new RealFileSystem[Id]
  implicit val printer: ConsolePathPrinter[Id] = new ConsolePathPrinter[Id]
  val program = new Program[Id, Path, Path]

  program.run(root)

  val dir: Path = root.resolve("test_dir")

  Files.exists(dir) shouldBe true

  val fDir: Path = dir.resolve("f")
  val bDir: Path = dir.resolve("b")

  Files.exists(fDir) && Files.isDirectory(fDir) shouldBe true
  Files.exists(fDir) && Files.isDirectory(bDir) shouldBe true

  val foo: Path = fDir.resolve("foo")
  val bar: Path = bDir.resolve("bar")
  val baz: Path = bDir.resolve("baz")

  Files.exists(foo) && Files.isRegularFile(foo) shouldBe true
  Files.exists(bar) && Files.isRegularFile(bar) shouldBe true
  Files.exists(baz) && Files.isRegularFile(baz) shouldBe true

}