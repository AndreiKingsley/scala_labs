import java.nio.file.{Files, Path, Paths, StandardCopyOption}

import cats.data.{IndexedStateT, StateT}
import cats.{Applicative, Functor, Id, Monad}
import cats.syntax.all._
import cats.instances.list._

import scala.jdk.CollectionConverters._
import scala.language.higherKinds

trait MkDir[F[_], Dir] {
  def mkDir(dir: Dir, name: String): F[Dir]
}

trait MkFile[F[_], Dir, File] {
  def mkFile(dir: Dir, name: String): F[File]
}

trait Printer[F[_], File] {
  def printName(file: File): F[Unit]
}

trait FilesInDir[F[_], Dir, File] {
  def getFilesInDir(dir: Dir): F[List[File]]
}

trait FileNames[F[_], File] {
  def getFileNames(files: List[File]): F[List[String]]
}

trait MoveFile[F[_], Dir, File] {
  def moveFile(dir: Dir, file: File): F[File]
}

class Program[F[_], Dir, File](implicit
                               F: Monad[F],
                               mkDir: MkDir[F, Dir],
                               mkFile: MkFile[F, Dir, File],
                               printer: Printer[F, File],
                               filesInDir: FilesInDir[F, Dir, File],
                               fileNames: FileNames[F, File],
                               moveFile: MoveFile[F, Dir, File]) {
  def run(dir: Dir): F[Unit] = for {
    // 1
    testDir <- mkDir.mkDir(dir, "test_dir")
    // 2
    _ <- mkFile.mkFile(testDir, "foo")
    _ <- mkFile.mkFile(testDir, "bar")
    _ <- mkFile.mkFile(testDir, "baz")
    // 3
    files <- filesInDir.getFilesInDir(testDir)
    // 4
    _ <- files.traverse(fileName => printer.printName(fileName))
    // 5
    names <- fileNames.getFileNames(files)
    firstLetters = names.map(_.head.toString)
    newDirs <- firstLetters.traverse(mkDir.mkDir(testDir, _))
    // 6
    _ <- files.zip(newDirs).traverse(fnd => moveFile.moveFile(fnd._2, fnd._1))
  } yield ()
}

class RealFileSystem[F[_] : Applicative] extends MkDir[F, Path] with MkFile[F, Path, Path]
  with FilesInDir[F, Path, Path] with FileNames[F, Path] with MoveFile[F, Path, Path] {
  override def mkDir(dir: Path, name: String): F[Path] =
    Files.createDirectories(dir.resolve(name)).pure[F]

  override def mkFile(dir: Path, name: String): F[Path] =
    Files.createFile(dir.resolve(name)).pure[F]

  override def getFilesInDir(dir: Path): F[List[Path]] =
    Files.list(dir).filter(Files.isRegularFile(_)).iterator().asScala.toList.pure[F]

  override def getFileNames(files: List[Path]): F[List[String]] = {
    files.map(path => path.getFileName.toString).pure[F]
  }

  override def moveFile(dir: Path, file: Path): F[Path] =
    Files.move(file, dir.resolve(file.getFileName), StandardCopyOption.REPLACE_EXISTING).pure[F]
}

class ConsolePathPrinter[F[_] : Applicative] extends Printer[F, Path] {
  override def printName(file: Path): F[Unit] = println(file.getFileName).pure[F]
}

/*
case class FakeDir(name: String, dirs: List[FakeDir], files: List[FakeFile])

case class FakeFile(name: String)

case class FileRef(path: List[String])

trait ModifyFakeDir[F[_]] {
  def modifyDir(fileRef: FileRef)(f: FakeDir => FakeDir): F[Unit]
}

object ModifyFakeDir {
  def apply[F[_]](implicit m: ModifyFakeDir[F]): ModifyFakeDir[F] = m
}

class FakeFileSystem[F[_] : ModifyFakeDir : Functor] extends MkDir[F, FileRef] with MkFile[F, FileRef, FileRef] {
  override def mkDir(dir: FileRef, name: String): F[FileRef] = ModifyFakeDir[F].modifyDir(dir) { currentDir =>
    currentDir.copy(dirs = FakeDir(name, Nil, Nil) :: currentDir.dirs)
  }.map(_ => FileRef(dir.path :+ name))

  override def mkFile(dir: FileRef, name: String): F[FileRef] = ModifyFakeDir[F].modifyDir(dir) { currentDir =>
    currentDir.copy(files = FakeFile(name) :: currentDir.files)
  }.map(_ => FileRef(dir.path :+ name))
}

class StateTModify[F[_] : Applicative] extends ModifyFakeDir[StateT[F, FakeDir, *]] {
  private def modifyDir(path: List[String], dir: FakeDir, f: FakeDir => FakeDir): FakeDir = path match {
    case Nil => f(dir)
    case head :: tail => dir.copy(dirs = dir.dirs.map {
      case current if current.name == head => modifyDir(tail, current, f)
      case current => current
    })
  }

  override def modifyDir(fileRef: FileRef)(f: FakeDir => FakeDir): StateT[F, FakeDir, Unit] = StateT.modify[F, FakeDir] { root =>
    modifyDir(fileRef.path, root, f)
  }
}

class ConsoleFakePrinter[F[_] : Applicative] extends Printer[F, FileRef] {
  override def printName(file: FileRef): F[Unit] = println(file.path.mkString(".")).pure[F]
}
*/

object TypeClasses {
  def main(args: Array[String]): Unit = {
    implicit val fs: RealFileSystem[Id] = new RealFileSystem[Id]
    implicit val printer: ConsolePathPrinter[Id] = new ConsolePathPrinter[Id]
    val program = new Program[Id, Path, Path]

    program.run(Paths.get("."))
  }
}