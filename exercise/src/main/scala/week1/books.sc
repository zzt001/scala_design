object books {
  case class Book(title: String, authors: List[String])

  val books = List(
    Book(
      title = "BookA",
      authors = List("Abelson, Harald", "Sussman, Gerald J.")),
    Book(
      title = "BookB",
      authors = List("Bird, Richard", "Wadler, Phil")),
    Book(
      title = "Effective Java",
      authors = List("Bloch, Joshua")),
    Book(
      title = "Effective Java 2",
      authors = List("Bloch, Joshua")),
    Book(
      title = "Java Puzzlers",
      authors = List("Bloch, Joshua", "Gafter, Neal")),
    Book(
      title = "Programming in Scala",
      authors = List("Odersky, Martin", "Spoon, Lex", "Venners, Bill")))

  for (b <- books; a <- b.authors if a startsWith "Bloch, ")
    yield b.title

  books.flatMap(b =>
    b.authors.withFilter(a => a startsWith "Bird") map (_ => b.title))


  trait Generator[+T] {
    self =>

    def generate: T

    def map[U](f: T => U): Generator[U] = {
      new Generator[U] {
        def generate: U = f(self.generate)
      }
    }

    def flatMap[U](f: T => Generator[U]): Generator[U] = {
      f(self.generate)
    }
  }


  def pairs[T, U](t: Generator[T], u: Generator[U]) = t flatMap {
    x => u map {y => (x, y)}
  }
}