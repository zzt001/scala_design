object generators {
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

  val integers = new Generator[Int] {
    def generate = scala.util.Random.nextInt()
  }

  val booleans = integers map (_ >= 0)
  val a = List
}