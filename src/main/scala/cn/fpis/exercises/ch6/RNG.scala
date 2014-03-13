package cn.fpis.exercises.ch6

import scala.annotation.tailrec

trait RNG {

  type Rand[+A] = RNG => (A, RNG)

  def nextInt: (Int, RNG)
  //def positiveInt(rng: RNG): (Int, RNG)
  //def double(rng: RNG): (Double, RNG)
  def intDouble(rng: RNG): ((Int, Double), RNG)
  def doubleInt(rng: RNG): ((Double, Int), RNG)
  def double3(rng: RNG): ((Double, Double, Double), RNG)
  //  def ints(count: Int)(rng: RNG): (List[Int], RNG)
  def ints2(count: Int)(rng: RNG): (List[Int], RNG)

  def intDoubleWithMap(rng: RNG): ((Int, Double), RNG)
  def doubleIntWithMap(rng: RNG): ((Double, Int), RNG)
  def intsWithSequence(count: Int)(rng: RNG): (List[Int], RNG)

  def unit[A](a: A): Rand[A] = rng => (a, rng)
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {

    rng =>
      {
        val (v, n) = s(rng)
        (f(v), n)
      }

  }

  //Acts as a combinator of two RNG calls
  def map2[A, B, C](a: Rand[A], b: Rand[B])(f: (A, B) => C): Rand[C] = {

    rng =>
      {
        val (va, na) = a(rng)
        val (vb, nb) = b(na)

        (f(va, vb), nb)
      }
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {

    fs.foldRight(unit(List.empty[A])) { (v, acc) =>

      map2(v, acc)(_ :: _)

    }

  }

  def flapMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {

    rng =>
      {
        val (va, na) = f(rng)
        g(va)(na)
      }

  }

  def positiveMax(n: Int): Rand[Int]

}

object RNG {
  def simple(seed: Long): RNG = new RNG {

    def nextInt = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.	    
    }

    def positiveIntWithFlatMap(rng: RNG) {

      rng.flapMap(_.nextInt) {
        v => rng => (if (v < 0) -(v + 1) else v, rng)
      }

    }

    def doubleWithMap(rng: RNG) = {
      map(positiveInt)(_ / (Int.MaxValue.toDouble + 1))(rng)
    }

    def intDouble(rng: RNG) = {
      val (i, n) = rng.nextInt
      val (d, n2) = double(n)

      ((i, d), n2)
    }

    def intDoubleWithMap(rng: RNG) = {
      map2(positiveInt, double)((_, _))(rng)
    }

    def doubleInt(rng: RNG) = {
      val (d, n) = double(rng)
      val (i, n2) = n.nextInt

      ((d, i), n2)
    }

    def doubleIntWithMap(rng: RNG) = {
      map2(double, positiveInt)((_, _))(rng)
    }

    def double3(rng: RNG) = {

      val (i1, n1) = double(rng)
      val (i2, n2) = double(n1)
      val (i3, n3) = double(n2)

      ((i1, i2, i3), n3)

    }

    def ints2(count: Int)(rng: RNG) = {

      @tailrec
      def loop(c: Int, s: RNG, ints: List[Int]): (List[Int], RNG) = {

        if (c == 0) (ints, s)
        else {
          val (i, n) = s.nextInt
          loop(c - 1, n, i :: ints)
        }

      }

      loop(count, rng, Nil)

    }

    def intsWithSequence(count: Int)(rng: RNG) = {

      sequence(List.fill(count)(positiveInt(_)))(rng)

    }

    def positiveMax(n: Int) = {

      map(positiveInt)(x => if (x > n) n else x)

    }

  }

  def boolean(g: RNG) = {
    val (v, s) = g.nextInt
    (if (v > 0) true else false, s)
  }

  def ints(count: Int)(rng: RNG) = {

    (0 until count).foldLeft((List.empty[Int], rng)) { (acc, v) =>

      acc match {
        case (lst, s) => {
          val (i, n) = s.nextInt
          (i :: lst, n)
        }
      }
    }
  }

  def positiveInt(rng: RNG) = {
    val (v, n) = rng.nextInt

    //If it's negative then sum 1 in case we're at Min.MaxValue and then switch sign because -(Int.MaxValue) is not Int.MinValue
    (if (v < 0) -(v + 1) else v, n)
  }

  def double(rng: RNG) = {
    val (i, n) = rng.nextInt
    (i / (Int.MaxValue.toDouble + 1), n)
  }

}