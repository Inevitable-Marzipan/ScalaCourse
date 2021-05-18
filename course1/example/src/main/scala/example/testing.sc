def inc(x: Int): Int = x + 1
def app(f: Int => Int, x: Int): Int = f(x)


def iterVal(f: Int => Int, t: Int, v: Int): Int = {
  def loop(acc: Int, c: Int): Int = {
    if (c == 0) acc
    else loop(f(acc), c - 1)
  }
  loop(v, t)
}

println(iterVal(inc, 2, 8))

def repeatFunc(f: Int => Int, t: Int): Int => Int = {
  def iterFunc(acc: Int => Int, v: Int): Int => Int = {
    if (v == 0) x => x
    else if (v == 1) acc
    else iterFunc(acc andThen f, v - 1)
  }
  iterFunc(f, t)
}

println(repeatFunc(inc, 2)(11))
def incTwo(x: Int): Int = repeatFunc(inc, 2)(x)
println(incTwo(11))

println(inc(7))
println(app(inc, 9))