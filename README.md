# Решение задач курса на SCALA

#### 4.1 Методы
Реализация вычисления чисел Фибоначчи

    def fibs(num: Int): Int = {
      if (num <= 0) {
        0
      } else if (num == 1) {
        1
      } else {
        fibs(num - 1) + fibs(num - 2)
      }
    }
    
#### 5.2 Pattern matching
На вход программе подается имя пользователя, email и список его коммитов, возможно пустой.
Считанные построчно данные лежат в списке `input`. После имени мог стоять как пробел, так и перенос строки, каждый коммит был в отдельной строчке. Другими словами, первые два элемента списка это либо имя и электронная почта, либо имя с почтой через пробел и первый коммит. Используя pattern-matching и регулярные выражения напечатайте через пробел имя пользователя и домен электронной почты (все, что стоит после собаки). Для некорректных данных напачатайте "invalid".

    import scala.util.matching.Regex

    val pattern: Regex = "([a-zA-Z]+ \\w+@\\w+.\\w+)".r
    val isName: Regex = "([a-zA-Z]+)".r
    val isEmail: Regex = "(\\w+@\\w+.\\w+)".r

    val input1 = input.slice(0,2)

    val result = input match {
      case List(isName(match1),isEmail(match2),_*) => s"$match1" +" "+ s"$match2".split("@")(1)
      case List(pattern(match1),_*)  => s"$match1".split(" ")(0)+" "+s"$match1".split(" ")(1).split("@")(1)
      case _ => "invalid"
    }
    println(result)
    
#### 5.3 Partial functions
В магазине красок проводится акция: на банки краски объемом от 5 до 10 литров скидка 5%, на банки больше 10 литров - 10%.
Одна банка представлена кейс классом `Jar(name: String, value: Int, price: Double)`, где `name` - артикул, `value` - объем, `price` - цена в рублях.
Напишите частичную функцию `discount`, которая при подстановке в метод `collect` списка банок, превратит его в список строк. Каждая строка должна состоять из артикула и размера скидки в рублях, записанных через пробел. В список должны входить только товары с ненулевой скидкой.

    val percent = (x:Double, y:Int) => x * y / 100
    val string = (a:String, c:Double) => s"$a $c"

    def discount: PartialFunction[Jar, String] = {
      case x:Jar if x.value >= 5 && x.value <= 10 => string(x.name, percent(x.price,5))
      case x:Jar if x.value > 10 => string(x.name, percent(x.price,10))
    }

    jars.collect(discount)
    
#### 5.6 Either
Правильной называется дробь, у которой модуль числителя  меньше модуля знаменателя. Будем представлять дробь парой (числитель, знаменатель).
Реализуйте операцию деления `divide(p: (Int, Int))(q: (Int, Int)): Either[String, (Int, Int)]`, возвращающую результат деления `p` на `q` или текст ошибки. Проверьте корректность входных данных, если входные дроби неправильные, верните ошибку `Left("Invalid input")`. Если делитель нулевой, верните `Left("Zero divisor")`. Если в результате получилась неправильная дробь, ошибка `Left("Improper result")`.
Сократите результат до простой дроби.

    def chekR(p: (Int, Int))(q: (Int, Int)): Option[((Int,Int),(Int,Int))] =
      if (p._1 >= p._2 | q._1 >= q._2) None else Some((p,q))

    def chekZ(p: (Int, Int))(q: (Int, Int)): Option[((Int,Int),(Int,Int))] =
      if (p._2 == 0 | q._1 == 0 | q._2 == 0) None else Some((p,q))

    def divide(p: (Int, Int))(q: (Int, Int)): Either[String, (Int, Int)] =
      chekZ(p)(q) match {
        case None => Left("Zero divisor")
        case Some(a) => chekR(a._1)(a._2) match {
          case None => Left("Invalid input")
          case Some(b) => if (b._1._1*b._2._2 >= b._1._2*b._2._1) Left("Improper result") else {
            val x = b._1._1*b._2._2
            val y = b._1._2*b._2._1

            val res = ((x / BigInt(x).gcd(y)).toInt,(y / BigInt(x).gcd(y)).toInt)

            Right(res)}
        }
      }

#### 6.1 Коллекции
Некоторые генетические алгоритмы для генерации новых хромосом из старых используют приём под названием кроссинговер.
Будем представлять хромосому с генами `[xxxxx]`   в виде списка `List('x', 'x', 'x', 'x', 'x')` . Тогда суть приёма заключается в следующем:
- Берутся две хромосомы одинаковой длины, например `[xxxxx]` и `[yyyyy]`. Списки для них будут выглядеть так:
`List('x', 'x', 'x', 'x', 'x')`
`List('y', 'y', 'y', 'y', 'y')`
- Выбираются так называемые `точки кроссинговера`. В нашем случае это некоторые индексы в диапазоне [1, длина списка генов хромосомы]. Пусть выбраны индексы 1 и 3.
- Для  каждого индекса, по возрастанию, хромосомы обмениваются своими частями, стоящими после этого индекса.

      object Main {
        def main(args: Array[String]) {
          val points: List[Int] = Lesson.points
          var chr1: List[Char] = Lesson.chr1
          var chr2: List[Char] = Lesson.chr2

          for (i <- points) {
            val res = Iterable(chr1, chr2)
              .map{x => x.splitAt(i)}
              .reduce((x, y) => (x._1 ++ y._2, y._1 ++ x._2))
            chr1 = res._1; chr2 = res._2
          }

          println(chr1.mkString(""))
          println(chr2.mkString(""))
        }
      }

#### 6.2 Коллекции
В данной задаче необходимо реализовать алгоритм нахождения k-ой порядковой статистики, матожидание времени работы которого составляет O(n). Для этого реализуйте метод kOrder.
На вход в первой строке подаётся k - номер порядковой статистики, которую надо найти. Во второй строке - элементы набора.

    import scala.annotation.tailrec
    import scala.io.StdIn.readLine

    object Main {
      def main(args: Array[String]) {

        val k = readLine().toInt
        val array = readLine().split(" ").map(_.toInt)
        val low = 0
        val hi = array.length - 1

        def swap(a: Array[Int], pos1: Int, pos2: Int): Unit = {
          val stash = a(pos1)
          a(pos1) = a(pos2)
          a(pos2) = stash
        }

        def partition(subArray: Array[Int], low: Int, hi: Int): Int = {
          val pivot = hi
          var i = low - 1
          for (j <- low until hi) {
            if (subArray(j) < subArray(pivot)){
              i+=1
              swap(subArray, i, j)
            }
          }
          val result = i+1
          swap(subArray, result, pivot)
          result
        }

        @tailrec
        def kOrder(list: Array[Int], k: Int, low: Int, hi: Int): Int = {
          val x = partition(list, low, hi)
          if (x+1 == k) {println(list(x)); list(x)}
          else if (x < k) kOrder(list, k, x+1, hi)
          else kOrder(list, k, low, x-1)
        }

        kOrder(array, k, low, hi)
      }}

#### 6.3 Коллекции
Многие играли в игру  Морской бой (https://goo.gl/SMWykY). В игре используется квадратное поле клеток, на которое перед началом игры каждый из игроков расставляет свои корабли.
Обращаем внимание на то, что все коллекции иммутабельны.
Входные данные
На вход в первой строке вам даётся число кораблей, которые игрок хочет добавить на поле.
Далее мы задаем положения кораблей: первая строка - название корабля (одним словом) и число клеток, занимаемых им, последующие строки - координаты его клеток, по одной клетке (соответственно, по две координаты) на строку.
Задание
Корабли добавляются на поле field (квадратное поле размера 10 на 10 клеток, задано заранее, булево значение во всех его клетках проинициализировано как false, координаты нумеруются с нуля) в такой же очередности, как они подаются на вход.
Необходимо проверять условия:
Корабль не имеет изгибов (ширина корабля всегда единица).
Длина корабля не больше четырёх.
Корабли могут примыкать к границам поля, но не могут касаться друг друга, даже углами своих клеток.
Выведите имена кораблей, которые получилось расставить на поле.

    import scala.annotation.tailrec
    import scala.io.StdIn.readLine

    object Main {
      def main(args: Array[String]): Unit = {

        import Naval.{Point, Field, Ship, Fleet, Game}
        import Lesson.field

        def validateShip(ship: Ship): Boolean = {
          if (ship.length == 1) true
          else{
            val side_a = ship
              .map(_._1)
              .sliding(2,1)
              .forall(x => x(0) == x(1))
            val side_b = ship
              .map(_._2)
              .sliding(2,1)
              .forall(x => x(0) == x(1))

            if ((ship.length <= 4) && (side_a | side_b)) true
            else false}}

        def validatePosition(ship: Ship, field: Field): Boolean = {
          var area = List[(Int,Int)]()
          val a = ship

          val side_a = if (ship.length == 1) true
                       else ship.map(_._1).sliding(2,1).forall(x => x(0) == x(1))

          if (side_a){
            area = area ++ List((a(0)._1,a(0)._2-1),
              (a(0)._1-1,a(0)._2-1),
              (a(0)._1+1,a(0)._2-1))

            area = area ++ List((a.last._1,a.last._2+1),
              (a.last._1-1,a.last._2+1),
              (a.last._1+1,a.last._2+1))

            for (i <- 0 until a.length) {
              area = area ++ List((a(i)._1-1,a(i)._2),
                (a(i)._1+1,a(i)._2))}}

          else {
            area = area ++ List((a(0)._1-1,a(0)._2),
              (a(0)._1-1,a(0)._2-1),
              (a(0)._1-1,a(0)._2+1))

            area = area ++ List((a.last._1+1,a.last._2),
              (a.last._1+1,a.last._2-1),
              (a.last._1+1,a.last._2+1))

            for (i <- 0 until a.length) {
              area = area ++ List((a(i)._1,a(i)._2-1),
                (a(i)._1,a(i)._2+1))}}

          val isPossible = area
            .filter(x => (x._1 != -1)&&(x._2 != -1)&&(x._1 != 10)&&(x._2 != 10))
            .map(x => field(x._1)(x._2))
            .forall(_ == false)
          isPossible}

        def enrichFleet(fleet: Fleet, name: String, ship: Ship): Fleet = {
          var fleet_r = fleet
          fleet_r += (name -> ship)
          println(name);fleet_r}

        def markUsedCells(field: Field, ship: Ship): Field = {
          var field_r = field
          for (i <- 0 until ship.length)
            field_r = field_r.updated(ship(i)._1,field_r(ship(i)._1).updated(ship(i)._2,true))
          field_r}

        def readShip: (String, Ship) = {
          val ship = readLine()
          val name = ship.split(" ")(0)
          val count = ship.split(" ")(1).toInt
          val coordinate = Stream.continually(readLine()).take(count).toList
            .collect{case x => x.split(" ")}
            .flatten
            .grouped(2)
            .toList
            .map(x => (x(0).toInt,x(1).toInt))
          (name, coordinate)}

        val countShips = readLine().toInt
        val (name,ship) = readShip
        val fleet = Map.empty[String, Ship]

        @tailrec
        def tryAddShip(game: Game, name: String, ship: Ship, countShips: Int): Game = {
          if (countShips == 0) game
          else if (validateShip(ship) && validatePosition(ship, game._1)) {
            val fleet_r = enrichFleet(game._2, name, ship)
            val field_r = markUsedCells(game._1, ship)
            if (countShips == 1){
              tryAddShip((field_r, fleet_r), name, ship, countShips - 1)}
            else {
              val (name_r, ship_r) = readShip
              tryAddShip((field_r, fleet_r), name_r, ship_r, countShips - 1)}}
          else {
            val (name_r, ship_r) = readShip
            tryAddShip(game, name_r, ship_r, countShips - 1)
          }
        }


        tryAddShip((field,fleet), name, ship, countShips)
      }
    }
    
#### 6.4 Коллекции
`List` - одна из любимых коллекций скалистов. Её иммутабельность играет на руку при написании параллельных программ, а её API позволяет эффективно работать с элементами, лежащими в начале коллекции. С задачей добавления одиночных элементов в начало она справится хорошо (за константное время), так как реализована на основе односвязного списка, но вот ассимптотическая оценка операции добавления таких элементов в конец вырастет до O(n) , где n - длина списка.
И тут на сцену выходит структура данных под названием `Difference List`.
Рассмотрим две операции над списками: операцию `.prepend` добавления элементов в начало списка и операцию `.append` добавления элементов в конец списка. Каждую такую операцию можно рассматривать как некоторую функцию `List[A] => List[A]`, где `A` - тип элементов списка, тогда некоторая цепочка таких операций - это композиция таких функций. Именно за счёт такого приёма DiffList-ы позволяют избавиться от дорогостоящего добавления в конец, заменяя его на добавление в начало.

    final class DiffListImpl[A](listFunc: List[A] => List[A])
        extends DiffList[A](listFunc) {
      def prepend(s: List[A]): DiffListImpl[A] =
        new DiffListImpl[A](listFunc andThen (s ++ _))

      def append(s: List[A]): DiffListImpl[A] =
        new DiffListImpl[A](listFunc andThen (_ ++ s))

      def result: List[A] = listFunc(Nil)
    }
    
#### 7.1 Классы
Ваша задача - спроектировать и реализовать класс официанта. Официант умеет принимать блюдо в заказ и в конце повторять, что было заказано. Также он вежлив и представляется.
Требования:
- имя класса Waiter
- метод для заказа блюда giveMe, принимает название блюда
- метод complete, возвращает список того, что было заказано
- при своем появлении официант здоровается с гостем
- необходимо, чтобы была возможна следующая запись
`val positions = waiter.giveMe("борщ").giveMe("хлеб").complete()`

      class Waiter(name: String, order: List[String] = List()) {

        if (name != null) println(s"My name $name")

        def giveMe(string: String): Waiter = {
          new Waiter(null,order ++ List(string))
        }

        def complete(): List[String] = {
          order
        }
      }
      
#### 7.3 Объекты
Дан список координат в трёхмерном пространстве. Вам нужно написать класс Point, который будет описывать точку в трёхмерном пространстве и объект-компаньон со следующими функциями:
- apply - фабрика, принимает три координаты и возвращает экземпляр типа Point
- average - принимает List[Point] и вычисляет усреднённую точку между всеми координатами, либо точку с началом осей координат, если её невозможно рассчитать
- show - принимает Point и превращает её в строку, состоящую из координат разделённых через пробел

Для каждой строки будет вызвана функция apply, затем для всех точек будет вызвана функция average. На выход будет передан результат функции show, примененный к усреднённой точке.

        case class Point(x:Double, y:Double, z:Double)

        object Point{
            def apply(x:Double=0.0, y:Double=0.0 ,z:Double=0.0): Point = {
                val res1 = new Point(x,y,z)
                res1
            }

            def average(points: List[Point]): Point = {
                if (points.isEmpty) Point(0.0,0.0,0.0)
                else{
                    val len = points.length
                    val res_sum = points.reduce((a,b) => Point(a.x + b.x, a.y + b.y, a.z + b.z))
                    val res = List(res_sum.x, res_sum.y, res_sum.z).map(_/len)
                    Point(res(0),res(1),res(2))}
            }

            def show(result:Point) = {
                val (x, y, z) = (result.x, result.y, result.z)
                s"$x $y $z"
            }
        }

#### 7.4 Объекты
Ваша задача - реализовать метод unapply у объекта FacedString﻿. Считать из потока ввода строку, сделать паттерн матчинг с ней, который определит, могла ли она быть результатом некоторого вызова apply. Если получилось выделить строку, от которой она была сконструирована, вывести эту строку на экран, если нет - вывести "Could not recognize string".

    import scala.io.StdIn.readLine
    import scala.util.matching.Regex

    object FacedString {
      def apply(input: String) = s"*_*$input*_*"

      def unapply(arg: String): Option[String] = {
        val isArg: Regex = "(\\*\\_\\*)(.*)(\\*\\_\\*)".r
        val result = arg match {
          case isArg(_,x,_) => Some(x)
          case _ => None
        }
        println(result.getOrElse("Could not recognize string"))
        result
      }
    }

    object Main {
    def main(args: Array[String]) {
        val input = readLine()
        FacedString.unapply(input)
    }}
