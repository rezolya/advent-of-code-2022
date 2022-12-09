import scala.io.Source

val day = 6

val filename = s"input/input$day.txt"
val bufferedSource = Source.fromResource(filename)
val inputIterator: Iterator[Char] = bufferedSource.getLines().flatMap(_.iterator)

//val inputIterator: Iterator[Char] =
//  "mjqjpqmgbljsphdztnvjfqwrcgsmlb".iterator
//  "bvwbjplbgvbhsrlpgdmjqwftvncz".iterator
//  "nppdvjthqldpwncqszvftbrmjlhg".iterator
//  "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg".iterator
//  "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw".iterator

//val inputIterator: Iterator[Char] =
//  "mjqjpqmgbljsphdztnvjfqwrcgsmlb".iterator // first marker after character 19
//  "bvwbjplbgvbhsrlpgdmjqwftvncz".iterator //first marker after character 23
//  "nppdvjthqldpwncqszvftbrmjlhg".iterator //first marker after character 23
//  "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg".iterator //first marker after character 29
//  "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw".iterator //first marker after character 26

inputIterator.sliding(14, 1).zipWithIndex.collectFirst {
  case (window, index) if window.distinct.size == window.size => index + window.size
}

bufferedSource.close()

