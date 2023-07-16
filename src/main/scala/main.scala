import mte._
import scala.language.postfixOps
import scala.annotation.unused

@main
def main(): Unit = {
  춘잣! {
    아니 자기가 "n" 이라는사람인데 개입 을 했대
  } 케바바바밥줘 {
    ("n" 조이고 뭉탱탱 수) ("i") {
      ("n"수) ("j") {
        스키비야 게이 "@"
      } 게이 "\n"
    }
  } 케바바바밥줘 {
    ("n"수) ("i") {
      ("n" 조이고 뭉탱뭉 수) ("j") {
        스키비야 게이 "@"
      } 게이 "\n"
    }
  }
}

@unused
def 입력받기(): Unit = {
  춘잣! {
    아니 자기가 "함수" 라는사람인데 {
      아~! "변수" 는 ("변수" 배 뭉탱뭉) 이 참 좋구나~!
    } 를 했대
  } 케바바바밥줘 {
    "함수"야 개입 먹어라??
  }
}

def 팩토리얼(): Unit = {
  춘잣! {
    아~! ("팩토리얼", "x")는 {
      (유링게슝한?) ("x" 돈 탱 원에??) {
        "팩토리얼"아 ("x" 코 뭉) 먹어라?? 조이고 "x"
      } 안유링게슝 {
        뭉
      }
    } 가 참 좋구나~!
  } 케바바바밥줘 {
    (11수) ("영양제") {
      "팩토리얼"아 "영양제" 먹어라?? 게이 "%s\n"
    }
  }
}

def 구구단(): Unit = {
  춘잣! {
    (11수) ("i") {
      (11수) ("j") {
        "i" 조이고 "j" 게이 "%s\t"
      } 게이 "\n"
    }
  }
}

def test1(): Unit = {
  춘잣! {
    정품 맞어 {
      주제넘은? {
        뭉 아 뭉탱 먹어라??
      }
    }
  }
}

class TestValidator(var n: Int = 0) {
  def validate(f: () => Unit): Unit =
    f()
    println("test %d validated".format(n))
    n += 1
}